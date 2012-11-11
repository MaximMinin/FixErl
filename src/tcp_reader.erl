%%% -------------------------------------------------------------------
%%% Author  : Maxim Minin
%%% Description :
%%%
%%% Created : 28.06.2012
%%% -------------------------------------------------------------------
-module(tcp_reader).

-include("fixerl.hrl").

-export([start_link/2, start_link/1, info/1, info/2]).

-export([system_continue/3, system_terminate/4, system_code_change/4]).

-export([init/3, init/2, mainloop/3]).


-import(gen_tcp).
-import(fprof).
-import(inet).
-import(prim_inet).

-define(HANDSHAKE_TIMEOUT, 10).
-define(NORMAL_TIMEOUT, 3).
-define(CLOSING_TIMEOUT, 1).
-define(FRAME_MIN_SIZE, 1).
-define(HEARTBEAT_TIMEOUT, 30).

-record(state, {sock, connection, callback, recv_ref, connection_state, worker, writer, session_par}).

-record(connection, {timeout_sec, frame_max}).

-define(INFO_KEYS,
        [pid, address, port, peer_address, peer_port,
         recv_oct, recv_cnt, send_oct, send_cnt, send_pend,
         state, timeout, frame_max]).

start_link(Session) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [self(),Session])}.

start_link(Session, []) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [self(),Session])};
start_link(Session, Sock) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [self(), Sock, Session])}.

init(Parent,Session) ->
    Deb = sys:debug_options([]),
    receive
        {go, Sock} -> 
                     start_connection(Parent, Deb, Sock, Session)
    end.

init(Parent, Sock, Session) ->
    Deb = sys:debug_options([]),
    start_connection(Parent, Deb, Sock, Session).

system_continue(Parent, Deb, State) ->
    ?MODULE:mainloop(Parent, Deb, State).

system_terminate(Reason, _Parent, _Deb, _State) ->
    exit(Reason).

system_code_change(Misc, _Module, _OldVsn, _Extra) ->
    {ok, Misc}.

info(Pid) ->
    gen_server:call(Pid, info).

info(Pid, Items) ->
    case gen_server:call(Pid, {info, Items}) of
        {ok, Res}      -> Res;
        {error, Error} -> throw(Error)
    end.

inet_op(F) -> throw_on_error(inet_error, F).

throw_on_error(E, Thunk) ->
    case Thunk() of
        {error, Reason} -> throw({E, Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.

peername(Sock) ->   
    try
        {Address, Port} = inet_op(fun () -> inet:peername(Sock) end),
        AddressS = inet_parse:ntoa(Address),
        {AddressS, Port}
    catch
        Ex ->lager:error("error on TCP connection ~p:~p~n",
                               [self(), Ex]),
              exit(normal)
    end.

start_connection(Parent, Deb, ClientSock, Session) ->
    process_flag(trap_exit, true),
    {PeerAddressS, PeerPort} = peername(ClientSock),
    try 
        lager:info("starting TCP connection ~p from ~s:~p~n",
                        [self(), PeerAddressS, PeerPort]),
        erlang:send_after(?HANDSHAKE_TIMEOUT * 1000, self(),
                          handshake_timeout),
        {ok, WriterPid} = fix_gateway:start_link(ClientSock, 
                                                 Session#session_parameter.fix_version, 
                                                 Session#session_parameter.senderCompId, 
                                                 Session#session_parameter.targetCompId,
                                                 Session#session_parameter.id
                                                ),
        case Session#session_parameter.role of
            initiator ->
                        fix_gateway:send(WriterPid, fix_utils:get_logon(Session#session_parameter.senderCompId,
                                                        Session#session_parameter.targetCompId));
            _Else -> ok
        end,
        mainloop(Parent, Deb, switch_callback(
                                #state{
                                       session_par = Session,
                                       sock = ClientSock,
                                       writer = WriterPid,
                                       connection = #connection{
                                       timeout_sec = ?HANDSHAKE_TIMEOUT,
                                       frame_max = ?FRAME_MIN_SIZE
                                    },
                                    callback = uninitialized_callback,
                                    recv_ref = none,
                                    connection_state = pre_init},
                                handshake, 0))
    catch
        Ex -> lager:error("error on TCP connection ~p from ~s:~p~n~p~n",
                               [self(), PeerAddressS, PeerPort, Ex])
    after
        lager:info("closing TCP connection ~p from ~s:~p~n",
                        [self(), PeerAddressS, PeerPort])
    end,
    done.

mainloop(Parent, Deb, State = #state{sock= Sock, recv_ref = Ref}) ->
    receive
        {inet_async, Sock, Ref, {ok, Data}} ->
            {ok, {_Address, _Port}} = inet:sockname(Sock),
            {State1, Callback1, Length1} =
            handle_input_fix(State#state.callback, Data, State#state{recv_ref = none}),
            mainloop(Parent, Deb,
                     switch_callback(State1, Callback1, Length1));
        {inet_async, Sock, Ref, {error, closed}} ->
            if State#state.connection_state =:= closed ->
                    State;
               true ->
                    exit(connection_closed_abruptly)
            end;
        {inet_async, Sock, Ref, {error, Reason}} ->
            throw({inet_error, Reason});
        {'EXIT', Parent, Reason} ->
            if State#state.connection_state =:= running ->
                    %%TODO
                   ok;
               true -> ok
            end,
            exit(Reason);
        {'EXIT', _Pid, E = {writer, send_failed, _Error}} ->
            throw(E);
        {'EXIT', _Pid, Reason} ->
             throw(Reason);
         terminate_connection ->
            State;
        handshake_timeout ->
            if State#state.connection_state =:= running orelse
               State#state.connection_state =:= closing orelse
               State#state.connection_state =:= closed ->
                    mainloop(Parent, Deb, State);
               true ->
                    throw({handshake_timeout, State#state.callback})
            end;
        fix_starting -> 
             C = case  State#state.callback of
                 handshake -> fix;
                 fix -> fix;
                 fix_started ->
                     fix;
                 fix_starting ->
                     fix_started
             end,
             mainloop(Parent, Deb,
             State#state{callback =  C});
        timeout ->
            throw({timeout, State#state.connection_state});
        {'$gen_call', _From, info} ->
            %%TODO gen_server:reply(From, infos(?INFO_KEYS, State)),
            mainloop(Parent, Deb, State);
        {'$gen_call', _From, {info, _Items}} ->
            %%TODO gen_server:reply(From, try {ok, infos(Items, State)}
                   %%                catch Error -> {error, Error}
                   %%              end),
            mainloop(Parent, Deb, State);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From,
                                  Parent, ?MODULE, Deb, State);
        Other ->
            %% internal error -> something worth dying for
            exit({unexpected_message, Other})
    end.

switch_callback(OldState, NewCallback, Length) ->
    Ref = inet_op(fun () -> prim_inet:async_recv(
                              OldState#state.sock, Length, -1) end),
    OldState#state{callback = NewCallback,
                recv_ref = Ref}.

handle_input_fix(handshake, Data,
             State = #state{session_par = Session, sock = Sock, connection = Connection, writer = WriterPid}) ->
    {ok, FixPid} = fix_worker:start_link(self(), WriterPid, 
                                         Session#session_parameter.senderCompId, 
                                         Session#session_parameter.targetCompId,
                                         Session#session_parameter.callbackModule,
                                         Session#session_parameter.role),
    {ok, Splitter} = fix_splitter:start_link(FixPid, Session#session_parameter.fix_version), 
    fix_splitter:newRowData(Splitter, Data),
    fix_heartbeat:start_heartbeat(Sock, WriterPid, Session#session_parameter.heartbeatInterval),
    {State#state{worker = Splitter,
              connection = Connection#connection{timeout_sec = ?NORMAL_TIMEOUT},
              connection_state = running},
             fix_starting, 0};
handle_input_fix(fix_starting, Data, State= #state{worker = Splitter}) ->
    fix_splitter:newRowData(Splitter, Data),
    {State, fix_starting, 0};
handle_input_fix(fix_started, Data, State = #state{sock = _Sock, worker = Splitter}) ->
    fix_splitter:newRowData(Splitter, Data),
    {State, fix, 0};
handle_input_fix(fix, Data, State= #state{worker = Splitter}) ->
    fix_splitter:newRowData(Splitter, Data),
    {State, fix, 0}.
