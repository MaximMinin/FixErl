%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created : 27.05.2012
%%% @end
%%% -------------------------------------------------------------------
-module(tcp_acceptor).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock, ref, id}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Id, LSock) ->
    gen_server:start_link(?MODULE, {Id, LSock}, []).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init({Id, LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {ok, #state{sock=LSock, ref=Ref, id =Id}};
        Error -> {stop, {cannot_accept, Error}}
    end.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({inet_async, LSock, Ref, {ok, Sock}},
            State = #state{sock=LSock, ref=Ref, id= Id}) ->
    {ok, Mod} = inet_db:lookup_socket(LSock),
    inet_db:register_socket(Sock, Mod),
    {ok, {Address, Port}} = inet:sockname(LSock),
    {ok, {PeerAddress, PeerPort}} = inet:peername(Sock),
    lager:info("accepted TCP connection on ~s:~p from ~s:~p~n",
                          [inet_parse:ntoa(Address), Port,
                           inet_parse:ntoa(PeerAddress), PeerPort]),
    {ok, Child} = tcp_client_sup:start_child(Id, []),
    ok = gen_tcp:controlling_process(Sock, Child),
    Child ! {go, Sock},
    case prim_inet:async_accept(LSock, -1) of
        {ok, NRef} -> {noreply, State#state{ref=NRef}};
        Error -> {stop, {cannot_accept, Error}, none}
    end;
handle_info({inet_async, LSock, Ref, {error, closed}},
            State=#state{sock=LSock, ref=Ref}) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
