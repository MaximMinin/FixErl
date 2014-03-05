%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created : 27.05.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fix_worker).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("fixerl.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/8, newMessage/2, getMessages/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
        handle_info/2, terminate/2, code_change/3]).

-record(state, {callback, pid, fixSender, count = 0, 
                senderCompID, targetCompID, role, session_id,
                mnesia_tables_name, fix_version}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Pid, FixSender, FixVersion, SenderCompID,
            TargetCompID, Callback, Role, SessionId) ->
    gen_server:start_link(?MODULE, [Pid, FixSender, FixVersion, SenderCompID, 
                                    TargetCompID, Callback, 
                                    Role, SessionId], []).

newMessage(Pid, Message)->
    gen_server:cast(Pid, {message, Message}).

getMessages(Pid, From, To) ->
    gen_server:call(Pid, {getMessages, From, To}).

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
init([Pid, FixSender, FixVersion, SenderCompID, TargetCompID, 
      Callback, Role, SessionId]) ->
    lager:md([{session, SessionId}]),
    State = #state{fix_version = FixVersion,
                   pid = Pid, fixSender = FixSender,
                   session_id = SessionId, senderCompID = SenderCompID,
                   targetCompID = TargetCompID, callback = Callback,
                   role = Role,
                   mnesia_tables_name = 
                       fixerl_mnesia_utils:get_tables_name(SessionId)},
    {ok, State}.

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
handle_call({getMessages, _From, _To}, _From, State) ->
    ToReturn = ok, %%TODO
    {reply, ToReturn, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({message, Msg}, #state{pid = Pid, fixSender = FixSender, 
                                   fix_version = FixVersion,
                                   senderCompID = SenderCompID, 
                                   targetCompID = TargetCompID,
                                   count = C, callback = {M,F}, 
                                   role = Role, 
                                   session_id = Id,
                                   mnesia_tables_name = [Tin,Tout]} = State) ->
    mnesia:transaction(fun() -> 
        mnesia:write({Tin, C+1 , Msg}) end),
    case erlang:element(1, Msg) of
        %%TODO sessionhandling
        logon -> lager:debug("LOGON: ~p SenderCompID: ~p TargetCompID: ~p", 
                            [Msg, SenderCompID, TargetCompID]),
            case fix_utils:check_logon(FixVersion,
                                       Msg, 
                                       SenderCompID, 
                                       TargetCompID) of
                ok -> 
                    case Role of
                        acceptor ->
                            fix_gateway:send(FixSender,
                                    fix_utils:get_logon(FixVersion,
                                                        SenderCompID,
                                                       TargetCompID));
                        initiator -> ok
                    end,
                    Pid ! fix_starting;
                nok -> 
                    fix_gateway:send(FixSender,
                        fix_utils:get_logout(FixVersion,
                                            SenderCompID,
                                            TargetCompID)),
                    erlang:exit(false_logon)
            end;
        testRequest -> lager:debug("TESTREQUEST: ~p~n", [Msg]),
                  fix_gateway:send(FixSender,
                                        fix_utils:get_heartbeat(FixVersion,
                                                                Msg));
        heartbeat -> lager:debug("HEARTBEAT: ~p~n", [Msg]);
        logout -> lager:debug("LOGOUT: ~p~n", [Msg]), 
                  fix_gateway:send(FixSender,
                      fix_utils:get_logout(FixVersion,
                                           SenderCompID,
                                           TargetCompID)),
                  erlang:exit(fix_session_close);
        resendRequest -> lager:debug("RESENDREQUEST: ~p~n", [Msg]),
            lists:map(fun(Num) -> 
                [{Tout, Num, ResendMessage}] = 
                 mnesia:dirty_read(({Tout, Num})),
                 fix_gateway:resend(FixSender, ResendMessage) end, 
                 fix_utils:get_numbers(FixVersion, Msg));
        _Else -> lager:notice("BUSINESS MESSAGE RECEIVED: ~p~n", [Msg]),
                 M:F(Id, Msg)
    end,
    {noreply, State#state{count = C+1}}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
