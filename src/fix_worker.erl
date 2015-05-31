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
-export([start_link/3, newMessage/2, getMessages/3, 
         get_session_parameter/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
        handle_info/2, terminate/2, code_change/3]).

-record(state, {callback, pid, fixSender, count = 0, 
                senderCompID, targetCompID, role, session_id,
                mnesia_tables_name, fix_version, mode,
                session_params, sync_flag = false, diff, diff_count}).

-define(LOG(Id, FormatString, Args),
    lager:info([{session, Id}],
               FormatString, Args)).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Pid, FixSender, Session) ->
    Name = list_to_atom(lists:concat([?MODULE,
                                      Session#session_parameter.id])),
    gen_server:start_link({local, Name}, ?MODULE, [Pid, FixSender, 
                                    Session], []).

newMessage(Pid, Message)->
    gen_server:cast(Pid, {message, Message}).

getMessages(Pid, From, To) ->
    gen_server:call(Pid, {getMessages, From, To}).

get_session_parameter(SessionId) ->
    Name = erlang:list_to_atom(lists:concat([?MODULE,  SessionId])),
    lager:info("get_session_parameter call ~p:~p", [Name, whereis(Name)]),
    gen_server:call(Name, get_session_parameter).

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
init([Pid, FixSender, Session]) ->
    Id = Session#session_parameter.id,
    lager:md([{session, Id}]),
    {{Y,M,D},_} = erlang:universaltime(),
    lager:trace_file(lists:concat(["log/session_", Id,"_",
                                   Y,M,D,".log"]),
                                  [{session, Id}], info),
    TableNames = fixerl_mnesia_utils:get_tables_name(Id),
    [In|_] = TableNames,
    case mnesia:table_info(In, size) of
        C when erlang:is_integer(C) -> 
            State = #state{session_params = Session,
                           fix_version = Session#session_parameter.fix_version,
                           pid = Pid, 
                           fixSender = FixSender,
                           session_id = Id, 
                           senderCompID = Session#session_parameter.senderCompId,
                           targetCompID = Session#session_parameter.targetCompId, 
                           callback = Session#session_parameter.callback,
                           role = Session#session_parameter.role,
                           mode = Session#session_parameter.callback_mode,
                           mnesia_tables_name = TableNames,
                          count = C},
            {ok, State};
        {aborted, Reason} ->
            {stop, Reason}
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
handle_call(get_session_parameter, _From, State) ->
    {reply, State#state.session_params, State};
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
handle_cast({message, {Msg, NotStandardFields}}, 
             #state{pid = Pid, fixSender = FixSender, 
                                   fix_version = FixVersion,
                                   senderCompID = SenderCompID, 
                                   targetCompID = TargetCompID,
                                   callback = {M,F}, 
                                   role = Role, 
                                   session_id = Id,
                                   mode = Mode,
                                   mnesia_tables_name = [Tin,Tout]} = State) ->
    ?LOG(Id, " <- ~s", [fix_convertor:format(Msg, FixVersion)]),
    CheckMsgSeqNum = ((State#state.session_params)#session_parameter.message_checks)#message_checks.check_msgSeqNum,
    CheckCompIds = ((State#state.session_params)#session_parameter.message_checks)#message_checks.check_CompIds,
    SyncFlagOld = State#state.sync_flag,
    InC = fix_utils:get_seq_number(FixVersion, Msg),
    C = State#state.count + 1,
    {SyncFlag, NewC, Diff, DiffCount} = 
    case {SyncFlagOld, InC == C, InC < C, CheckMsgSeqNum} of
        {false, true, false, _} -> 
                process_msg(Msg, NotStandardFields, Pid, FixSender, FixVersion,
                            SenderCompID, TargetCompID, M, F, Role, Id, Mode, Tin, Tout,
                            CheckCompIds, State#state.count + 1),
                ?LOG(Id, "session is ok", []),
                {false, State#state.count + 1, undefined, undefined};
        {true, true, false, true} -> 
                process_msg(Msg, NotStandardFields, Pid, FixSender, FixVersion,
                            SenderCompID, TargetCompID, M, F, Role, Id, Mode, Tin, Tout,
                            CheckCompIds, State#state.count + 1),
                case State#state.diff > State#state.diff_count of
                    false ->
                        ?LOG(Id, "session is sync again", []),
                        {false, State#state.count + 1, 
                         undefined, undefined};
                    true ->
                        ?LOG(Id, "session sync run: ~p ~p", [State#state.diff , State#state.diff_count]),
                        {true, State#state.count + 1, 
                         State#state.diff , State#state.diff_count +1}
                end;
        {false, false, true, true} ->
                ?LOG(Id, "msgSeqNum too low - accepted: ~p received: ~p ",
                    [C, InC]),
                fix_gateway:send(FixSender,
                    fix_utils:get_logout(FixVersion,
                                        SenderCompID,
                                        TargetCompID)),
                erlang:exit(msg_seq_num_too_low);
        {true, false, true, true} ->
                    process_msg(Msg, NotStandardFields, Pid, FixSender, FixVersion,
                                SenderCompID, TargetCompID, M, F, Role, Id, Mode, Tin, Tout,
                                CheckCompIds, State#state.count + 1),
                    {true, State#state.count + 1, State#state.diff , State#state.diff_count +1};
        {false, false, false, true} ->
            ?LOG(Id, " accepted msgSeqNum: ~p received: ~p send resend request",
                [C, InC]),
            fix_gateway:send(FixSender,
                        fix_utils:get_resend_request(FixVersion,
                                            SenderCompID,
                                            TargetCompID, InC, C)),
            {true, State#state.count, InC - C, 0};
        {true, false, false, true} ->
            ?LOG(Id, " accepted msgSeqNum: ~p received: ~p sync in doing - ignore msg",
                [C, InC]),
            {true, State#state.count, State#state.diff , State#state.diff_count};
        {_, _, _, false} ->
            process_msg(Msg, NotStandardFields, Pid, FixSender, FixVersion,
                        SenderCompID, TargetCompID, M, F, Role, Id, Mode, Tin, Tout,
                        CheckCompIds, State#state.count + 1),
            ?LOG(Id, " accepted msgSeqNum: ~p received: ~p",
                [C, InC]),
            {false, State#state.count, undefined, undefined};
        Else ->
            ?LOG(Id, " Something is wrong: ~p ", [Else]),
            erlang:exit(todo)
    end,
    {noreply, State#state{count = NewC, 
                          sync_flag = SyncFlag, diff = Diff, diff_count = DiffCount}}.

process_msg(Msg, NotStandardFields, Pid, FixSender, FixVersion,
            SenderCompID, TargetCompID, M, F, Role, Id, Mode, Tin, Tout,
            CheckCompIds, C) ->
    mnesia:transaction(fun() ->
        mnesia:write({Tin, C , Msg}) end),
    case erlang:element(1, Msg) of
        %%TODO sessionhandling
        logon -> 
            case not CheckCompIds orelse ok == fix_utils:check_logon(FixVersion,
                                       Msg, 
                                       SenderCompID, 
                                       TargetCompID) of
                true -> 
                    case Role of
                        acceptor ->
                            fix_gateway:send(FixSender,
                                    fix_utils:get_logon(FixVersion,
                                                        SenderCompID,
                                                       TargetCompID));
                        initiator -> ok
                    end,
                    Pid ! fix_starting;
                false -> 
                    fix_gateway:send(FixSender,
                        fix_utils:get_logout(FixVersion,
                                            SenderCompID,
                                            TargetCompID)),
                    erlang:exit(false_logon)
            end;
        testRequest -> 
            fix_gateway:send(FixSender,
                             fix_utils:get_heartbeat(FixVersion,
                                                     Msg));
        heartbeat -> ok;
        logout ->
            fix_gateway:send(FixSender,
                fix_utils:get_logout(FixVersion,
                                     SenderCompID,
                                     TargetCompID)),
            erlang:exit(fix_session_close);
        resendRequest ->
            lists:map(fun(Num) -> 
                case  mnesia:dirty_read({Tout, Num}) of
                    [{Tout, Num, ResendMessage}] ->
                        fix_gateway:resend(FixSender, ResendMessage);
                    [] -> ok
                end end, 
                 fix_utils:get_numbers(FixVersion, Msg));
        _Else ->
            case Mode of
                all -> 
                    M:F(Id, Msg, NotStandardFields);
                _Standard ->
                    M:F(Id, Msg)
            end
    end.

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

