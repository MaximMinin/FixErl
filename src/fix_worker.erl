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
-export([start_link/3, newMessage/2, 
         get_session_parameter/1, get_message_count/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
        handle_info/2, terminate/2, code_change/3]).

-record(state, {pid, fixSender, count = 0, login, 
                mnesia_tables_name, session_params,
                sync_flag = false,diff, diff_count}).

-define(LOG(Id, FormatString, Args),
    lager:info([{session, Id}],
               FormatString, Args)).

-define(LOG_DEV(Id, FormatString, Args),
    lager:debug([{session, Id}],
               FormatString, Args)).
-define(LOG_ERROR(Id, FormatString, Args),
    lager:error([{session, Id}],
               FormatString, Args)).

-define(LOGOUT_TIMEOUT, 500).

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

get_session_parameter(SessionId) ->
    Name = erlang:list_to_atom(lists:concat([?MODULE,  SessionId])),
    lager:info("get_session_parameter call ~p:~p", [Name, whereis(Name)]),
    gen_server:call(Name, get_session_parameter).

get_message_count(SessionId) ->
    Name = erlang:list_to_atom(lists:concat([?MODULE,  SessionId])),
    gen_server:call(Name, get_message_count).

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
    lager:trace_file(lists:concat(["fixlog/session_", Id,"_",
                                   Y,M,D,".log"]),
                                  [{session, Id}], info),
    TableNames = fixerl_mnesia_utils:get_tables_name(Id),
    [In|_] = TableNames,
    case mnesia:table_info(In, size) of
        C when erlang:is_integer(C) -> 
            State = #state{session_params = Session,
                           pid = Pid, 
                           fixSender = FixSender,
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
handle_call(get_message_count, _From, State) ->
    {reply, State#state.count, State};
handle_call(get_session_parameter, _From, State) ->
    {reply, State#state.session_params, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({message, {Msg, NotStandardFields}}, 
             #state{pid = Pid, fixSender = FixSender, 
                                   session_params = Session,                        
                                   mnesia_tables_name = [Tin,Tout]} = State) ->
    FixVersion = Session#session_parameter.fix_version,
    SenderCompID = Session#session_parameter.senderCompId,
    TargetCompID = Session#session_parameter.targetCompId, 
    Id = Session#session_parameter.id, 
    ?LOG(Id, " <- ~s", [fix_convertor:format(Msg, FixVersion)]),
    CheckMsgSeqNum = ((State#state.session_params)#session_parameter.message_checks)#message_checks.check_msgSeqNum,
    SyncFlagOld = State#state.sync_flag,
    InC = fix_utils:get_seq_number(FixVersion, Msg),
    C = State#state.count + 1,
    {SyncFlag, NewC, Diff, DiffCount} = 
    case {SyncFlagOld, InC == C, InC < C, CheckMsgSeqNum} of
        {false, true, false, _} -> 
                PrMsgNum = process_msg(Msg, NotStandardFields, Pid, FixSender, Tin, Tout,
                                       State#state.count + 1, State#state.session_params,
                                       State#state.login),
                ?LOG_DEV(Id, "session is ok", []),
                {false, State#state.count + PrMsgNum, undefined, undefined};
        {true, true, false, true} -> 
                PrMsgNum = process_msg(Msg, NotStandardFields, Pid, FixSender, Tin, Tout,
                                       State#state.count + 1, State#state.session_params,
                                       State#state.login),
                case State#state.diff >= State#state.diff_count + PrMsgNum of
                    false ->
                        ?LOG(Id, "session is sync again", []),
                        {false, State#state.count + PrMsgNum, 
                         undefined, undefined};
                    true ->
                        ?LOG(Id, "session sync run: ~p ~p", [State#state.diff , State#state.diff_count]),
                        {true, State#state.count + PrMsgNum, 
                         State#state.diff , State#state.diff_count + PrMsgNum}
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
                    PrMsgNum = process_msg(Msg, NotStandardFields, Pid, FixSender, Tin, Tout,
                                		   State#state.count + 1, State#state.session_params,
                                           State#state.login),
                    {true, State#state.count + PrMsgNum, State#state.diff , State#state.diff_count +PrMsgNum};
        {false, false, false, true} ->
            case erlang:element(1, Msg) == logon andalso
                     State#state.login == undefined  of
                true ->
            ?LOG(Id, " logon whith wrong number: accepted msgSeqNum: ~p received: ~p.",
                [C, InC]),
                    process_msg(Msg, NotStandardFields, Pid, FixSender, Tin, Tout,
                                State#state.count + 1, State#state.session_params,
                                State#state.login);
                false -> ok
            end,
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
            PrMsgNum = process_msg(Msg, NotStandardFields, Pid, FixSender, Tin, Tout,
                        		   State#state.count + 1, State#state.session_params,
                                   State#state.login),
            ?LOG(Id, " accepted msgSeqNum: ~p received: ~p",
                [C, InC]),
            {false, State#state.count + PrMsgNum, undefined, undefined};
        Else ->
            ?LOG(Id, " Something is wrong: ~p ", [Else]),
            erlang:exit(todo)
    end,
    NewLogin=
    case erlang:element(1, Msg) of
        logon -> erlang:now();
        _ -> State#state.login
    end,
    {noreply, State#state{count = NewC, login = NewLogin,
                          sync_flag = SyncFlag, diff = Diff, diff_count = DiffCount}}.

process_msg(Msg, _NotStandardFields, Pid, FixSender, Tin, _Tout, C, Parameter, undefined) ->
        mnesia:transaction(fun() ->
        mnesia:write({Tin, C , Msg}) end),
    FixVersion = Parameter#session_parameter.fix_version,
    SenderCompID = Parameter#session_parameter.senderCompId,
    TargetCompID = Parameter#session_parameter.targetCompId,
    Role = Parameter#session_parameter.role,
    Id = Parameter#session_parameter.id,
    CheckCompIds = Parameter#session_parameter.message_checks#message_checks.check_CompIds,
    LogonCallback = Parameter#session_parameter.logon_callback,
    case erlang:element(1, Msg) of
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
                    Pid ! fix_starting,
                    case LogonCallback of
                        {M, F} -> M:F();
                        _ -> ok
                    end,
                    ok;
                false -> 
                    fix_gateway:send(FixSender,
                        fix_utils:get_logout(FixVersion,
                                            SenderCompID,
                                            TargetCompID)),
                    timer:sleep(?LOGOUT_TIMEOUT),
                    erlang:exit(false_logon)
            end, 1;
        _Else ->
            ?LOG(Id, "not logged - session ~p will be closed", [Id]),
            erlang:exit(not_logged)
    end;
process_msg(Msg, NotStandardFields, _Pid, FixSender, Tin, Tout, C, Parameter, _Login) ->
	FixVersion = Parameter#session_parameter.fix_version,
    SenderCompID = Parameter#session_parameter.senderCompId,
	TargetCompID = Parameter#session_parameter.targetCompId,
	Skips = Parameter#session_parameter.skip_by_resend_request,
	{M,F} = Parameter#session_parameter.callback,
	Id = Parameter#session_parameter.id,
	Mode = Parameter#session_parameter.callback_mode,
    mnesia:transaction(fun() ->
        mnesia:write({Tin, C , Msg}) end),
    case erlang:element(1, Msg) of
        logon -> 
            ?LOG_ERROR(Id, "late logon received", []),
            1;
        testRequest -> 
            fix_gateway:send(FixSender,
                             fix_utils:get_heartbeat(FixVersion,
                                                     Msg)),
            1;
        heartbeat -> 1;
        logout ->
            fix_gateway:send(FixSender,
                fix_utils:get_logout(FixVersion,
                                     SenderCompID,
                                     TargetCompID)),
            timer:sleep(?LOGOUT_TIMEOUT),
            erlang:exit(fix_session_close);
        resendRequest ->
            case fix_utils:get_from_to(FixVersion, Msg) of
                undefined -> 
                    ?LOG(Id, "no resend - messages not found", []),
                    ok;
                {From, To} ->
                    ?LOG(Id, "resend from ~p to ~p", [From, To]),
                    MS = [{{'$1','$2','$3'},
                            [{'andalso',{'>=','$2',From},{'=<','$2', To}}],
                            [{{'$1','$2','$3'}}]}],
                    
                    resend_msgs(lists:sort(mnesia:dirty_select(Tout, MS)),
                                undefined, Skips, Id, FixSender, 
                                FixVersion, SenderCompID, TargetCompID) 
            end,
            1;
        sequenceReset ->
            {NextNumber, _GapFillMessage} = fix_utils:get_reset_atr(FixVersion, Msg),
            case NextNumber == (C+1) of
                false ->
                    S = lists:seq(C+1, erlang:max(NextNumber-1,C+1)),
                    lists:map(fun(Num) -> 
                                      mnesia:transaction(fun() ->
                                                                 mnesia:write({Tin, Num , Msg})
                                                         end)
                              end, S),
                    erlang:length(S)+1;
                true ->
                    1
            end;
        _Else ->
            case Mode of
                all -> 
                    M:F(Id, Msg, NotStandardFields);
                _Standard ->
                    M:F(Id, Msg)
            end, 1
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
resend_msgs([], Acc, _Skips, Id, _FixSender, 
            _FixVersion, _SenderCompID, _TargetCompID) ->
    ?LOG(Id, "resend_msgs end: ~p", [Acc]),
    ok;
resend_msgs([{_Tout, Num, ResendMessage}|Msgs],
            Acc, Skips, Id, FixSender, 
            FixVersion, SenderCompID, TargetCompID) ->
    IsToSkip = fix_utils:is_msg_to_skip(FixVersion, 
                                        ResendMessage, Skips),
    ?LOG_DEV(Id, "resend_msgs: ~p ~p ~p ~p", [Acc, Num, IsToSkip, ResendMessage]),
    case {IsToSkip, Acc, Msgs} of
        {false, undefined, _} ->
            fix_gateway:resend(FixSender, ResendMessage),
            resend_msgs(Msgs, undefined, Skips, Id, FixSender, 
                        FixVersion, SenderCompID, TargetCompID);
        {false, Int, _} ->
            GapMsg = fix_utils:get_sequence_reset(FixVersion,
                                                  SenderCompID,
                                                  TargetCompID,
                                                  Int, Num),
            fix_gateway:resend(FixSender, GapMsg),
            fix_gateway:resend(FixSender, ResendMessage),
            resend_msgs(Msgs, undefined, Skips, Id, FixSender,
                        FixVersion, SenderCompID, TargetCompID);
        {true, undefined, []} ->
            GapMsg = fix_utils:get_sequence_reset(FixVersion,
                                                  SenderCompID,
                                                  TargetCompID,
                                                  Num, Num+1),
            fix_gateway:resend(FixSender, GapMsg);
        {true, Int, []} ->
            GapMsg = fix_utils:get_sequence_reset(FixVersion,
                                                  SenderCompID,
                                                  TargetCompID,
                                                  Int, Num+1),
            fix_gateway:resend(FixSender, GapMsg);
        {true, undefined, _} ->
            resend_msgs(Msgs, Num, Skips, Id, FixSender,
                        FixVersion, SenderCompID, TargetCompID);
        {true, Int, _} ->
            resend_msgs(Msgs, Int, Skips, Id, FixSender,
                        FixVersion, SenderCompID, TargetCompID)
    end.
