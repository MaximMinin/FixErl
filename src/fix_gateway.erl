%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created : 28.06.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fix_gateway).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/6, send/2, send/3, save/2,
         resend/2, send_heartbeat/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
        terminate/2, code_change/3]).

-record(state, {socket, count = 0, fix_version, 
                senderCompID, targetCompID,
                table_out_name, id}).

%% ====================================================================
%% External functions
%% ====================================================================
save(Pid, Message) ->
    gen_server:call(Pid, {save, Message}).
send(Pid, Message)->
    gen_server:call(Pid, {send, Message, <<>>}).
send(Pid, Message, NotStandardPart)->
    gen_server:call(Pid, {send, Message, NotStandardPart}).
send_heartbeat(Pid)->
    gen_server:call(Pid, send_heartbeat).
resend(Pid, Message)->
    gen_server:call(Pid, {resend, Message}).

%% ====================================================================
%% Server functions
%% ====================================================================
start_link(Socket, FixVersion, 
           SenderCompID, TargetCompID, Id, StartSeqNum)->
    [_InTabel, OutTable] = fixerl_mnesia_utils:get_tables_name(Id),
    gen_server:start_link({local, Id},?MODULE, 
        [Socket, FixVersion, 
         SenderCompID, TargetCompID, 
         OutTable, StartSeqNum, Id], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Socket, FixVersion, 
      SenderCompID, TargetCompID, 
      OutTable, undefined, Id]) ->
    lager:md([{session, Id}]),
    {{Y,M,D},_} = erlang:universaltime(),
    lager:trace_file(lists:concat(["fixlog/session_", Id,"_",
                                   Y,M,D,".log"]),
                                  [{session, Id}], info),
    case mnesia:table_info(OutTable, size) of
        C when erlang:is_integer(C) -> 
            {ok, #state{socket = Socket, count = C, 
                        fix_version = FixVersion, 
                        senderCompID = SenderCompID, 
                        targetCompID = TargetCompID,
                        table_out_name = OutTable,
                        id = Id}};
        {aborted, Reason} ->
            {stop, Reason}
    end;
init([Socket, FixVersion, 
      SenderCompID, TargetCompID, 
      OutTable, StartSeqNum, Id]) ->
    lager:md([{session, Id}]),
    {{Y,M,D},_} = erlang:universaltime(),
    lager:trace_file(lists:concat(["fixlog/session_", Id,"_",
                                   Y,M,D,".log"]),
                                  [{session, Id}], info),
    {ok, #state{socket = Socket, count = StartSeqNum, 
                fix_version = FixVersion, 
                senderCompID = SenderCompID, 
                targetCompID = TargetCompID,
                table_out_name = OutTable,
                id = Id}}.

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
handle_call(send_heartbeat, _From,
            #state{socket = Socket, count = Count, 
            senderCompID = SenderCompID, targetCompID = TargetCompID,
            fix_version = FixVersion, table_out_name = T, id = Id} 
           = State) ->
    NewCount = Count+1,
    try 
        Record = fix_convertor:set_msg_seqnum(fix_utils:get_heartbeat(FixVersion,
                                        SenderCompID, TargetCompID), 
                                        NewCount, FixVersion), 
        Bin = fix_convertor:record2fix(Record, FixVersion), 
        mnesia:transaction(fun() -> 
            mnesia:write({T, NewCount , Bin}) end),
        gen_tcp:send(Socket, Bin),
        lager:info([{session, Id}, 
                    {type, out}], " -> ~p", 
                    [fix_convertor:format(Record, FixVersion)])
    catch error:Error -> 
            lager:error("~p", [Error])
    end,
    {reply, ok, State#state{count = NewCount}};
handle_call({resend, Msg}, _From, #state{socket = Socket,
                                         fix_version = FixVersion,
                                         id = Id} = State)
            when is_tuple(Msg)->
    try 
    Bin = fix_convertor:record2fix(Msg, 
                                   FixVersion), 
        gen_tcp:send(Socket, Bin),
        lager:info([{session, Id}], " resend -> ~p", 
                   [Bin])
    catch error:Error -> 
            lager:error("~p", [Error])
    end,
    {reply, ok, State};
handle_call({resend, Bin}, _From, #state{socket = Socket,
                                         id = Id} = State) ->
    try 
        gen_tcp:send(Socket, Bin),
        lager:info([{session, Id}], " resend -> ~p", 
                   [Bin])
    catch error:Error -> 
            lager:error("~p", [Error])
    end,
    {reply, ok, State};
handle_call({send, Record, NotStandardPart}, _From,
            #state{socket = Socket, count = Count, 
                   fix_version = FixVersion,
                   table_out_name = T,
                   id = Id} = State)
            when erlang:is_tuple(Record) ->
    NewCount = Count+1,
    NewRecord = fix_convertor:set_msg_seqnum(Record, 
                                       NewCount, FixVersion), 
    Bin = fix_convertor:record2fix(NewRecord, 
                                   NotStandardPart,
                                   FixVersion), 
    gen_tcp:send(Socket, Bin),
    mnesia:transaction(fun() -> 
        mnesia:write({T, NewCount , Bin}) end),
    lager:info([{session, Id}], " -> ~p", 
               [fix_convertor:format(NewRecord, FixVersion)]),
    {reply, ok, State#state{count = NewCount}};
handle_call({save, Record}, _From,
            #state{socket = Socket, count = Count, 
                   fix_version = FixVersion,
                   table_out_name = T,
                   id = Id} = State)
            when erlang:is_tuple(Record) ->
    NewCount = Count+1,
    NewRecord = fix_convertor:set_msg_seqnum(Record, 
                                       NewCount, FixVersion), 
    Bin = fix_convertor:record2fix(NewRecord, FixVersion), 
    mnesia:transaction(fun() -> 
        mnesia:write({T, NewCount , Bin}) end),
    lager:info([{session, Id}], " save -> ~p", 
               [fix_convertor:format(NewRecord, FixVersion)]),
    {reply, ok, State#state{count = NewCount}}.
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
 
