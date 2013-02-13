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
-export([start_link/5, send/2, resend/2, send_heartbeat/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
        terminate/2, code_change/3]).

-record(state, {socket, count = 0, fix_version, 
                senderCompID, targetCompID}).

%% ====================================================================
%% External functions
%% ====================================================================
send(Pid, Message)->
    gen_server:cast(Pid, {send, Message}).
send_heartbeat(Pid)->
    gen_server:cast(Pid, send_heartbeat).
resend(Pid, Message)->
    gen_server:cast(Pid, {resend, Message}).

%% ====================================================================
%% Server functions
%% ====================================================================
start_link(Socket, FixVersion, SenderCompID, TargetCompID, Id)->
    gen_server:start_link({local, Id},?MODULE, 
        [Socket, FixVersion, SenderCompID, TargetCompID], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Socket, FixVersion, SenderCompID, TargetCompID]) ->
    case mnesia:table_info(fix_out_messages, size) of
        C when erlang:is_integer(C) -> 
            {ok, #state{socket = Socket, count = C, 
                        fix_version = FixVersion, 
                        senderCompID = SenderCompID, 
                        targetCompID = TargetCompID}};
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(send_heartbeat, #state{socket = Socket, count = Count, 
            senderCompID = SenderCompID, targetCompID = TargetCompID,
            fix_version = FixVersion} = State) ->
    try 
        NewCount = Count+1,
        Record = convertor:setMsgSeqNum(fix_utils:get_heartbeat(
                                        SenderCompID, TargetCompID), 
                                        NewCount, FixVersion), 
        Bin = convertor:convertRecordToFix(Record, FixVersion), 
        mnesia:transaction(fun() -> 
            mnesia:write({fix_out_messages, NewCount , Bin}) end),
        gen_tcp:send(Socket, Bin),
        lager:debug("FIX OUT MESSAGE: ~p", 
                    [convertor:format(Record, FixVersion)])
    catch error:Error -> 
            lager:error("~p - ERROR: ~p~n", [?MODULE, Error])
    end,
    {noreply, State};
handle_cast({resend, Bin}, #state{socket = Socket} = State) ->
    try 
        gen_tcp:send(Socket, Bin)
    catch error:Error -> 
            lager:error("~p - ERROR: ~p~n", [?MODULE, Error])
    end,
    {noreply, State};
handle_cast({send, Record}, #state{socket = Socket, count = Count, 
                                   fix_version = FixVersion} = State)
            when erlang:is_tuple(Record) ->
    NewCount = Count+1,
    try 
        NewRecord = convertor:setMsgSeqNum(Record, 
                                           NewCount, FixVersion), 
        Bin = convertor:convertRecordToFix(NewRecord, FixVersion), 
        mnesia:transaction(fun() -> 
            mnesia:write({fix_out_messages, NewCount , Bin}) end),
        gen_tcp:send(Socket, Bin),
        lager:info("FIX OUT MESSAGE -> ~p", 
                   [convertor:format(NewRecord, FixVersion)])
    catch error:Error -> 
            lager:error("~p - ERROR: ~p~n", [?MODULE, Error])
    end,
    {noreply, State#state{count = NewCount}};
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

