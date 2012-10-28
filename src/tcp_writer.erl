%%% -------------------------------------------------------------------
%%% Author  : Maxim Minin
%%% Description :
%%%
%%% Created : 28.06.2012
%%% -------------------------------------------------------------------
-module(tcp_writer).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("FIX_4_2.hrl"). %%TODO
%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, send/2, resend/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket, count = 0}).

%% ====================================================================
%% External functions
%% ====================================================================
send(Pid, Message)->
    gen_server:cast(Pid, {send, Message}).
resend(Pid, Message)->
    gen_server:cast(Pid, {resend, Message}).

%% ====================================================================
%% Server functions
%% ====================================================================
start_link(Socket)->
    gen_server:start_link(?MODULE, [Socket], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Socket]) ->
    case mnesia:table_info(fix_out_messages, size) of
        C when erlang:is_integer(C) -> 
            {ok, #state{socket = Socket, count = C}};
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
handle_cast({resend, Bin}, #state{socket = Socket} = State) ->
    try 
        gen_tcp:send(Socket, Bin)
    catch error:Error -> io:format("ERROR: ~p~n", [Error])
    end,
    {noreply, State};
handle_cast({send, Record}, #state{socket = Socket, count = Count} = State) when erlang:is_tuple(Record) ->
    NewCount = Count+1,
    try 
        io:format("TO BE SEND: ~p~n", [Record]),
        Header = erlang:element(2, Record),
        NewHeader = Header#standardHeader{msgSeqNum = NewCount},
        NewRecord = erlang:setelement(2, Record, NewHeader),
        Bin = convertor:convertRecordtoFix(NewRecord, "FIX_4_2"), %%TODO
        mnesia:transaction(fun() -> mnesia:write({fix_out_messages, NewCount , Bin}) end),
        io:format("TO BE SEND: ~p~n", [Bin]),
        gen_tcp:send(Socket, Bin)
    catch error:Error -> io:format("ERROR: ~p~n", [Error])
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
terminate(Reason, _State) ->
    io:format("TCP_WRITER TERMINATE: ~p~n", [Reason]),
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

