%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created : 03.06.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fix_splitter).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, newRowData/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {last = <<>>, clientPid, fix_version}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Pid, FixVersion)->
    gen_server:start_link(?MODULE, [Pid, FixVersion], []).

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
init([Client, FixVersion]) ->
    {ok, #state{clientPid = Client, fix_version = FixVersion}}.

newRowData(Pid, Data) ->
    gen_server:cast(Pid, {new, Data}).

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
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({new, Data}, #state{last = Last, clientPid = ClientPid, 
            fix_version = FixVersion} = State) ->
    {Broken, Messages} = split(binary:list_to_bin([Last, Data])),
    lists:map(fun(M) ->  
      try Rec = convertor:convertFixToRecord(M, FixVersion),
          fix_worker:newMessage(ClientPid, Rec),
          lager:info("FIX IN MESSAGE <- ~p", 
                    [convertor:format(Rec, FixVersion)])
     catch error:Error -> 
            lager:error("~p - MESSAGE CAN NOT BE INTERPRETED: ~p~n",
                        [M, Error])
     end
     end, Messages),
    {noreply, State#state{last = Broken}}.

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
split(Bin) ->
    [L|Liste] = binary:split(Bin, <<"10=">>, [global]),
    split(Liste, L, []).

split([E|[]], Last, ToReturn) ->
    case binary:split(E, <<1>>) of 
        [<<>>] ->
            {Last, ToReturn};
        [Int|Rest] ->
            {binary:list_to_bin(Rest), 
            [binary:list_to_bin([Last,<<"10=">>,Int,<<1>>])|ToReturn]}
    end;
split([E|Liste], Last, ToReturn) ->
    [Int|Rest] =  binary:split(E, <<1>>), 
    split(Liste, Rest, 
         [binary:list_to_bin([Last, <<"10=">>, Int, <<1>>])|ToReturn]). 
