%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created : 28.06.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fixerl_tcp_listener).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Port, ConcurrentAcceptorCount, AcceptorSup) ->
    gen_server:start_link(
      ?MODULE, {Port, ConcurrentAcceptorCount, AcceptorSup}, []).


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
init({Port, ConcurrentAcceptorCount, AcceptorSup}) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, 
                        [
                         binary,
                         {packet, raw},
                         {reuseaddr, true},
                         {exit_on_close, false},
                         {active, false}]) of
        {ok, LSock} ->
            lists:foreach(fun (_) ->
                              {ok, _APid} = supervisor:start_child(
                                                AcceptorSup, [LSock])
                          end, 
                lists:duplicate(ConcurrentAcceptorCount, dummy)),
            {ok, {LIPAddress, LPort}} = inet:sockname(LSock),
            lager:error("started TCP listener on ~s:~p~n",
                        [inet_parse:ntoa(LIPAddress), LPort]),
            {ok, #state{
                        sock=LSock
                       }};
        {error, Reason} ->
           lager:error(
              "failed to start TCP listener on: ~p - ~p~n",
              [Port, Reason]),
            {stop, {cannot_listen, Port, Reason}}
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
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, #state{sock=LSock}) ->
    {ok, {IPAddress, Port}} = inet:sockname(LSock),
    gen_tcp:close(LSock),
    lager:error("stopped TCP listener on ~s:~p~n",
                          [inet_parse:ntoa(IPAddress), Port]).

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
