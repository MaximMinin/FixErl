%%% -------------------------------------------------------------------
%%% Author  : Maxim Minin
%%% Description :
%%%
%%% Created : 27.05.2012
%%% -------------------------------------------------------------------
-module(fix_worker).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, newMessage/2, getMessages/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {clients = [], pid, fixSender, count = 0}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Pid, FixSender) ->
    gen_server:start_link(?MODULE, [Pid, FixSender], []).

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
init([Pid, FixSender]) ->
    State = #state{pid = Pid, fixSender = FixSender},
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
handle_cast({message, Msg}, #state{pid = Pid, fixSender = FixSender, count = C} = State) ->
    mnesia:transaction(fun() -> mnesia:write({fix_in_messages, C+1 , Msg}) end),
    case erlang:element(1, Msg) of
        %%TODO sessionhandling
        logon -> Pid ! fix_starting;
        testRequest -> tcp_writer:send(FixSender, ""); 
        heartbeat -> io:format("HEARTBEAT: ~p~n", [Msg]);
        logout -> io:format("LOGOUT: ~p~n", [Msg]), erlang:exit(fix_session_close);
        resendRequest -> lists:map(fun(Num) -> 
                                           [{fix_out_messages, Num, ResendMessage}] = mnesia:dirty_read(({fix_out_messages, Num})),
                                           tcp_writer:resend(FixSender, ResendMessage)
                                   end, 
                                   fix_utils:get_messagesnumbers_toresend(Msg))
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
terminate(Reason, _State) ->
    io:format("REASON: ~p~n", [Reason]),
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
