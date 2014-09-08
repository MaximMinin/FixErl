%% Author: Maxim Minin
%% Created: 01.09.2014
%% Description: TODO: Add description to fix_4_2_properstatem
-module(fix_4_2_properstatem).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include("fixerl.hrl").

-compile([{no_auto_import, [date/0, time/0]}, 
          export_all, debug_info]).
-export([initial_state/0, next_state/3,
         precondition/2,postcondition/3,
         command/1]).
-define(MASTER, fixerl).
-define(DUMMY, dummy).

-record(state, {messages = []}).

%%% Property

prop_master() ->
    ?FORALL(
       Cmds, commands(?MODULE),
       ?TRAPEXIT(
      begin
          start_dummy(),
          {History, State, Result} = run_commands(?MODULE, Cmds),
          Messages = stop_dummy(),
          check_messages(Messages, State#state.messages),
          ?WHENFAIL(
            io:format("History: ~w\n State: ~w\n",
               [History, State]),
         aggregate(command_names(Cmds), Result =:= ok))
      end)).

initial_state() ->
    #state{}.

command(#state{}) ->
    {call,?MASTER,send,[test1, fix_4_2_record_generator:test_record()]}.

next_state(S, _V, {call,_,send,[_Name, Record]}) ->
    S#state{messages = [Record|S#state.messages]}.

precondition(_S, {call,_,send,[_Name, Record]}) ->
    case erlang:element(1, Record) of
        logon -> false;
        logout -> false;
        testRequest -> false;
        heartbeat -> false;
        resendRequest -> false;
        _ -> true
    end.

postcondition(_S, {call,_,send,[_Name, _Record]}, _Result) ->
    true.

setup() ->
    mnesia:delete_schema([node()|nodes()]),
    mnesia:create_schema([node()|nodes()]),
    lager:start(),
    lager:set_loglevel(lager_console_backend, notice),
    fixerl_mnesia_utils:init(),
    Ret = application:start(fixerl),
    S1 = #session_parameter{
                             id = test1, 
                             port = 12345,  
                             senderCompId = "TEST1", targetCompId = "TEST", fix_version = 'FIX 4.2',
                             heartbeatInterval = 30, role = acceptor,
                             callback = {?MODULE, callback1}
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = test, 
                             host = localhost, port = 12345, max_reconnect = 10, reconnect_interval = 20, 
                             senderCompId = "TEST", targetCompId = "TEST1", fix_version = 'FIX 4.2',
                             heartbeatInterval = 30, role = initiator,
                             callback = {?MODULE, callback}
                           },
    fixerl:start_session(S),
    Ret.

clean() ->
    fixerl:stop_session(test),
    fixerl:stop_session(test1),
    application:stop(fixerl),
    application:stop(lager).

callback(_Id, M) ->
  ?DUMMY ! M,
  ok.
callback1(_Id, _M) ->
    ok.

start_dummy() ->
    Pid = erlang:spawn(?MODULE, receiver_dummy, [[]]),
    erlang:register(?DUMMY, Pid).
    
receiver_dummy(L) ->
  receive
     {stop, Pid} -> 
         erlang:unregister(?DUMMY),
         Pid ! L; 
     M ->
         receiver_dummy([M|L])
  after 1000 -> ok
  end.

stop_dummy() ->
    timer:sleep(100),
    ?DUMMY ! {stop, self()},
    receive
        M -> M
    after 1000 -> []
    end.

check_messages([],[]) -> ok;
check_messages([M|R], [M1|R1]) ->
    true = fix_4_2_record_generator:is_eq(M, M1),
    check_messages(R, R1).

