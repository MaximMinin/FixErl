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

-define(FIX_VERSION, 'FIX 4.2').
-define(MASTER, fixerl).
-define(DUMMY, dummy).

-record(state, {messages = []}).

%%% Property

prop_master() ->
    ?FORALL(
       Cmds, commands(?MODULE),
       ?TRAPEXIT(
      begin
          erlang:register(?DUMMY, self()),
          {History, State, Result} = run_commands(?MODULE, Cmds),
          Messages = receive_messages(),
          erlang:unregister(?DUMMY),
          check_messages(State#state.messages, Messages),
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
    case erlang:element(1, Record) of
        resendRequest -> 
            case fix_utils:get_numbers(?FIX_VERSION, Record) of
                [] -> S;
                L ->
                    [A|_] = L,
                    E = lists:last(L),
                    Resended = lists:sublist(S#state.messages, A, E-A),
                    S#state{messages = lists:append(S#state.messages, Resended)}
            end;
        heartbeat ->
            S;
        testRequest ->
            S;
        _ -> S#state{messages = [Record|S#state.messages]}
    end.

precondition(_S, {call,_,send,[_Name, Record]}) ->
    case erlang:element(1, Record) of
        logon -> false;
        logout -> false;
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
    Ret = fixerl:start(),
    S1 = #session_parameter{
                             id = test1, 
                             port = 12345,  
                             senderCompId = "TEST1", targetCompId = "TEST", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 30, role = acceptor,
                             callback = {?MODULE, callback1}
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = test, 
                             host = localhost, port = 12345, max_reconnect = 10, reconnect_interval = 20, 
                             senderCompId = "TEST", targetCompId = "TEST1", fix_version = ?FIX_VERSION,
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

receive_messages() ->
    receive_messages([]).

receive_messages(L) ->
  receive
     M ->
         receive_messages([M|L])
  after 300 -> L
  end.

check_messages([],[]) -> ok;
check_messages([M|R], [M1|R1]) ->
    true = fix_4_2_record_generator:is_eq(M, M1),
    check_messages(R, R1).

