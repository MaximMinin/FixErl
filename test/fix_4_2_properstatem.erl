%% Author: Maxim Minin
%% Created: 01.09.2014
%% Description: TODO: Add description to fix_4_2_properstatem
-module(fix_4_2_properstatem).

-behaviour(proper_statem).

-include("fixerl_proper.hrl").

-define(FIX_VERSION, 'FIX 4.2').

-record(state, {messages = [],messages1 = []}).

%%% Property

prop_master() ->
    ?FORALL(
       Cmds, commands(?MODULE),
       ?TRAPEXIT(
      begin
          erlang:register(?DUMMY, self()),
          {History, State, Result} = run_commands(?MODULE, Cmds),
          {Messages, Messages1} = receive_messages(),
          erlang:unregister(?DUMMY),
          check_messages(State#state.messages, Messages),
          check_messages(State#state.messages1, Messages1),
          ?WHENFAIL(
            begin
                ?EMERGENCY("History: ~w\n State: ~w\n", [History, State]),
                ?EMERGENCY("History: ~w\n State: ~w\n", [History, State])
            end,
         aggregate(command_names(Cmds), Result =:= ok))
      end)).

initial_state() ->
    #state{}.

command(#state{}) ->
    oneof([
           {call,fixerl,send,[test1, fix_4_2_record_generator:test_record()]},
           {call,fixerl,send,[test, fix_4_2_record_generator:test_record(), 
                              fix_4_2_record_generator:fix_binary()]}
          ]).

next_state(S, _V, {call,_,send,[test, Record, _Bin]}) ->
    case erlang:element(1, Record) of
        resendRequest -> 
            case fix_utils:get_numbers(?FIX_VERSION, Record) of
                [] -> S;
                L ->
                    [A|_] = L,
                    E = lists:last(L),
                    Resended = lists:sublist(S#state.messages1, A, E-A),
                    S#state{messages1 = lists:append(S#state.messages1, Resended)}
            end;
        heartbeat ->
            S;
        testRequest ->
            S;
        _ -> S#state{messages1 = [Record|S#state.messages1]}
    end;
next_state(S, _V, {call,_,send,[_Name, Record]}) ->
    case erlang:element(1, Record) of
        heartbeat ->
            S;
        testRequest ->
            S;
        _ -> S#state{messages = [Record|S#state.messages]}
    end.

precondition(_S, {call,_,send,[test, Record, _Bin]}) ->
    case erlang:element(1, Record) of
        logon -> false;
        heartbeat -> false;
        logout -> false;
        testRequest -> false;
        sequenceReset -> false;
        resendRequest -> false;
        _ -> true
    end;

precondition(_S, {call,_,send,[_Name, Record]}) ->
    case erlang:element(1, Record) of
        logon -> false;
        heartbeat -> false;
        logout -> false;
        testRequest -> false;
        sequenceReset -> false;
        resendRequest -> false;
         _ -> true
   end.

postcondition(_S, {call,_,send,[_Name, _Record, _Bin]}, _Result) ->
    true;
postcondition(_S, {call,_,send,[_Name, _Record]}, _Result) ->
    true.

setup() ->
	application:stop(mnesia),
    mnesia:delete_schema([node()|nodes()]),
    mnesia:create_schema([node()|nodes()]),
    application:set_env(lager, error_logger_hwm, 500),
    lager:start(),
    application:start(fix_convertor),
    lager:set_loglevel(lager_console_backend, emergency),
    fixerl_mnesia_utils:init(),
    Ret = fixerl:start(),
    S1 = #session_parameter{
                             id = test1, 
                             port = 11119,  
                             senderCompId = "TEST1", targetCompId = "TEST",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = acceptor,
                             callback = {?MODULE, callback1}
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = test, 
                             host = localhost, port = 11119, max_reconnect = 10,
                             reconnect_interval = 20, 
                             senderCompId = "TEST", targetCompId = "TEST1",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = initiator,
                             callback = {?MODULE, callback}
                           },
    fixerl:start_session(S),
    Ret.

clean() ->
    application:stop(fixerl),
    application:stop(lager),
    application:stop(mnesia).

callback(_Id, M) ->
    ?DUMMY ! {test, M},
    ok.

callback1(_Id, M) ->
    ?DUMMY ! {test1, M},
    ok.

receive_messages() ->
    receive_messages([], []).

receive_messages(L, L1) ->
  receive
     {test, M}->
         receive_messages([M|L], L1);
     {test1, M}->
         receive_messages(L, [M|L1])
  after 50 -> {L, L1}
  end.

check_messages([],[]) -> ok;
check_messages([M|R], [M1|R1]) ->
    true = fix_4_2_record_generator:is_eq(M, M1),
    check_messages(R, R1).

