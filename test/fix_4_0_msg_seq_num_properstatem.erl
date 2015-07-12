%% Author: Maxim Minin
%% Created: 01.09.2014
%% Description: TODO: Add description to fix_4_0_msg_seq_num_properstatem
-module(fix_4_0_msg_seq_num_properstatem).

-behaviour(proper_statem).

-include("fixerl_proper.hrl").
-include_lib("fix_convertor/include/FIX_4_0.hrl").

-define(FIX_VERSION, 'FIX 4.0').

-define(ID_, fix_4_0_msg_seq_num_properstatem_send).
-record(state, {messages = []}).

%%% Property

prop_master() ->
    ?FORALL(
       Cmds, commands(?MODULE),
       ?TRAPEXIT(
      begin
          fix_4_0_msg_seq_num_properstatem:setup(),
          {History, State, Result} = run_commands(?MODULE, Cmds),
          timer:sleep(100),
		  fixerl:send(?ID_, fix_utils:get_heartbeat(?FIX_VERSION, "TEST", "TEST1")),
		  MsgNumOut = erlang:length(State#state.messages)+2,
          timer:sleep(500),
		  MsgNumIn = fix_worker:get_message_count(?MODULE),
          fix_4_0_msg_seq_num_properstatem:clean(),
          ?WHENFAIL(
              begin
                ?EMERGENCY("Test failed", []),
                ?EMERGENCY("In: ~p Out: ~p Result: ~p", [MsgNumIn, MsgNumOut, Result]),
                ?EMERGENCY("History: ~w", [History]),
                ?EMERGENCY("State: ~w", [State])
              end,
              aggregate(command_names(Cmds), Result =:= ok andalso MsgNumOut == MsgNumIn))
      end)).

initial_state() ->
    #state{}.

command(#state{}) ->
    oneof([
          {call,fix_gateway,save,[?ID_, fix_4_0_record_generator:test_record()]},
          {call,fix_gateway,save,[?ID_, fix_4_0_record_generator:test_record()]},
          {call,fix_gateway,save,[?ID_, fix_4_0_record_generator:test_record()]},
          {call,fixerl,send,[?ID_, fix_4_0_record_generator:test_record()]}
          ]).

next_state(S, _V, {call,_,_command,[?ID_, Record]}) ->
    S#state{messages = [Record|S#state.messages]}.

precondition(_S, {call,_,_command,[_Name, Record]}) ->
    case erlang:element(1, Record) of
        logout -> false;
        sequenceReset -> false;
        resendRequest -> false;
        _ -> true
    end.

postcondition(_S, {call,_,_command,[_Name, _Record]}, _Result) ->
    true.

setup() ->
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    fixerl_mnesia_utils:init(),
    S1 = #session_parameter{
                             id = ?ID_, 
                             port = 11120,  
                             senderCompId = "TEST1", targetCompId = "TEST",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 10, role = acceptor, 
                             message_checks = #message_checks{check_msgSeqNum = false},
                             callback = {?MODULE, callback1}
                           },
    application:set_env(fixerl, sessions, [S1]),
    Ret = fixerl:start(),
    S = #session_parameter{
                             id = ?MODULE, 
                             host = localhost, port = 11120, max_reconnect = 10,
                             reconnect_interval = 20, 
                             senderCompId = "TEST", targetCompId = "TEST1",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 10, role = initiator, 
                             message_checks = #message_checks{check_msgSeqNum = true},
                             callback = {?MODULE, callback}, callback_mode = all
                           },
    fixerl:start_session(S),
    timer:sleep(100),
    Ret.

clean() ->
    application:set_env(fixerl, sessions, []),
    application:stop(fixerl),
    application:stop(mnesia).

callback(_Id, _M, _N) ->
    ok.
callback(_Id, _M) ->
    ok.

callback1(_Id, _M, _N) ->
    ok.
callback1(_Id, _M) ->
    ok.
