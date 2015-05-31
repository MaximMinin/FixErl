%% Author: Maxim Minin
%% Created: 01.09.2014
%% Description: TODO: Add description to fix_5_0_msg_seq_num_properstatem
-module(fix_5_0_msg_seq_num_properstatem).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include("fixerl.hrl").
-include_lib("fix_convertor/include/FIX_5_0.hrl").

-compile([{no_auto_import, [date/0, time/0]}, 
          export_all, debug_info]).
-export([initial_state/0, next_state/3,
         precondition/2,postcondition/3,
         command/1]).

-define(FIX_VERSION, 'FIX 5.0').
-define(MASTER, fixerl).
-define(DUMMY, dummy).
-define(ID_, fix_5_0_msg_seq_num_properstatem_send).
-record(state, {messages = []}).

%%% Property

prop_master() ->
    ?FORALL(
       Cmds, commands(?MODULE),
       ?TRAPEXIT(
      begin
          fix_5_0_msg_seq_num_properstatem:setup(),
          true = erlang:register(?DUMMY, self()),
          timer:sleep(200),
          {History, State, Result} = run_commands(?MODULE, Cmds),
          Messages  = receive_messages(),
          
          erlang:unregister(?DUMMY),
          fix_5_0_msg_seq_num_properstatem:clean(),
          io:format("State:~p~n" ,[State#state.messages]),
          io:format("Receive:~p~n",[Messages]),
          io:format("Result:~p~n",[Result]),
          C = check_messages(State#state.messages, Messages),
          io:format("check_messages:~p~n",[C]),
          %%true = C,
          ?WHENFAIL(
            io:format("State:~p~nReceive:~p~nHistory: ~w\n State: ~w\n",
               [lists:sort(State#state.messages),lists:sort(Messages), History, State]),
         aggregate(command_names(Cmds), Result =:= ok))
      end)).

initial_state() ->
    #state{}.

command(#state{}) ->
    oneof([
           {call,?MASTER,send,[?ID_, fix_5_0_record_generator:test_record()]},
           {call,fix_gateway,save,[?ID_, fix_5_0_record_generator:test_record()]}
          ]).

next_state(S, _V, {call,_,_command,[?ID_, Record]}) ->
    S#state{messages = [Record|S#state.messages]}.

precondition(_S, {call,_,_command,[_Name, Record]}) ->
    case erlang:element(1, Record) of
        logon -> false;
        logout -> false;
        resendRequest -> false;
        testRequest -> false;
        heartbeat -> false;
        _ -> true
    end.

postcondition(_S, {call,_,_command,[_Name, _Record]}, _Result) ->
    true.

setup() ->
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    fixerl_mnesia_utils:init(),
    Ret = fixerl:start(),
    S1 = #session_parameter{
                             id = ?ID_, 
                             port = 12345,  
                             senderCompId = "TEST1", targetCompId = "TEST", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = acceptor, 
                             message_checks = #message_checks{check_msgSeqNum = true},
                             callback = {?MODULE, callback1}
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = ?MODULE, 
                             host = localhost, port = 12345, max_reconnect = 10, reconnect_interval = 20, 
                             senderCompId = "TEST", targetCompId = "TEST1", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = initiator, 
                             message_checks = #message_checks{check_msgSeqNum = true},
                             callback = {?MODULE, callback}
                           },
    fixerl:start_session(S),
    Ret.

clean() ->
    fixerl:stop_session(?MODULE),
    fixerl:stop_session(?ID_),
    application:stop(fixerl),
    application:stop(mnesia).

reset_sessions() ->
    S = fix_worker:get_session_parameter(?MODULE),
    S1 = fix_worker:get_session_parameter(?ID_),
    fixerl:reset_session(?MODULE),
    fixerl:reset_session(?ID_),
    fixerl:start_session(S1),
    fixerl:start_session(S).

callback(_Id, M) ->
    ?DUMMY ! M,
    ok.

callback1(_Id, _M) ->
    ok.

receive_messages() ->
    receive_messages([]).

receive_messages(L) ->
  receive
     M->
         receive_messages([M|L])
  after 100 -> L
  end.

check_messages(L, L1) ->
   io:format("OUT: ~p IN: ~p~n", [length(L),length(L1)]),
    erlang:length(L) == erlang:length(L1).
