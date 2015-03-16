%% Author: Maxim Minin
%% Created: 01.09.2014
%% Description: TODO: Add description to fix_4_2_resend_properstatem
-module(fix_4_2_resend_properstatem).

-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include("fixerl.hrl").
-include_lib("fix_convertor/include/FIX_4_2.hrl").

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
          fix_4_2_resend_properstatem:setup(),
          erlang:register(?DUMMY, self()),
          timer:sleep(200),
          {History, State, Result} = run_commands(?MODULE, Cmds),
          Messages  = receive_messages(),
          erlang:unregister(?DUMMY),
          fix_4_2_resend_properstatem:clean(),
%%          io:format("State:~p~n" ,[State#state.messages]),
%%          io:format("Receive:~p~n",[Messages]),
          io:format("Result:~p~n",[Result]),
          true = check_messages(State#state.messages, Messages),
          ?WHENFAIL(
            io:format("State:~p~nReceive:~p~nHistory: ~w\n State: ~w\n",
               [lists:sort(State#state.messages),lists:sort(Messages), History, State]),
         aggregate(command_names(Cmds), Result =:= ok))
      end)).

initial_state() ->
    #state{}.

command(#state{messages = []}) ->
    {call,?MASTER,send,[test1, fix_4_2_record_generator:test_record()]};
command(#state{}) ->
    oneof([
           {call,?MASTER,send,[test1, fix_4_2_record_generator:test_record()]},
           {call,?MASTER,send,[test, fix_4_2_record_generator:resend_record()]}
          ]).

next_state(S, _V, {call,_,send,[test, Record]}) ->
    case fix_utils:get_numbers(?FIX_VERSION, Record) of
        [] -> S;
        L ->
            [A|_] = L,
            E = lists:last(L),
            Resended = lists:sublist(S#state.messages, A, E-A+1),
            lager:info("RESEND: ~p", [Resended]),
            S#state{messages = lists:append(S#state.messages, Resended)}
    end;
next_state(S, _V, {call,_,send,[test1, Record]}) ->
    case erlang:element(1, Record) of
        heartbeat ->
            S;
        testRequest ->
            S;
        _ -> S#state{messages = [Record|S#state.messages]}
    end.

precondition(S, {call,_,send,[test, Record]}) ->
    Length = erlang:length(S#state.messages),
    %%TODO
    Length > 4 andalso Length >= Record#resendRequest.beginSeqNo;

precondition(_S, {call,_,send,[_Name, Record]}) ->
    case erlang:element(1, Record) of
        logon -> false;
        logout -> false;
        resendRequest -> false;
        testRequest -> false;
        heartbeat -> false;
        _ -> true
    end.

postcondition(_S, {call,_,send,[_Name, _Record]}, _Result) ->
    true.

setup() ->
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    fixerl_mnesia_utils:init(),
    Ret = fixerl:start(),
    S1 = #session_parameter{
                             id = test1, 
                             port = 12345,  
                             senderCompId = "TEST1", targetCompId = "TEST", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = acceptor,
                             callback = {?MODULE, callback1}
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = test, 
                             host = localhost, port = 12345, max_reconnect = 10, reconnect_interval = 20, 
                             senderCompId = "TEST", targetCompId = "TEST1", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = initiator,
                             callback = {?MODULE, callback}
                           },
    fixerl:start_session(S),
    Ret.

clean() ->
    fixerl:stop_session(test),
    fixerl:stop_session(test1),
    application:stop(fixerl),
    application:stop(mnesia).

reset_sessions() ->
    S = fix_worker:get_session_parameter(test),
    S1 = fix_worker:get_session_parameter(test1),
    fixerl:reset_session(test),
    fixerl:reset_session(test1),
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
  after 500 -> L
  end.

check_messages(L, L1) ->
%%  io:format("OUT: ~p IN: ~p~n", [length(L),length(L1)]),
    erlang:length(L) == erlang:length(L1).
%% check_messages([],[]) -> ok;
%% check_messages([M|R], [M1|R1]) ->
%%     true = fix_4_2_record_generator:is_eq(M, M1),
%%     check_messages(R, R1).

