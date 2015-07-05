%% Author: Maxim Minin
%% Created: 01.09.2014
%% Description: TODO: Add description to fix_5_0_resend_properstatem
-module(fix_5_0_resend_properstatem).

-behaviour(proper_statem).

-include("fixerl_proper.hrl").
-include_lib("fix_convertor/include/FIX_5_0.hrl").

-define(FIX_VERSION, 'FIX 5.0').
-define(ID_, fix_5_0_resend_properstatem_send).
-record(state, {messages = []}).

%%% Property

prop_master() ->
    ?FORALL(
       Cmds, commands(?MODULE),
       ?TRAPEXIT(
      begin
          fix_5_0_resend_properstatem:setup(),
          erlang:register(?DUMMY, self()),
          wait_for_logon(),
          {History, State, Result} = run_commands(?MODULE, Cmds),
          Messages  = receive_messages(),
          erlang:unregister(?DUMMY),
          fix_5_0_resend_properstatem:clean(),
          ?LOG("State:~p~n" ,[State#state.messages]),
          ?LOG("Receive:~p~n",[Messages]),
          ?LOG("Result:~p~n",[Result]),
          C = check_messages(State#state.messages, Messages),
          ?LOG("check_messages:~p~n",[C]),
          true = C,
          ?WHENFAIL(
            ?EMERGENCY("State:~p~nReceive:~p~nHistory: ~w\n State: ~w\n",
               [lists:sort(State#state.messages),lists:sort(Messages), History, State]),
         aggregate(command_names(Cmds), Result =:= ok))
      end)).

initial_state() ->
    #state{}.

command(#state{messages = []}) ->
    {call,fixerl,send,[?ID_, fix_5_0_record_generator:test_record()]};
command(#state{}) ->
    oneof([
           {call,fixerl,send,[?ID_, fix_5_0_record_generator:test_record()]},
           {call,fixerl,send,[?MODULE, fix_5_0_record_generator:resend_record()]}
          ]).

next_state(S, _V, {call,_,send,[?MODULE, Record]}) ->
    case fix_utils:get_numbers(?FIX_VERSION, Record) of
        [] -> S;
        L ->
            [A|_] = L,
            E = lists:last(L),
            Resended = lists:sublist(S#state.messages, A, E-A+1),
            ?LOG("RESEND: ~p", [Resended]),
            S#state{messages = lists:append(S#state.messages, Resended)}
    end;
next_state(S, _V, {call,_,send,[?ID_, Record]}) ->
    case erlang:element(1, Record) of
        heartbeat ->
            S;
        testRequest ->
            S;
        _ -> S#state{messages = [Record|S#state.messages]}
    end.

precondition(S, {call,_,send,[?MODULE, Record]}) ->
    Length = erlang:length(S#state.messages),
    %%TODO
    Length > 4 andalso Length >= Record#resendRequest.beginSeqNo;

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

postcondition(_S, {call,_,send,[_Name, _Record]}, _Result) ->
    true.

setup() ->
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    fixerl_mnesia_utils:init(),
    Ret = fixerl:start(),
    S1 = #session_parameter{
                             id = ?ID_, 
                             port = 11116,  
                             senderCompId = "TEST1", targetCompId = "TEST",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = acceptor, 
                             message_checks = #message_checks{check_msgSeqNum = false},
                             callback = {?MODULE, callback1} 
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = ?MODULE, 
                             host = localhost, port = 11116,
                             max_reconnect = 10, reconnect_interval = 20, 
                             senderCompId = "TEST", targetCompId = "TEST1",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = initiator, 
                             message_checks = #message_checks{check_msgSeqNum = false},
                             callback = {?MODULE, callback}, 
                             logon_callback = {?MODULE, logon_succeeded}
                           },
    fixerl:start_session(S),
    Ret.

clean() ->
    fixerl:stop_session(?MODULE),
    fixerl:stop_session(?ID_),
    application:stop(fixerl),
    application:stop(mnesia).

callback(_Id, M) ->
    ?DUMMY ! M,
    ok.

logon_succeeded()->
    ?DUMMY ! logon_succeeded.

wait_for_logon() ->
    receive
        logon_succeeded -> ok
    end.

callback1(_Id, _M) ->
    ok.

receive_messages() ->
    receive_messages([]).

receive_messages(L) ->
  receive
     M->
         receive_messages([M|L])
  after 50 -> L
  end.

check_messages(L, L1) ->
   ?LOG("OUT: ~p IN: ~p~n", [length(L),length(L1)]),
    erlang:length(L) == erlang:length(L1).
