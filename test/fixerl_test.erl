%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_test
-module(fixerl_test).

%%
%% Include files
%%
-include("fixerl.hrl").
-include_lib("fix_convertor/include/FIX_4_2.hrl").
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([
          start_sessions/0, stop/1, callback/2, callback1/2, receiver/1, sender/0
         ]).

%%
%% API Functions
%%


fixerl_test_() ->
{"Simple test",
{setup,
    fun start_sessions/0,
    fun stop/1,
    fun(_SetupData) ->
        {inparallel,
            [
             {timeout, 600, ?_assert(erlang:is_pid(spawn(?MODULE, sender, [])))},
             {timeout, 600, ?_assert(receiver(0))}
            ]}
end}}.

stop(_Args) ->
application:stop(fixerl),
application:stop(mnesia),
lager:notice("DIR:~p", [file:get_cwd()]),
application:stop(lager).

start_sessions() ->
net_kernel:start([fixerl, shortnames]),
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

sender() ->
timer:sleep(2000),
lager:info("sender"),
    RecA = #marketDataIncrementalRefresh{ standardHeader = #standardHeader{
                                                                                msgType = marketDataIncrementalRefresh,
                                                                                senderCompID = "SNDR",
                                                                                targetCompID = "TRGT",
                                                                                sendingTime = "20110802-10:00:00"},
                                             rgr_marketDataIncrementalRefresh_268 = 
                                                 [#rgr_marketDataIncrementalRefresh_268{mDUpdateAction = change,
                                                                                                     mDEntryRefID = "0001",
                                                                                                    mDEntryPx = 10},
                                                  #rgr_marketDataIncrementalRefresh_268{mDUpdateAction = change,
                                                                                                    mDEntryRefID = "0002",
                                                                                                    mDEntryPx = 11}],
                                            standardTrailer = #standardTrailer{}},
  Nums = lists:seq(1, 10001),
  lists:map(fun(_X) -> timer:sleep(10),
                       fix_gateway:send(test1, RecA) end, Nums),
  timer:sleep(2000),
true.

callback(_Id, M) ->
  %%lager:info("MESSAGE IN CALLBACK: ~p", [M]),
  case erlang:whereis(receiver) of 
     P when erlang:is_pid(P) -> P ! M;
     _Else -> ok
  end,
  ok.
callback1(_Id, M) ->
ok.

receiver(0) ->
  erlang:register(receiver, self()),
  receive
     M -> lager:notice("START: ~p", [erlang:now()]),
           receiver(1, erlang:now())
  end.
receiver(10000, StartTime) ->
  receive
     M -> 
EndTime = erlang:now(),
{_, Ss, Sm} = StartTime,
{_, Es, Em} = EndTime,
Diff = Es*1000000+Em-Ss*1000000-Sm,
lager:notice("Start: ~p ENDE: ~p Diff:~p", [StartTime, EndTime,Diff]),
fixerl:stop_session(test),
fixerl:stop_session(test1),
true
  end;
receiver(X, StartTime) ->
  receive
     M -> 
           receiver(X+1, StartTime)
  end.

%%
%% Local Functions
%%

