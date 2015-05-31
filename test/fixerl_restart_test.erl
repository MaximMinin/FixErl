%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_restart_test
-module(fixerl_restart_test).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("fixerl.hrl").

%%
%% Exported Functions
%%
-compile([export_all]).

-define(FIX_VERSION, 'FIX 5.0').

%%
%% API Functions
%%

fixerl_restart_test_() ->
    {timeout, 60000, ?_assert(test_run())}.

%%
%% Local Functions
%%
test_run() ->
    setup(),
    timer:sleep(5000),
    try
    fixerl:send(heartbeat1, bla)
    catch Err:F  -> lager:info("ENDE ~p  ~p", [Err, F])
    end,
    timer:sleep(3500),
    clean(),
    true.

setup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()|nodes()]),
    mnesia:create_schema([node()|nodes()]),
    lager:start(),
    lager:set_loglevel(lager_console_backend, notice),
    fixerl_mnesia_utils:init(),
    ok = application:start(fixerl),
    start_sessions().

start_sessions() ->
    start_sessions_2(),
%%     timer:sleep(200),
    start_sessions_1().

start_sessions_1() ->
    
    S1 = #session_parameter{
                             id = heartbeat, 
                             port = 54321,  
                             senderCompId = "TEST1", targetCompId = "TEST", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 30, role = acceptor,max_reconnect = 10, reconnect_interval = 2, 
                             message_checks = #message_checks{check_msgSeqNum = true},
                             callback = {?MODULE, callback1}
                           },
    fixerl:start_session(S1),
    ok.

start_sessions_2() ->
    S = #session_parameter{
                             id = heartbeat1, 
                             host = localhost, port = 54321, max_reconnect = 10, reconnect_interval = 4, 
                             senderCompId = "TEST", targetCompId = "TEST1", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 30, role = initiator,
                             message_checks = #message_checks{check_msgSeqNum = true},
                             callback = {?MODULE, callback}
                           },
    fixerl:start_session(S),
    ok.

clean() ->
%%     stop_sessions(),
    application:stop(fixerl),
    application:stop(lager),
    application:stop(mnesia).

stop_sessions() -> 
    fixerl:stop_session(heartbeat1),
    fixerl:stop_session(heartbeat).

callback(_Id, _M) ->
    ok.

callback1(_Id, _M) ->
    ok.

