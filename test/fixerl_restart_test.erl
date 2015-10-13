%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_restart_test
-module(fixerl_restart_test).

%%
%% Include files
%%
-include("fixerl_test.hrl").

%%
%% Exported Functions
%%

-define(FIX_VERSION, 'FIX 5.0').
-define(ID_1, fixerl_restart_test_out).
-define(ID_2, fixerl_restart_test_in).

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
    fixerl:send(?ID_2, {bla})
    catch Err:F  -> ?LOG("ENDE ~p  ~p", [Err, F])
    end,
    timer:sleep(5000),
    R = whereis(?ID_1) =/= undefined
    andalso whereis(?ID_2) =/= undefined,
    clean(),
    R.

setup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    application:set_env(lager, error_logger_hwm, 500),
    lager:start(),
    application:start(fix_convertor),
    lager:set_loglevel(lager_console_backend, emergency),
    fixerl_mnesia_utils:init(),
    ok = application:start(fixerl),
    start_sessions().

start_sessions() ->
    start_sessions_2(),
    start_sessions_1().

start_sessions_1() ->
    
    S1 = #session_parameter{
                             id = ?ID_1, 
                             port = 11112,  
                             senderCompId = "RESTART1", targetCompId = "RESTART",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 30, role = acceptor,
                             max_reconnect = 10, reconnect_interval = 2, 
                             message_checks = #message_checks{check_msgSeqNum = true},
                             callback = {?MODULE, callback1}
                           },
    fixerl:start_session(S1),
    ok.

start_sessions_2() ->
    S = #session_parameter{
                             id = ?ID_2, 
                             host = localhost, port = 11112,
                             max_reconnect = 10, reconnect_interval = 4, 
                             senderCompId = "RESTART", targetCompId = "RESTART1",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 30, role = initiator,
                             message_checks = #message_checks{check_msgSeqNum = true},
                             callback = {?MODULE, callback}
                           },
    fixerl:start_session(S),
    ok.

clean() ->
    fixerl:stop_session(?ID_2),
    fixerl:stop_session(?ID_1),
    application:stop(fixerl),
    application:stop(lager),
    application:stop(mnesia).

callback(_Id, _M) ->
    ok.

callback1(_Id, _M) ->
    ok.

