%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_stop_test
-module(fixerl_stop_test).

%%
%% Include files
%%
-include("fixerl_test.hrl").


%%
%% Exported Functions
%%

-define(FIX_VERSION, 'FIX 5.0').
-define(ID_1, fixerl_stop_test_acceptor).
-define(ID_2, fixerl_stop_test_initiator).

%%
%% API Functions
%%

fixerl_stop_test_() ->
    {timeout, 60000, ?_assert(test_run())}.

%%
%% Local Functions
%%
test_run() ->
    setup(),
    start_session_1(),
    fixerl:stop_session(?ID_1),
    gen_tcp:listen(11111, []),
    start_session_2(),
    timer:sleep(100),
    fixerl:stop_session(?ID_2),
    timer:sleep(1500),
    R = whereis(?ID_1) == undefined
    andalso whereis(?ID_2) == undefined,
    clean(),
    R.

setup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    application:set_env(lager, error_logger_hwm, 500),
    lager:start(),
    lager:set_loglevel(lager_console_backend, emergency),
    fixerl_mnesia_utils:init(),
    ok = application:start(fixerl).

start_session_1() ->
    S1 = #session_parameter{
                             id = ?ID_1, 
                             port = 11111,  
                             senderCompId = "TEST1", targetCompId = "TEST",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 30, role = acceptor,
                             max_reconnect = 10, reconnect_interval = 1, 
                             message_checks = #message_checks{check_msgSeqNum = true},
                             callback = {?MODULE, callback1}, 
                             logon_callback = {?MODULE, logon_succeeded}
                           },
    fixerl:start_session(S1).

start_session_2() ->
    S = #session_parameter{
                             id = ?ID_2, 
                             host = localhost, port = 11111,
                             max_reconnect = 10, reconnect_interval = 1, 
                             senderCompId = "TEST", targetCompId = "TEST1",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = initiator, 
                             message_checks = #message_checks{check_msgSeqNum = false},
                             callback = {?MODULE, callback}, 
                             logon_callback = {?MODULE, logon_succeeded}
                           },
    
    fixerl:start_session(S).

clean() ->
    fixerl:stop_session(bla),
    application:stop(fixerl),
    application:stop(lager),
    application:stop(mnesia).

callback(_Id, _M) ->
    ok.

logon_succeeded()->
    ?DUMMY ! logon_succeeded.

wait_for_logon() ->
    receive
        logon_succeeded -> ok
    end.
