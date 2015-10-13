%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_heartbeat_test
-module(fixerl_heartbeat_test).

%%
%% Include files
%%
-include("fixerl_test.hrl").

-define(FIX_VERSION, 'FIX 4.2').
-define(ID_1, fixerl_heartbeat_test_out).
-define(ID_2, fixerl_heartbeat_test_in).

%%
%% API Functions
%%

fixerl_4_2_heartbeat_test_() ->
    {timeout, 6000, ?_assert(test_4_2_run())}.

%%
%% Local Functions
%%
test_4_2_run() ->
    setup(),
    %% sleep for heartbeat test ...
    timer:sleep(5*1000*4),
    R = whereis(?ID_1) == undefined
    andalso whereis(?ID_2) == undefined,
    clean(),
    R.

setup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()|nodes()]),
    mnesia:create_schema([node()|nodes()]),
    application:set_env(lager, error_logger_hwm, 500),
    lager:start(),
    application:start(fix_convertor),
    lager:set_loglevel(lager_console_backend, emergency),
    fixerl_mnesia_utils:init(),
    ok = fixerl:start(),
    start_sessions().

start_sessions() ->
    S1 = #session_parameter{
                             id = ?ID_1, 
                             port = 11114,  
                             senderCompId = "TEST1", targetCompId = "TEST",
                             fix_version = ?FIX_VERSION,
                             max_reconnect = 10, reconnect_interval = 60,
                             heartbeatInterval = 30, role = acceptor,
                             callback = {?MODULE, callback}
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = ?ID_2, 
                             host = localhost, port = 11114,
                             max_reconnect = 10, reconnect_interval = 60, 
                             senderCompId = "TEST", targetCompId = "TEST1", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = initiator,
                             callback = {?MODULE, callback}, start_seqnum = 10
                           },
    fixerl:start_session(S),
    ok.

clean() ->
    application:stop(fixerl),
    application:stop(lager),
    application:stop(mnesia).

callback(_Id, _M) ->
    ok.
