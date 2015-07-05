%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_logout_test
-module(fixerl_logout_test).

%%
%% Include files
%%
-include("fixerl_test.hrl").

-define(FIX_VERSION, 'FIX 4.2').
-define(ID_1, fixerl_logout_test_out).
-define(ID_2, fixerl_logout_test_in).

fixerl_test_() ->
    {timeout, 6000, ?_assert(test_run())}.

%%
%% Local Functions
%%
test_run() ->
    setup(),
    timer:sleep(1000),
%%     fix_gateway:resend(?ID_1, <<"todo",1,"10=122",1>>),
%%     fix_gateway:resend(?ID_1, <<"todo=todo",1,"10=122",1>>),
%%     fix_gateway:resend(?ID_1, <<"8=FIX.4.2",1,"9=97",1,"35=X",1,
%%                               "49=SNDR",1,"56=TRGT",1,"34=2",1,"52=20110802-10:00:00",1,
%%                               "268=2",1,
%%                               "279=1",1,"280=0001",1,"270=10",1,
%%                               "279=1",1,"280=0002",1,"270=11",1,
%%                               "10=110",1,"">>),
%%     fix_gateway:resend(?ID_1, {todo}),
    fixerl:send(?ID_1, fix_utils:get_logout(?FIX_VERSION, "PONG", "PING")),
    timer:sleep(3000),
    R = whereis(?ID_1) =/= undefined
    andalso whereis(?ID_2) == undefined,
    clean(),
    true.

setup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    application:set_env(lager, error_logger_hwm, 500),
    lager:start(),
    lager:set_loglevel(lager_console_backend, emergency),
    fixerl_mnesia_utils:init(),
    ok = fixerl:start(),
    start_sessions().

start_sessions() ->
    S1 = #session_parameter{
                             id = ?ID_1, 
                             port = 11113,  
                             senderCompId = "PONG", targetCompId = "PING", 
                             fix_version = ?FIX_VERSION,
                             max_reconnect = 10, reconnect_interval = 1,
                             heartbeatInterval = 30, role = acceptor,
                             callback = {?MODULE, callback1}
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = ?ID_2, 
                             host = localhost, port = 11113,
                             max_reconnect = 10, reconnect_interval = 2, 
                             senderCompId = "PING", targetCompId = "PONG",
                              fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = initiator,
                             callback = {?MODULE, callback}
                           },
    fixerl:start_session(S),
    ok.

clean() ->
    application:stop(fixerl),
    application:stop(lager),
    application:stop(mnesia).

callback(_Id, _M) ->
    ok.

callback1(_Id, _M) ->
    ok.

