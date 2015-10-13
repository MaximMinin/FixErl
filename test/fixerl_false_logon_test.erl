%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_false_logon_test
-module(fixerl_false_logon_test).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("fixerl.hrl").

%%
%% Exported Functions
%%
-compile([export_all]).

-define(FIX_VERSION, 'FIX 4.2').
-define(ID_1, fixerl_false_logon_test_out).
-define(ID_2, fixerl_false_logon_test_in).

%%
%% API Functions
%%

fixerl_test_() ->
    {timeout, 6000, ?_assert(test_run())}.

%%
%% Local Functions
%%
test_run() ->
    setup(),
    timer:sleep(4000),
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
    application:start(fix_convertor),
    lager:set_loglevel(lager_console_backend, emergency),
    fixerl_mnesia_utils:init(),
    ok = fixerl:start(),
    start_sessions().

start_sessions() ->
    S1 = #session_parameter{
                             id = ?ID_1, 
                             port = 11115,  
                             max_reconnect = 10, reconnect_interval = 1, 
                             senderCompId = "TEST1", targetCompId = "TEST",
                             fix_version = ?FIX_VERSION,
                             heartbeatInterval = 30, role = acceptor,
                             callback = {?MODULE, callback}
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = ?ID_2, 
                             host = localhost, port = 11115,
                             max_reconnect = 10, reconnect_interval = 20, 
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
