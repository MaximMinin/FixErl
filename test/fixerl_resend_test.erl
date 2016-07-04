%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_resend_test
-module(fixerl_resend_test).

%%
%% Include files
%%
-include("fixerl_test.hrl").
-include_lib("fix_convertor/include/FIX_4_2.hrl").

-define(FIX_VERSION, 'FIX 4.2').
-define(ID_1, fixerl_resend_test_out).
-define(ID_2, fixerl_resend_test_in).

%%
%% API Functions
%%

fixerl_resend_test_() ->
    {timeout, 2000, ?_assertEqual(3, test_resend_run())}.

%%
%% Local Functions
%%
test_resend_run() ->
    setup(),
    timer:sleep(500),
    fix_gateway:save(?ID_1, #massQuote{quoteReqID="123"}),
    fixerl:send(?ID_1, fix_utils:get_heartbeat(?FIX_VERSION, "TEST", "TEST1")),
    timer:sleep(500),
    MsgNumIn = fix_worker:get_message_count(?ID_2),
    MsgNumIn.

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
                             port = 11134,  
                             senderCompId = "TEST1", targetCompId = "TEST",
                             fix_version = ?FIX_VERSION,
                             max_reconnect = 10, reconnect_interval = 60,
                             heartbeatInterval = 30, role = acceptor,
                             callback = {?MODULE, callback}
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = ?ID_2, 
                             host = localhost, port = 11134,
                             max_reconnect = 10, reconnect_interval = 60, 
                             senderCompId = "TEST", targetCompId = "TEST1", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 30, role = initiator,
                             message_checks = #message_checks{check_msgSeqNum = true},
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
