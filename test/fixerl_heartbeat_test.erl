%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_heartbeat_test
-module(fixerl_heartbeat_test).

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
    clean(),
    true.

setup() ->
    mnesia:stop(),
    mnesia:delete_schema([node()|nodes()]),
    mnesia:create_schema([node()|nodes()]),
    lager:start(),
    lager:set_loglevel(lager_console_backend, notice),
    fixerl_mnesia_utils:init(),
    ok = fixerl:start(),
    start_sessions().

start_sessions() ->
    S1 = #session_parameter{
                             id = heartbeat, 
                             port = 54321,  
                             senderCompId = "TEST1", targetCompId = "TEST", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 30, role = acceptor,
                             callback = {?MODULE, callback1}
                           },
    fixerl:start_session(S1),
    S = #session_parameter{
                             id = heartbeat1, 
                             host = localhost, port = 54321, max_reconnect = 10, reconnect_interval = 20, 
                             senderCompId = "TEST", targetCompId = "TEST1", fix_version = ?FIX_VERSION,
                             heartbeatInterval = 5, role = initiator,
                             callback = {?MODULE, callback}, start_seqnum = 10
                           },
    fixerl:start_session(S),
    ok.

clean() ->
    stop_sessions(),
    application:stop(fixerl),
    application:stop(lager).

stop_sessions() -> 
    fixerl:stop_session(test),
    fixerl:stop_session(test1).

callback(_Id, _M) ->
    ok.

callback1(_Id, _M) ->
    ok.

