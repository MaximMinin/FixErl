%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_test
-module(fixerl_test).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-compile([export_all]).
%%
%% API Functions
%%

fixerl_4_2_proper_test_() ->
    {timeout, 6000, ?_assert(test_4_2_run())}.

fixerl_4_2_resend_proper_test_() ->
    {timeout, 6000, ?_assert(resend_test_4_2_run())}.

fixerl_5_0_resend_proper_test_() ->
    {timeout, 6000, ?_assert(resend_test_5_0_run())}.

fixerl_5_0_msq_seq_num_proper_test_() ->
    {timeout, 6000, ?_assert(msq_seq_num_test_5_0_run())}.
%%
%% Local Functions
%%
test_4_2_run() ->
    fix_4_2_properstatem:setup(),
    R = proper:quickcheck(proper:numtests(100, fix_4_2_properstatem:prop_master())),
    %% sleep for heartbeat test ...
    timer:sleep(5*1000*4),
    fix_4_2_properstatem:clean(),
    R.

resend_test_4_2_run() ->
    lager:start(),
    lager:set_loglevel(lager_console_backend, notice),
    R = proper:quickcheck(proper:numtests(100, fix_4_2_resend_properstatem:prop_master())),
    application:stop(lager),
    R.

resend_test_5_0_run() ->
    lager:start(),
    lager:set_loglevel(lager_console_backend, notice),
    R = proper:quickcheck(proper:numtests(100, fix_5_0_resend_properstatem:prop_master())),
    application:stop(lager),
    R.


msq_seq_num_test_5_0_run() ->
    lager:start(),
    lager:set_loglevel(lager_console_backend, notice),
    R = proper:quickcheck(proper:numtests(100, fix_5_0_msg_seq_num_properstatem:prop_master())),
    application:stop(lager),
    R.
