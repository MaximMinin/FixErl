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

%%
%% Local Functions
%%
test_4_2_run() ->
    fix_4_2_properstatem:setup(),
    R = proper:quickcheck(proper:numtests(100, fix_4_2_properstatem:prop_master())),
    %% sleep for heartbeat test ...
    timer:sleep(30*1000*4),
    fix_4_2_properstatem:clean(),
    R.
