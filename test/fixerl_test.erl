%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fixerl_test
-module(fixerl_test).

%%
%% Include files
%%
-include("fixerl.hrl").
%%
%% Exported Functions
%%
-export([
          test/0, callback/1
         ]).

%%
%% API Functions
%%
test() ->
%%lager:start(),
%%application:start(mnesia),
%%application:start(fixerl),
S1 = #session_parameter{
                             id = test1, 
                             port = 12345,  
                             senderCompId = <<"TEST1">>, targetCompId = <<"TEST">>, fix_version = "FIX_4_2",
                             heartbeatInterval = 30, role = acceptor,
                             callbackModule = {?MODULE, callback}
                           },
fixerl_root_sup:start_session(S1),
S = #session_parameter{
                             id = test, 
                             host = localhost, port = 12345, max_reconnect = 10, reconnect_interval = 20, 
                             senderCompId = <<"TEST">>, targetCompId = <<"TEST1">>, fix_version = "FIX_4_2",
                             heartbeatInterval = 30, role = initiator,
                             callbackModule = {?MODULE, callback}
                           },
fixerl_root_sup:start_session(S).

callback(M) ->
lager:info("MESSAGE IN CALLBACK: ~p", [M]).

%%
%% Local Functions
%%

