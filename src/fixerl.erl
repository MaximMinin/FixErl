%% Author: Maxim Minin
%% Created: 12.11.2012
%% Description: TODO: Add description to fixerl
-module(fixerl).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, start_session/1, stop_session/1, send/2]).

start() ->
    application:start(fixerl).

start_session(SessionParams) ->
    fixerl_root_sup:start_session(SessionParams).

stop_session(SessionId) ->
    fixerl_root_sup:stop_session(SessionId).

send(SessionId, Msg) ->
    fix_gateway:send(SessionId, Msg).
