%%% -------------------------------------------------------------------
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created: 12.11.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fixerl).

%%
%% Exported Functions
%%
-export([start/0, start_session/1, stop_session/1, send/2]).

%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts the server
%%
%% @spec start() -> ok | {error, Reason}
%%
%% @end
%% --------------------------------------------------------------------
start() ->
    application:start(fixerl).

%% --------------------------------------------------------------------
%% @doc Starts the fix session
%%
%% @spec start_session(SessionParams::#session_parameter{}) ->
%%      {ok, pid} | {error, Reason}
%%
%% @end
%% --------------------------------------------------------------------
start_session(SessionParams) ->
    fixerl_root_sup:start_session(SessionParams).

%% --------------------------------------------------------------------
%% @doc Stop the fix session
%%
%% @spec stop_session(SessionId::atom()) -> ok
%%
%% @end
%% --------------------------------------------------------------------
stop_session(SessionId) ->
    fixerl_root_sup:stop_session(SessionId).

%% --------------------------------------------------------------------
%% @doc Send the message 
%%
%% @spec send(SessionId::atom(), Msg::tuple()) -> ok 
%%
%% @end
%% --------------------------------------------------------------------
send(SessionId, Msg) ->
    fix_gateway:send(SessionId, Msg).
