%%% -------------------------------------------------------------------
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created: 12.11.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fixerl).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("fixerl.hrl").

%%
%% Exported Functions
%%
-export([start/0, start_session/1, stop_session/1, reset_session/1,
         send/2, send/3]).

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
%%      {ok, pid()} | {error, Reason}
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
    fixerl_stop_timer:deactivate_timeout(SessionId),
    fixerl_root_sup:stop_session(SessionId).

%% --------------------------------------------------------------------
%% @doc stop session, clear session table and start session.
%%
%% @spec stop_session(SessionId::atom()) -> ok
%%
%% @end
%% --------------------------------------------------------------------
reset_session(SessionId) ->
    P = fix_worker:get_session_parameter(SessionId),
    fixerl_stop_timer:deactivate_timeout(SessionId),
    fixerl_root_sup:stop_session(SessionId),
    fixerl_mnesia_utils:clear_table(SessionId),
    fixerl_root_sup:start_session(P).

%% --------------------------------------------------------------------
%% @doc Send the message 
%%
%% @spec send(SessionId::atom(), Msg::tuple()) -> ok 
%%
%% @end
%% --------------------------------------------------------------------
send(SessionId, Msg) ->
    fix_gateway:send(SessionId, Msg).

%% --------------------------------------------------------------------
%% @doc Send the message 
%%
%% @spec send(SessionId::atom(), 
%%             NotStandardPart::binary(),
%%             Msg::tuple()) -> ok 
%%
%% @end
%% --------------------------------------------------------------------
send(SessionId, Msg, NotStandardPart) ->
    fix_gateway:send(SessionId, Msg, NotStandardPart).
