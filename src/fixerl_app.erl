%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% @end
%%% -------------------------------------------------------------------
-module(fixerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    R = fixerl_root_sup:start_link(),
    case application:get_env(fixerl, sessions) of
        undefined -> ok;
        {ok, []} -> ok;
        {ok, Sessions} ->
            lists:map(fun(S) ->
                fixerl_root_sup:start_session(S) end, Sessions)
    end,
    R.
stop(_State) ->
    ok.
