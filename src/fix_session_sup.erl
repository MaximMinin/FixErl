%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created : 28.06.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fix_session_sup).

-behaviour(supervisor).

-include("fixerl.hrl").
%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, 
                         permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(#session_parameter{id = Id} = S) ->
    fixerl_mnesia_utils:create_table(Id),
    supervisor:start_link({local, erlang:list_to_atom(
                                    lists:concat([Id, "_",?MODULE]))}, 
                            ?MODULE, [S]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([S]) ->
    AChild = {fixerl_tcp_listener_sup,{fixerl_tcp_listener_sup, start_link,
                [S#session_parameter.id, 
                 S#session_parameter.host,
                 S#session_parameter.port, 
                 S#session_parameter.max_reconnect,
                 S#session_parameter.reconnect_interval]},
          permanent,2000,supervisor,[tcp_listener_sup]},
    BChild = {fixerl_tcp_client_sup,{fixerl_tcp_client_sup, start_link,[S]},
          permanent,2000,supervisor,[fixerl_tcp_client_sup]},
    CChild = {fixerl_stop_timer,{fixerl_stop_timer, start_link,
                                 [S#session_parameter.id, 
                                  S#session_parameter.reconnect_interval]},
              permanent,
              S#session_parameter.reconnect_interval*1000+1,
              worker,[fixerl_stop_timer]},
    {ok,{{one_for_all,
          S#session_parameter.max_reconnect,
          S#session_parameter.max_reconnect*S#session_parameter.reconnect_interval - 1},
    [AChild, BChild, CChild]}}.

