%%% -------------------------------------------------------------------
%%% Author  : Maxim Minin
%%% Description :
%%%
%%% Created : 28.06.2012
%%% -------------------------------------------------------------------
-module(fix_session_sup).

-behaviour(supervisor).

-include("fixerl.hrl").
%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(#session_parameter{id = Id} = S) ->
    supervisor:start_link({local, erlang:list_to_atom(lists:concat([Id, "_",?MODULE]))}, ?MODULE, [S]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([S]) ->
    AChild = {tcp_listener_sup,{tcp_listener_sup, start_link,[ 
                                                               S#session_parameter.id, 
                                                               S#session_parameter.host, 
                                                               S#session_parameter.port,
                                                               S#session_parameter.max_reconnect,
                                                               S#session_parameter.reconnect_interval
                                                             ]},
          permanent,2000,supervisor,[tcp_listener_sup]},
    BChild = {tcp_client_sup,{tcp_client_sup, start_link,[S]},
          permanent,2000,supervisor,[tcp_client_sup]},
    {ok,{{one_for_all,0,1}, [AChild, BChild]}}.

