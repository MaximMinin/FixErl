%%% -------------------------------------------------------------------
%%% Author  : Maxim Minin
%%% Description :
%%%
%%% Created : 28.06.2012
%%% -------------------------------------------------------------------
-module(fixerl_sup).

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

start_link(#session_parameter{host=Host, port= Port}) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Host, Port]) ->
    AChild = {tcp_listener_sup,{tcp_listener_sup, start_link,[Host, Port]},
          permanent,2000,supervisor,[tcp_listener_sup]},
    BChild = {tcp_client_sup,{tcp_client_sup, start_link,[]},
          permanent,2000,supervisor,[tcp_client_sup]},
    {ok,{{one_for_all,0,1}, [AChild, BChild]}}.

