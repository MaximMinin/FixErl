%%% -------------------------------------------------------------------
%%% Author  : Maxim Minin
%%% Description :
%%%
%%% Created : 28.06.2012
%%% -------------------------------------------------------------------
-module(tcp_client_sup).

-behaviour(supervisor).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("fixerl.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Session) ->
    supervisor:start_link({local, erlang:list_to_atom(lists:concat([Session#session_parameter.id, "_", ?MODULE]))}, ?MODULE, [Session]).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([Session]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{tcp_client, {tcp_reader,start_link,[Session]},
            temporary, brutal_kill, worker, [tcp_reader]}]}}.
