%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created : 28.06.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fixerl_tcp_client_sup).

-behaviour(supervisor).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("fixerl.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1, start_child/2]).

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
    supervisor:start_link({local, 
                           list_to_atom(
                            lists:concat([Session#session_parameter.id, 
                                          "_", ?MODULE]))}, 
                          ?MODULE, [Session]).

start_child(Id, Arg) ->
    lager:info("STARTE TCP READER PID: ~p ", [whereis(Id)]),
    lager:info("STARTE TCP READER: ~p ~p", [Id, Arg]),
    supervisor:start_child(Id, [Arg]).

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
          [{fixerl_tcp_reader, {fixerl_tcp_reader,start_link,[Session]},
            permanent, 10, worker, [fixerl_tcp_reader]}]}}.
