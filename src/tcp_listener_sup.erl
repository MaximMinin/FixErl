%%% -------------------------------------------------------------------
%%% Author  : Maxim Minin
%%% Description :
%%%
%%% Created : 28.06.2012
%%% -------------------------------------------------------------------
-module(tcp_listener_sup).

-behaviour(supervisor).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/1, start_link/2]).

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
start_link(Port) ->
    supervisor:start_link({local, ?MODULE},
      ?MODULE, {Port}).

start_link(Host, Port) ->
    supervisor:start_link({local, ?MODULE},
      ?MODULE, {Host, Port}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init({Port}) ->
    Name = tcp_name(tcp_acceptor_sup, Port),
    {ok, {{one_for_all, 10, 10},
          [{tcp_acceptor_sup, {tcp_acceptor_sup, start_link,
                               [Name]},
            transient, infinity, supervisor, [tcp_acceptor_sup]},
           {tcp_listener, {tcp_listener, start_link,
                           [Port, 1, Name]},
            transient, 100, worker, [tcp_listener]}]}};
init({Host, Port}) ->
    Name = tcp_name(tcp_acceptor_sup, Host, Port),
    io:format("ACCEPTOR: ~p~n", [Name]),
    {ok, {{one_for_all, 10, 10},
          [{tcp_acceptor_sup, {tcp_acceptor_sup, start_link,
                               [Name]},
            transient, infinity, supervisor, [tcp_acceptor_sup]},
           {tcp_connector, {tcp_connector, start_link,
                           [Host, Port, Name]},
            transient, infinity, worker, [tcp_connector]}
          ]
         }
    }.


%%
%% Local Functions
%%
tcp_name(Prefix, Port)
  when is_number(Port) ->
    list_to_atom(
      lists:flatten(
        io_lib:format("~w_:~w",
                      [Prefix, Port]))).
tcp_name(Prefix, Host, Port)
  when is_number(Port) ->
    list_to_atom(
      lists:flatten(
        io_lib:format("~w_~w:~w",
                      [Prefix, Host, Port]))).
