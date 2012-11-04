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
-export([start_link/5]).

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
start_link(Id, undefined, Port, undefined, undefined) ->
    supervisor:start_link({local, erlang:list_to_atom(lists:concat([Id, "_", ?MODULE]))},
      ?MODULE, {Port, Id});
start_link(Id, Host, Port, MaxReconnect, ReconnectTimeout) ->
    supervisor:start_link({local, erlang:list_to_atom(lists:concat([Id, "_", ?MODULE]))},
      ?MODULE, {Host, Port, MaxReconnect, ReconnectTimeout, Id}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init({Port, Id}) ->
    Name = tcp_name(tcp_acceptor_sup, Port),
    {ok, {{one_for_all, 10, 10},
          [{tcp_acceptor_sup, {tcp_acceptor_sup, start_link,
                               [Name, erlang:list_to_atom(lists:concat([Id, "_", tcp_client_sup]))]},
            transient, infinity, supervisor, [tcp_acceptor_sup]},
           {tcp_listener, {tcp_listener, start_link,
                           [Port, 1, Name]},
            transient, 100, worker, [tcp_listener]}]}};
init({Host, Port, MaxReconnect, ReconnectTimeout, Id}) ->
    Name = tcp_name(tcp_acceptor_sup, Host, Port),
    {ok, {{one_for_all, 10, 10},
          [{tcp_acceptor_sup, {tcp_acceptor_sup, start_link,
                               [Name]},
            transient, infinity, supervisor, [tcp_acceptor_sup]},
           {tcp_connector, {tcp_connector, start_link,
                           [Host, Port, erlang:list_to_atom(lists:concat([Id, "_", tcp_client_sup])), MaxReconnect, ReconnectTimeout]},
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
