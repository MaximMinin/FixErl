%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created : 28.06.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fixerl_tcp_listener_sup).

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
start_link(Id, undefined, Port, _MaxReconnect, _ReconnectTimeout) ->
    supervisor:start_link({local, 
        list_to_atom(lists:concat([Id, "_", ?MODULE]))},
        ?MODULE, {Port, Id});
start_link(Id, Host, Port, MaxReconnect, ReconnectTimeout) ->
    supervisor:start_link({local, 
        list_to_atom(lists:concat([Id, "_", ?MODULE]))},
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
    Name = tcp_name(fixerl_tcp_acceptor_sup, Port),
    {ok, {{one_for_all, 10, 10},
          [{fixerl_tcp_acceptor_sup, {fixerl_tcp_acceptor_sup, start_link,
                               [Name, get_sup_id(Id)]},
            transient, infinity, supervisor, [fixerl_tcp_acceptor_sup]},
           {fixerl_tcp_listener, {fixerl_tcp_listener, start_link,
                           [Port, 1, Name]},
            transient, 100, worker, [fixerl_tcp_listener]}]}};
init({Host, Port, MaxReconnect, ReconnectTimeout, Id}) ->
    Name = tcp_name(fixerl_tcp_acceptor_sup, Host, Port),
    {ok, {{one_for_all, 10, 10},
          [{fixerl_tcp_acceptor_sup, {fixerl_tcp_acceptor_sup, start_link,
                               [Name]},
            transient, infinity, supervisor, [fixerl_tcp_acceptor_sup]},
           {fixerl_tcp_connector, {fixerl_tcp_connector, start_link,
                           [Host, Port, get_sup_id(Id),
                            MaxReconnect, ReconnectTimeout]},
            transient, infinity, worker, [fixerl_tcp_connector]}
          ]
         }
    }.


%%
%% Local Functions
%%
get_sup_id(Id) ->
    list_to_atom(lists:concat([Id, "_", fixerl_tcp_client_sup])).

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
