%% Author: Maxim Minin
%% Created: 23.06.2012
%% Description: TODO: Add description to tcp_connector
-module(tcp_connector).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/3, init/5]).

%%
%% API Functions
%%
-define(RECONNECT_TIMEOUT, 80).

start_link(Host, Port, AcceptorSup) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [self(), Host, Port, AcceptorSup, 0])}.

init(Parent, Host, Port, AcceptorSup, C) ->
    State = gen_tcp:connect(Host, Port, 
                         [
                         binary,
                         {packet, raw}, % no packaging
                         {reuseaddr, true}, % allow rebind without waiting
                         {exit_on_close, false},
                         {active, false}]
                                                       ),
    io:format("State: ~p~n", [State]),
    case State of
        {ok, Socket} ->
            {ok, APid} = supervisor:start_child(tcp_client_sup, [Socket]),
            erlang:monitor(process, APid),
            main_loop(Parent, Host, Port, AcceptorSup, 0);
        Error ->
            io:format("Error: ~p~n", [Error]),
            erlang:send_after(?RECONNECT_TIMEOUT, self(), reconnect),
            main_loop(Parent, Host, Port, AcceptorSup, C)
    end.

%%
%% Local Functions
%%
main_loop(_Parent, _Host, _Port, _AcceptorSup, C) when C > 8 ->
    exit(max_recconect);
main_loop(Parent, Host, Port, AcceptorSup, C) ->
    receive
        reconnect ->
            init(Parent, Host, Port, AcceptorSup, C+1);
        {'DOWN', Ref, Type, Object, Info} ->
            io:format("DOWN: ~p ~p ~p ~p~n", [Ref, Type, Object, Info]),
            exit(close_socket);
        stop ->
            ok;
        Other ->
            io:format("REASON FOR SOCKET EXIT: ~p~n", [Other]),
            %% internal error - something worth dying for
            exit({unexpected_message, Other})
    end.
