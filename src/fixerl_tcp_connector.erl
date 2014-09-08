%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created: 23.06.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fixerl_tcp_connector).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_link/5, init/7]).

%%
%% API Functions
%%

start_link(Host, Port, AcceptorSup, MaxReconnect, ReconnectTimeout) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [self(), Host, Port, AcceptorSup, 0, MaxReconnect, ReconnectTimeout])}.

init(Parent, Host, Port, AcceptorSup, C, MaxReconnect, ReconnectTimeout) ->
    State = gen_tcp:connect(Host, Port, 
                         [
                         binary,
                         {packet, raw}, % no packaging
                         {reuseaddr, true}, % allow rebind without waiting
                         {exit_on_close, false},
                         {active, false}]
                                                       ),
    case State of
        {ok, Socket} ->
            {ok, APid} = fixerl_tcp_client_sup:start_child(AcceptorSup, Socket),
            erlang:monitor(process, APid),
            main_loop(Parent, Host, Port, AcceptorSup, 0, MaxReconnect, ReconnectTimeout);
        Error ->
            lager:error("~p - ERROR: ~p~n", [?MODULE, Error]),
            erlang:send_after(ReconnectTimeout*1000, self(), reconnect),
            main_loop(Parent, Host, Port, AcceptorSup, C, MaxReconnect, ReconnectTimeout)
    end.

%%
%% Local Functions
%%
main_loop(_Parent, _Host, _Port, _AcceptorSup, C, MaxReconnect, _ReconnectTimeout) when C > MaxReconnect ->
    exit(max_reconnect);
main_loop(Parent, Host, Port, AcceptorSup, C, MaxReconnect, ReconnectTimeout) ->
    receive
        reconnect ->
            init(Parent, Host, Port, AcceptorSup, C+1, MaxReconnect, ReconnectTimeout);
        {'DOWN', Ref, Type, Object, Info} ->
            lager:info("DOWN: ~p ~p ~p ~p~n", [Ref, Type, Object, Info]),
            exit(normal);
        stop ->
            ok;
        Other ->
            lager:error("REASON FOR SOCKET EXIT: ~p~n", [Other]),
            %% internal error - something worth dying for
            exit({unexpected_message, Other})
    end.
