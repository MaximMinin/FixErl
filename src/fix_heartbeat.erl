%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created: 28.06.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fix_heartbeat).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_heartbeat/3]).
%%
%% API Functions
%%
start_heartbeat(Sock, Pid, TimeoutSec) ->
    Parent = self(),
    spawn_link(fun () -> heartbeater(Sock, TimeoutSec * 1000,
                                     recv_oct, 1,
                                     fun () ->
                                             Parent ! timeout,
                                             stop
                                     end,
                                     erlang:monitor(process, Parent)) end),
    spawn_link(fun () -> heartbeater(Sock, TimeoutSec * 1000 div 2,
                                     send_oct, 0,
                                     fun () ->
                                             catch fix_gateway:send_heartbeat(Pid),
                                             continue
                                     end,
                                     erlang:monitor(process, Parent)) end),
    ok.
%%
%% Local Functions
%%
y(X) ->
    F = fun (P) -> X(fun (A) -> (P(P))(A) end) end,
    F(F).

heartbeater(Sock, TimeoutMillisec, StatName, Threshold, Handler, MonitorRef) ->
    Heartbeat =
        fun (F) ->
                fun ({StatVal, SameCount}) ->
                        receive
                            {'DOWN', MonitorRef, process, _Object, _Info} -> 
                                 ok;
                            Other -> 
                                exit({unexpected_message, Other})
                        after TimeoutMillisec ->
                                case inet:getstat(Sock, [StatName]) of
                                    {ok, [{StatName, NewStatVal}]} ->
                                        if NewStatVal =/= StatVal ->
                                                F({NewStatVal, 0});
                                           SameCount < Threshold ->
                                                F({NewStatVal, SameCount + 1});
                                           true ->
                                                case Handler() of
                                                    stop     -> ok;
                                                    continue -> F({NewStatVal, 0})
                                                end
                                        end;
                                    {error, einval} ->
                                        ok;
                                    {error, Reason} ->
                                        exit({cannot_get_socket_stats, Reason})
                                end
                        end
                end
        end,
    (y(Heartbeat))({0, 0}).
