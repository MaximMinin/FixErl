%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description: TODO: Add description to fixerl_mnesia_utils
%%%
%%% Created: 28.10.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fixerl_mnesia_utils).

%%
%% Include files
%%
-include("fixerl.hrl").
%%
%% Exported Functions
%%
-export([init/0]).

-export([ensure_mnesia_running/0, 
         create_table/1,
         get_tables_name/1]).
%%
%% API Functions
%%

init() ->
    mnesia:start(),
    ok = ensure_mnesia_running(),
    ok = ensure_mnesia_dir(),
    ok.

%%
%% Local Functions
%%

create_table(SessionId) ->
    Ts = get_tables_name(SessionId),
    lists:map(fun(TableName) ->
        case tables_exist(TableName) of
            false ->
                TabDef = get_table_def(),
                {atomic, ok} = mnesia:create_table(TableName, TabDef),
                {atomic, ok} = mnesia:change_table_copy_type(TableName,
                                                             node(),
                                                             disc_copies),
                lists:map(fun(Node) -> mnesia:add_table_copy(TableName,
                                                             Node,
                                                             disc_copies)
                          end, nodes()),
                wait_for_table(TableName);
        true -> ok
        end end, Ts).

get_tables_name(SessionId)->
    {{Y,M,D},_}=erlang:universaltime(),
    [list_to_atom(lists:concat([SessionId,"_in_",Y,"_",M,"_",D])),
    list_to_atom(lists:concat([SessionId,"_out_",Y,"_",M,"_",D]))].

tables_exist(TableName) ->
    try
        mnesia:table_info(TableName, [all]),
        true
    catch _:_ ->
              false
    end.

get_table_def()->
     [
         {type, set},
         {attributes, [number, message]}
     ].

wait_for_table(Name) -> 
           case mnesia:wait_for_tables([Name], 30000) of
                ok -> ok;
                {timeout, BadTab} ->
                    throw({error, {timeout_waiting_for_table, BadTab}});
                {error, Reason} ->
                    throw({error, {failed_waiting_for_table, Reason}})
            end.

dir() -> mnesia:system_info(directory).
    
ensure_mnesia_dir() ->
    MnesiaDir = dir() ++ "/",
    case filelib:ensure_dir(MnesiaDir) of
        {error, Reason} ->
            throw({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
        ok -> ok
    end.

ensure_mnesia_running() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        no -> throw({error, mnesia_not_running})
    end.
