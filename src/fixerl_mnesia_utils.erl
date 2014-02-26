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

-export([ensure_mnesia_running/0, wait_for_tables/0, 
         check_schema_integrity/0, create_tables/0,
         create_table/1, get_tables_name/1]).
%%
%% API Functions
%%

init() ->
    mnesia:start(),
    ok = ensure_mnesia_running(),
    ok = ensure_mnesia_dir(),
    create_tables(),
    wait_for_tables(),
    check_schema_integrity(),
    ok.

%%
%% Local Functions
%%
table_definitions() ->
    [
    {last_startup_run, 
                    [
                        {type, set},
                        {disc_copies, [node()]},
                        {attributes, record_info(fields, last_startup_run)}
                    ]
    }
    ].

create_tables() ->
    lists:foreach(fun ({Tab, TabArgs}) ->
                          case mnesia:create_table(Tab, TabArgs) of
                              {atomic, ok} -> ok;
                              {aborted, Reason} ->
                                  throw({error, {table_creation_failed,
                                                 Tab, TabArgs, Reason}})
                          end
                  end,
                  table_definitions()),
    ok.

create_table(SessionId) ->
    {InTableName, OutTableName} = get_tables_name(SessionId),
    case tables_exist(InTableName, OutTableName) of
        false ->
            TabDef = get_table_def(),
            {atomic, ok} = mnesia:create_table(InTableName, TabDef),
            {atomic, ok} = mnesia:create_table(OutTableName, TabDef);
        true -> ok
    end.

get_tables_name(SessionId)->
    {{Y,M,D},_}=erlang:universaltime(),
    {list_to_atom(lists:concat([SessionId,"_in_",Y,"_",M,"_",D])),
    list_to_atom(lists:concat([SessionId,"_out_",Y,"_",M,"_",D]))}.

tables_exist(InTableName, OutTableName) ->
    try
        mnesia:table_info(InTableName, [all]),
        mnesia:table_info(OutTableName, [all]),
        true
    catch _:_ ->
              false
    end.

get_table_def()->
     [
         {type, set},
         %%{disc_copies, [node()]},
         {attributes, [number, message]}
     ].

wait_for_tables() -> 
           case mnesia:wait_for_tables(table_names(), 30000) of
                ok -> ok;
                {timeout, BadTabs} ->
                    throw({error, {timeout_waiting_for_tables, BadTabs}});
                {error, Reason} ->
                    throw({error, {failed_waiting_for_tables, Reason}})
            end.

check_schema_integrity() ->
    case catch [mnesia:table_info(Tab, version) || Tab <- table_names()] of
        {'EXIT', Reason} -> {error, Reason};
        _ -> ok
    end.

table_names() ->
    [Tab || {Tab, _} <- table_definitions()].

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
