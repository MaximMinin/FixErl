%% Author: Maxim Minin
%% Created: 28.10.2012
%% Description: TODO: Add description to fixerl_mnesia_utils
-module(fixerl_mnesia_utils).

%%
%% Include files
%%
-include("fixerl.hrl").
%%
%% Exported Functions
%%
-export([init/0]).

-export([ensure_mnesia_running/0, wait_for_tables/0, check_schema_integrity/0, create_tables/0]).
%%
%% API Functions
%%

init() ->
    mnesia:start(),
    ok = ensure_mnesia_running(),
    ok = ensure_mnesia_dir(),
    create_tables(),
    wait_for_tables(),
    ok = clear_temp_tables(),
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
    },
    {fix_in_messages, 
                    [
                        {type, set},
                        {disc_copies, [node()]},
                        {attributes, record_info(fields, fix_in_messages)}
                    ]
    },
    {fix_out_messages,  
                    [
                        {type, set},
                        {disc_copies, [node()]},
                        {attributes, record_info(fields, fix_out_messages)}
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


temp_tables() ->
    [fix_in_messages, fix_out_messages].

clear_temp_tables() ->
    {Date, _} = erlang:universaltime(),
    case mnesia:dirty_read(({last_startup_run, 1})) of
    [{last_startup_run, 1, LastDate}] ->
       case Date > LastDate of
           true ->
                   lists:map(fun(Tab) -> mnesia:clear_table(Tab)end, temp_tables()),
               Res = mnesia:transaction(fun () ->
                   mnesia:write({last_startup_run, 1, Date})
               end),
               io:format("CLEAR: ~p~n", [Res]),
               ok;
           false ->
               ok
       end;
    [] ->
        mnesia:transaction(fun () ->
                           mnesia:write({last_startup_run, 1, Date})
                           end),
        ok
    end,
    ok.


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
