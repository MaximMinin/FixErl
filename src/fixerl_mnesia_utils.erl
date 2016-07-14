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
         create_table/1, clear_table/1,
         get_tables_name/1,
		 delete_old_tables/2]).
%%
%% API Functions
%%

clear_table(SessionId) ->
    Ts = get_tables_name(SessionId),
    mnesia:clear_table(Ts).

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
                try
                    {atomic, ok} = mnesia:create_table(TableName, TabDef),
                    {atomic, ok} = mnesia:change_table_copy_type(TableName,
                                                             node(),
                                                             disc_copies),
                    lists:map(fun(Node) -> mnesia:add_table_copy(TableName,
                                                             Node,
                                                             disc_copies)
                          end, nodes()),
                    wait_for_table(TableName)
                catch _:_ -> ok end;
        true -> ok
        end end, Ts).

get_tables_name(SessionId)->
    {{Y,M,D},_}=erlang:universaltime(),
    [list_to_atom(lists:concat([SessionId,"_in_",Y,"_",M,"_",D])),
    list_to_atom(lists:concat([SessionId,"_out_",Y,"_",M,"_",D]))].

delete_old_tables(SessionId, Days) ->
	[Table || Table <- mnesia:system_info(tables), delete(Table, Table, SessionId, Days) /= ok].

delete(Name, Table, SessionId, Days) when is_atom(Table)->
	delete(Name, erlang:atom_to_list(Table), SessionId, Days);
delete(Name, Table, SessionId, Days) when is_atom(SessionId)->
	delete(Name, Table, erlang:atom_to_list(SessionId), Days);
delete(Name, Table, SessionId, Days) ->
	case string:tokens(Table, "_") of
		[SessionId,_,Year,Month,Day] ->
			A = calendar:date_to_gregorian_days(list_to_integer(Year), 
												list_to_integer(Month),
												list_to_integer(Day)),
			{{Y,M,D},_} = erlang:universaltime(), 
			B = calendar:date_to_gregorian_days(Y,M,D),
			case (B - A) > Days of
				true -> 
					{atomic, ok} = mnesia:delete_table(Name),
					Name;
				_-> ok
			end;
		_ -> 
			ok
	end.

tables_exist(TableName) ->
    try
        mnesia:table_info(TableName, all),
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
