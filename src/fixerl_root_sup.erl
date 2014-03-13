%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created : 28.06.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fixerl_root_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("fixerl.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0, start_session/1, stop_session/1]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Func: start_link/0
%% Returns: {ok,  pid()} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% --------------------------------------------------------------------
%% @doc Starts the fix session
%%
%% @spec start_session(SessionParams::#session_parameter{}) ->
%%      {ok, pid()} | {error, Reason}
%%
%% @end
start_session(#session_parameter{id=Id}=S) ->
    lager:info("Start session: ~p",[S]),
    fixerl_mnesia_utils:create_table(Id),
    supervisor:start_child(?MODULE, [S]).

%% --------------------------------------------------------------------
%% @doc Stop the fix session
%%
%% @spec stop_session(SessionId::atom()) -> ok
%%
%% @end
%% --------------------------------------------------------------------
stop_session(SessionId) ->
    lager:info("Stop child: ~p - ~p",[SessionId, whereis(SessionId)]),
    supervisor:terminate_child(?MODULE, whereis(SessionId)).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    AChild = {fix_session_sup, {fix_session_sup, start_link,[]},
	      permanent,2000,supervisor,[fix_session_sup]},
    {ok,{{simple_one_for_one,0,1}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

