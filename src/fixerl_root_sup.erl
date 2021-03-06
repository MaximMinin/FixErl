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
start_session({session_parameter, Id, Host, Port, MaxRec, RecInt, Role,
               Callback, CallbackMode, LogonCallback,
               FixVersion, StartSeqNum, HearbeatInterval,
               SenderCompId, TargetCompId, SkipByResend, Checks}) ->
    S = #session_parameter{ id = Id, host = Host, port = Port,
                            max_reconnect = MaxRec, reconnect_interval = RecInt,
                            role = Role, callback = Callback,
                            callback_mode = CallbackMode, logon_callback = LogonCallback,
                            fix_version = FixVersion, start_seqnum = StartSeqNum,
                            heartbeatInterval = HearbeatInterval, senderCompId = SenderCompId,
                            targetCompId = TargetCompId, skip_by_resend_request = SkipByResend,
                            message_checks = Checks},
    lager:info("Start session: ~p",[S]),
    supervisor:start_child(?MODULE, [S]);
start_session(#session_parameter{}=S) ->
    lager:info("Start session: ~p",[S]),
    supervisor:start_child(?MODULE, [S]).

%% --------------------------------------------------------------------
%% @doc Stop the fix session
%%
%% @spec stop_session(SessionId::atom()) -> ok
%%
%% @end
%% --------------------------------------------------------------------
stop_session(SessionId) ->
    lager:info("Stop session: ~p - ~p",[SessionId, whereis(SessionId)]),
    S = list_to_atom(lists:concat([SessionId, "_",fix_session_sup])),
    case whereis(S) of
        Pid when erlang:is_pid(Pid) ->
            R = supervisor:terminate_child(?MODULE, Pid),
            lager:info("Stop session :~p ~p", [SessionId, R]);
        _Else ->
            lager:error("Session not found: ~p ",[SessionId]),
            {error, not_found}
    end.

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
    {ok,{{simple_one_for_one,10,10}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

