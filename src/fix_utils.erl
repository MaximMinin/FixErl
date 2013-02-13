%%% -------------------------------------------------------------------
%%% @private
%%% @author  : Maxim Minin
%%% @doc
%%% Description : @TODO
%%%
%%% Created : 28.06.2012
%%% @end
%%% -------------------------------------------------------------------
-module(fix_utils).

%%
%% Include files
%%
-include("FIX_4_2.hrl").
%%
%% Exported Functions
%%
-export([get_logon/2, get_heartbeat/2, get_heartbeat/1,
         get_numbers/1, getNow/0, getNow/1, getUniq/0,
         check_logon/3, get_logout/2]).

%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Checks the logon message
%%
%% @spec get_logon(Logon::#logon{},
%%                 SenderCompID::binary(), 
%%                 TargetCompID::binary()) -> ok|nok
%% @end
%% --------------------------------------------------------------------
check_logon(#logon{standardHeader = #standardHeader{
                senderCompID = TargetCompID, 
                targetCompID = SenderCompID}},
            SenderCompID, TargetCompID) -> ok;
check_logon(_,_,_) -> nok.


%% --------------------------------------------------------------------
%% @doc Gets the logout message
%%
%% @spec get_logout(SenderCompID::binary(), 
%%                 TargetCompID::binary()) -> #logout{}
%% @end
%% --------------------------------------------------------------------
get_logout(SenderCompID, TargetCompID) ->
    #logout{standardHeader = #standardHeader{
                msgType = logout,
                sendingTime = ?MODULE:getNow(),
                senderCompID = SenderCompID, 
                targetCompID = TargetCompID},
           standardTrailer = #standardTrailer{}}.


%% --------------------------------------------------------------------
%% @doc Gets the logon message
%%
%% @spec get_logon(SenderCompID::binary(), 
%%                 TargetCompID::binary()) -> #logon{}
%% @end
%% --------------------------------------------------------------------
get_logon(SenderCompID, TargetCompID) ->
    #logon{standardHeader = #standardHeader{
                msgType = logon,
                sendingTime = ?MODULE:getNow(),
                senderCompID = SenderCompID, 
                targetCompID = TargetCompID},
           standardTrailer = #standardTrailer{},
           repeatingReg_logon_384 = [#repeatingReg_logon_384{}],
           heartBtInt = 30}.

%% --------------------------------------------------------------------
%% @doc Gets the heartbeat message
%%
%% @spec get_heartbeat(SenderCompID::binary(), 
%%                     TargetCompID::binary()) -> #hearbeat{}
%% @end
%% --------------------------------------------------------------------
get_heartbeat(SenderCompID, TargetCompID)->
    #heartbeat{testReqID = ?MODULE:getUniq(),
               standardHeader = #standardHeader{
                    msgType = heartbeat,
                    sendingTime = ?MODULE:getNow(),
                    senderCompID = SenderCompID,
                    targetCompID = TargetCompID},
                standardTrailer = #standardTrailer{}}.

%% --------------------------------------------------------------------
%% @doc Gets the heartbeat message
%%
%% @spec get_heartbeat(TestRequest::#testRequest{}) -> #hearbeat{}
%%
%% @end
%% --------------------------------------------------------------------
get_heartbeat(#testRequest{standardHeader = #standardHeader{
                                targetCompID = TargetCompID,
                                senderCompID = SenderCompID},
                           testReqID = TestReqID})->
    #heartbeat{testReqID = TestReqID,
               standardHeader = #standardHeader{
                    msgType = heartbeat,
                    sendingTime = ?MODULE:getNow(),
                    senderCompID = TargetCompID,
                    targetCompID = SenderCompID},
                standardTrailer = #standardTrailer{}}.

%% --------------------------------------------------------------------
%% @doc Gets the list of message numbers to be resend
%%
%% @spec get_numbers(ResendRequest::#resendRequest{}) -> [integer()]
%%
%% @end
%% --------------------------------------------------------------------
get_numbers(#resendRequest{beginSeqNo=Start, endSeqNo = End})->
    lists:seq(Start, End).
    
%% --------------------------------------------------------------------
%% @doc Gets timestamp (now + AddTimeInSec) in fix format 
%% (yyyyMMdd-hh:mm:ss)
%%
%% @spec getNow(AddTimeInSec::integer()) -> binary()
%%
%% @end
%% --------------------------------------------------------------------
getNow(AddTimeInSec) ->
    AddSec = AddTimeInSec rem 60,
    AddMin = AddTimeInSec div 60,
    {{Year, Month, Day}, {Hour, Minute, Second}} = 
        erlang:universaltime(),
    list_to_binary(lists:concat([integer_to_list(Year),
                                 getTwoDigits(Month),
                                 getTwoDigits(Day),
                                 "-",
                                 getTwoDigits(Hour),
                                 ":",
                                 getTwoDigits(Minute+AddMin),
                                 ":",
                                 getTwoDigits(Second+AddSec)])).

%% --------------------------------------------------------------------
%% @doc Gets timestamp in fix format (yyyyMMdd-hh:mm:ss)
%%
%% @spec getNow() -> binary()
%%
%% @end
%% --------------------------------------------------------------------
getNow() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = 
        erlang:universaltime(),
    list_to_binary(lists:concat([integer_to_list(Year),
                                 getTwoDigits(Month),
                                 getTwoDigits(Day),
                                 "-",
                                 getTwoDigits(Hour),
                                 ":",
                                 getTwoDigits(Minute),
                                 ":",
                                 getTwoDigits(Second)])).

%% --------------------------------------------------------------------
%% @doc Gets uniq id for fix message
%%
%% @spec getUniq() -> binary()
%%
%% @end
%% --------------------------------------------------------------------
getUniq () ->
    {MegaSecs, Secs, MicroSecs} = now(),
    list_to_binary(lists:concat([MegaSecs,Secs,MicroSecs])).


    
%% ====================================================================
%% Local Functions
%% ====================================================================
getTwoDigits(Int) when Int < 10 ->
    lists:concat(["0",Int]);
getTwoDigits(Int) ->
    erlang:integer_to_list(Int).

