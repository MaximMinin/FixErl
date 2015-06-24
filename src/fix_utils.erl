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

%%
%% Exported Functions
%%
-export([is_session_msg/2, get_sequence_reset/4,
         get_logon/3, get_logout/3,
         check_logon/4, 
         get_heartbeat/3, get_heartbeat/2,
         get_numbers/2, get_seq_number/2,
         getNow/0, getNow/1, getUniq/0,
         get_resend_request/5]).

%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Checks the logon message
%%
%% @spec is_session_msg(FixVersion::#fix_version{},
%%                 Message::binary()) -> boolean()
%% @end
%% --------------------------------------------------------------------
is_session_msg(FixVersion, Message) ->
    {Record, _} = fix_convertor:fix2record(Message, FixVersion),
    case erlang:element(1, Record) of
        logon -> true;
        testRequest -> true;
        heartbeat -> true;
        logout -> true;
        resendRequest -> true;
        _ -> false
    end.
%% --------------------------------------------------------------------
%% @doc Checks the logon message
%%
%% @spec get_logon(Logon::#logon{},
%%                 SenderCompID::binary(), 
%%                 TargetCompID::binary()) -> ok|nok
%% @end
%% --------------------------------------------------------------------
check_logon(FixVersion, Logon,
            SenderCompID, TargetCompID) -> 
    Utils = fix_convertor:get_util_module(FixVersion),
    
    Header = erlang:element(2, Logon),
    D = Utils:get_record_def(standardHeader),
    P1 = find_first(targetCompID, D),
    TargetId = erlang:element(P1, Header),
    P2 = find_first(senderCompID, D),
    SenderID = erlang:element(P2, Header),
    case TargetId == SenderCompID andalso
             SenderID == TargetCompID of
        true ->
            ok;
        false ->
            nok
    end.

%% --------------------------------------------------------------------
%% @doc Gets the sequenceReset message
%%
%% @spec get_sequence_reset(FixVersion::, fix_version(),
%%                          SenderCompID::binary(), 
%%                          TargetCompID::binary(),
%%                          NewSeqNo::int()) -> #sequenceReset{}
%% @end
%% --------------------------------------------------------------------
get_sequence_reset(FixVersion, SenderCompID, TargetCompID, NewSeqNo) ->
    Utils = fix_convertor:get_util_module(FixVersion),
    L = Utils:getRecord(sequenceReset),
    H = Utils:getRecord(standardHeader),
    T = Utils:getRecord(standardTrailer),
    R = 
    Utils:setFieldInRecord(sequenceReset, gapFillFlag,
    Utils:setFieldInRecord(sequenceReset, newSeqNo,
    Utils:setFieldInRecord(sequenceReset, standardHeader,
                           Utils:setFieldInRecord(sequenceReset, 
                                                  standardTrailer, L, T),
    Utils:setFieldInRecord(standardHeader, targetCompID, 
    Utils:setFieldInRecord(standardHeader, sendingTime, 
    Utils:setFieldInRecord(standardHeader, msgType, 
    Utils:setFieldInRecord(standardHeader, senderCompID, H, SenderCompID),
                           sequenceReset), ?MODULE:getNow()), TargetCompID)),
                           NewSeqNo+1), gapFillMessage),
    fix_convertor:set_msg_seqnum(R, NewSeqNo, FixVersion).

%% --------------------------------------------------------------------
%% @doc Gets the logout message
%%
%% @spec get_logout(SenderCompID::binary(), 
%%                 TargetCompID::binary()) -> #logout{}
%% @end
%% --------------------------------------------------------------------
get_logout(FixVersion, SenderCompID, TargetCompID) ->
    Utils = fix_convertor:get_util_module(FixVersion),
    L = Utils:getRecord(logout),
    H = Utils:getRecord(standardHeader),
    T = Utils:getRecord(standardTrailer),
    Utils:setFieldInRecord(logout, standardHeader,
                           Utils:setFieldInRecord(logout, 
                                                  standardTrailer, L, T),
    Utils:setFieldInRecord(standardHeader, targetCompID, 
    Utils:setFieldInRecord(standardHeader, sendingTime, 
    Utils:setFieldInRecord(standardHeader, msgType, 
    Utils:setFieldInRecord(standardHeader, senderCompID, H, SenderCompID),
                           logout), ?MODULE:getNow()), TargetCompID)).
%% --------------------------------------------------------------------
%% @doc Gets the logon message
%%
%% @spec get_logon(SenderCompID::binary(), 
%%                 TargetCompID::binary()) -> #logon{}
%% @end
%% --------------------------------------------------------------------
get_logon('FIX 5.0', SenderCompID, TargetCompID) ->
    Utils = fix_convertor:get_util_module('FIX 5.0'),
    Utils:setFieldInRecord(logon, defaultApplVerID,
                            get_logon2(Utils, SenderCompID, 
                                       TargetCompID), "7");
get_logon('FIX 5.0 SP 1', SenderCompID, TargetCompID) ->
    Utils = fix_convertor:get_util_module('FIX 5.0 SP 1'),
    Utils:setFieldInRecord(logon, defaultApplVerID,
                            get_logon2(Utils, SenderCompID, 
                                       TargetCompID), "8");
get_logon('FIX 5.0 SP 2', SenderCompID, TargetCompID) ->
    Utils = fix_convertor:get_util_module('FIX 5.0 SP 2'),
    Utils:setFieldInRecord(logon, defaultApplVerID,
                            get_logon2(Utils, SenderCompID, 
                                       TargetCompID), "9");
get_logon(FixVersion, SenderCompID, TargetCompID) ->
    Utils = fix_convertor:get_util_module(FixVersion),
    get_logon2(Utils, SenderCompID, TargetCompID).

get_logon2(Utils, SenderCompID, TargetCompID) ->
    L = Utils:getRecord(logon),
    H = Utils:getRecord(standardHeader),
    T = Utils:getRecord(standardTrailer),
    Utils:setFieldInRecord(logon, heartBtInt,
    Utils:setFieldInRecord(logon, encryptMethod,
    Utils:setFieldInRecord(logon, standardHeader,
                           Utils:setFieldInRecord(logon, 
                                                  standardTrailer, L, T),
    Utils:setFieldInRecord(standardHeader, targetCompID, 
    Utils:setFieldInRecord(standardHeader, sendingTime, 
    Utils:setFieldInRecord(standardHeader, msgType, 
    Utils:setFieldInRecord(standardHeader, senderCompID, H, SenderCompID),
                           logon), ?MODULE:getNow()), TargetCompID)),
                           none), 30).

%% --------------------------------------------------------------------
%% @doc Gets the heartbeat message
%%
%% @spec get_heartbeat(SenderCompID::binary(), 
%%                     TargetCompID::binary()) -> #hearbeat{}
%% @end
%% --------------------------------------------------------------------
get_heartbeat(FixVersion, SenderCompID, TargetCompID)->
    Utils = fix_convertor:get_util_module(FixVersion),
    L = Utils:getRecord(heartbeat),
    H = Utils:getRecord(standardHeader),
    T = Utils:getRecord(standardTrailer),
    Utils:setFieldInRecord(heartbeat,testReqID,
    Utils:setFieldInRecord(heartbeat, standardHeader,
                           Utils:setFieldInRecord(heartbeat, 
                                                  standardTrailer, L, T),
    Utils:setFieldInRecord(standardHeader, targetCompID, 
    Utils:setFieldInRecord(standardHeader, sendingTime, 
    Utils:setFieldInRecord(standardHeader, msgType, 
    Utils:setFieldInRecord(standardHeader, senderCompID, H, SenderCompID),
                           heartbeat), ?MODULE:getNow()),
                           TargetCompID)),
                           ?MODULE:getUniq()).

%% --------------------------------------------------------------------
%% @doc Gets the resendRequest message
%%
%% @spec get_resendRequest(SenderCompID::binary(), 
%%                     TargetCompID::binary()) -> #hearbeat{}
%% @end
%% --------------------------------------------------------------------
get_resend_request(FixVersion, SenderCompID, TargetCompID, BeginSeqNo, EndSeqNo)->
    Utils = fix_convertor:get_util_module(FixVersion),
    L = Utils:getRecord(resendRequest),
    H = Utils:getRecord(standardHeader),
    T = Utils:getRecord(standardTrailer),
    Utils:setFieldInRecord(resendRequest,beginSeqNo,
    Utils:setFieldInRecord(resendRequest,endSeqNo,
    Utils:setFieldInRecord(resendRequest, standardHeader,
                           Utils:setFieldInRecord(resendRequest, 
                                                  standardTrailer, L, T),
    Utils:setFieldInRecord(standardHeader, targetCompID, 
    Utils:setFieldInRecord(standardHeader, sendingTime, 
    Utils:setFieldInRecord(standardHeader, msgType, 
    Utils:setFieldInRecord(standardHeader, senderCompID, H, SenderCompID),
                           resendRequest), ?MODULE:getNow()),
                           TargetCompID)),
                           BeginSeqNo), EndSeqNo).

%% --------------------------------------------------------------------
%% @doc Gets the message number
%%
%% @spec get_seq_number(TestRequest::#testRequest{}) -> int()
%%
%% @end
%% --------------------------------------------------------------------
get_seq_number(FixVersion, Message)->
    Utils = fix_convertor:get_util_module(FixVersion),
    Header = erlang:element(2, Message),
    D = Utils:get_record_def(standardHeader),
    P = find_first(msgSeqNum, D),
    TargetCompID = erlang:element(P, Header).


%% --------------------------------------------------------------------
%% @doc Gets the heartbeat message
%%
%% @spec get_heartbeat(TestRequest::#testRequest{}) -> #hearbeat{}
%%
%% @end
%% --------------------------------------------------------------------
get_heartbeat(FixVersion, TestRequest)->
    Utils = fix_convertor:get_util_module(FixVersion),
    D1 = Utils:get_record_def(testRequest),
    P1 = find_first(testReqID, D1),
    TestReqID = erlang:element(P1, TestRequest),
    
    Header = erlang:element(2, TestRequest),
    D2 = Utils:get_record_def(standardHeader),
    P2 = find_first(targetCompID, D2),
    TargetCompID = erlang:element(P2, Header),
    P3 = find_first(senderCompID, D2),
    SenderCompID = erlang:element(P3, Header),
    
    L = Utils:getRecord(heartbeat),
    H = Utils:getRecord(standardHeader),
    T = Utils:getRecord(standardTrailer),
    Utils:setFieldInRecord(heartbeat,testReqID,
    Utils:setFieldInRecord(heartbeat, standardHeader,
                           Utils:setFieldInRecord(heartbeat, 
                                                  standardTrailer, L, T),
    Utils:setFieldInRecord(standardHeader, targetCompID, 
    Utils:setFieldInRecord(standardHeader, sendingTime, 
    Utils:setFieldInRecord(standardHeader, msgType, 
    Utils:setFieldInRecord(standardHeader, senderCompID, H, SenderCompID),
                           heartbeat), ?MODULE:getNow()),
                           TargetCompID)),
                           TestReqID).
    
%% --------------------------------------------------------------------
%% @doc Gets the list of message numbers to be resend
%%
%% @spec get_numbers(ResendRequest::#resendRequest{}) -> [integer()]
%%
%% @end
%% --------------------------------------------------------------------
get_numbers(FixVersion, ResendRequest) ->
    Utils = fix_convertor:get_util_module(FixVersion),
    L = Utils:get_record_def(resendRequest),
    P1 = find_first(beginSeqNo, L),
    P2 = find_first(endSeqNo, L),
    case (P1 > 0) andalso (P2 > 0) andalso (P1 =< P2) of
        true ->
            lists:seq(erlang:element(P1, ResendRequest),
             erlang:element(P2, ResendRequest));
        false -> []
    end.
    
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
    lists:concat([integer_to_list(Year),
                                 getTwoDigits(Month),
                                 getTwoDigits(Day),
                                 "-",
                                 getTwoDigits(Hour),
                                 ":",
                                 getTwoDigits(Minute+AddMin),
                                 ":",
                                 getTwoDigits(Second+AddSec)]).

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
    lists:concat([integer_to_list(Year),
                                 getTwoDigits(Month),
                                 getTwoDigits(Day),
                                 "-",
                                 getTwoDigits(Hour),
                                 ":",
                                 getTwoDigits(Minute),
                                 ":",
                                 getTwoDigits(Second)]).

%% --------------------------------------------------------------------
%% @doc Gets uniq id for fix message
%%
%% @spec getUniq() -> binary()
%%
%% @end
%% --------------------------------------------------------------------
getUniq () ->
    {MegaSecs, Secs, MicroSecs} = now(),
    lists:concat([MegaSecs,Secs,MicroSecs]).


    
%% ====================================================================
%% Local Functions
%% ====================================================================
getTwoDigits(Int) when Int < 10 ->
    lists:concat(["0",Int]);
getTwoDigits(Int) ->
    erlang:integer_to_list(Int).

find_first(Element, List) when is_list(List) ->
    find_first(Element, List, 0).

find_first(_Element, [], Inc) when is_integer(Inc) ->
    Inc + 1;
find_first(Element, [Element | _Tail], Inc) when is_integer(Inc) ->
    Inc + 1;
find_first(Element, [_ | Tail], Inc) when is_integer(Inc) ->
    find_first(Element, Tail, Inc + 1).

