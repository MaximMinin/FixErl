%% Author: Maxim Minin
%% Created: 24.06.2012
%% Description: TODO: Add description to fix_utils
-module(fix_utils).

%%
%% Include files
%%
-include("FIX_4_2.hrl").
%%
%% Exported Functions
%%
-export([get_logon/2, get_heartbeat/2, get_heartbeat_on_testrequest/1,
         get_messagesnumbers_toresend/1,
         getNow/0, getNow/1, getUniq/0
         ]).

%%
%% API Functions
%%
get_logon(SenderCompID, TargetCompID) ->
    #logon{
           standardHeader = #standardHeader{
                                             msgType = logon,
                                             sendingTime = ?MODULE:getNow(),
                                             senderCompID = SenderCompID, 
                                             targetCompID = TargetCompID
                                            },
           standardTrailer = #standardTrailer{},
           repeatingReg_logon_384 = [#repeatingReg_logon_384{}],
           heartBtInt = 30
          }.
get_heartbeat(SenderCompID, TargetCompID)->
    #heartbeat{
               testReqID = ?MODULE:getUniq(),
               standardHeader = #standardHeader{
                                                  msgType = heartbeat,
                                                  sendingTime = ?MODULE:getNow(),
                                                  senderCompID = SenderCompID,
                                                  targetCompID = TargetCompID
                                                 },
                standardTrailer = #standardTrailer{}
               }.

get_heartbeat_on_testrequest(#testRequest{standardHeader = #standardHeader{
                                                                           targetCompID = TargetCompID,
                                                                           senderCompID = SenderCompID
                                                                           },
                                          testReqID = TestReqID})->
    #heartbeat{
               testReqID = TestReqID,
               standardHeader = #standardHeader{
                                                  msgType = heartbeat,
                                                  sendingTime = ?MODULE:getNow(),
                                                  senderCompID = TargetCompID,
                                                  targetCompID = SenderCompID
                                                 },
                standardTrailer = #standardTrailer{}
               }.

get_messagesnumbers_toresend(#resendRequest{beginSeqNo=Start, endSeqNo = End})->
    lists:seq(Start, End).
    
getNow(AddTimeInSec) ->
    AddSec = AddTimeInSec rem 60,
    AddMin = AddTimeInSec div 60,
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    erlang:list_to_binary(lists:concat([
                                        erlang:integer_to_list(Year),
                                        getTwoDigits(Month),
                                        getTwoDigits(Day),
                                        "-",
                                        getTwoDigits(Hour),
                                        ":",
                                        getTwoDigits(Minute+AddMin),
                                        ":",
                                        getTwoDigits(Second+AddSec)
                                       ]
                                      )
                         ).
getNow() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    erlang:list_to_binary(lists:concat([
                                        erlang:integer_to_list(Year),
                                        getTwoDigits(Month),
                                        getTwoDigits(Day),
                                        "-",
                                        getTwoDigits(Hour),
                                        ":",
                                        getTwoDigits(Minute),
                                        ":",
                                        getTwoDigits(Second)
                                       ]
                                      )
                         ).

getUniq () ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    erlang:list_to_binary(lists:concat([
                                        erlang:integer_to_list(MegaSecs),
                                        erlang:integer_to_list(Secs),
                                        erlang:integer_to_list(MicroSecs)
                                       ]
                                      )
                         ).


    
%%
%% Local Functions
%%
getTwoDigits(Int) when Int < 10 ->
    lists:concat(["0",Int]);
getTwoDigits(Int) ->
    erlang:integer_to_list(Int).

