%% @author Maxim Minin 
%% @doc @todo Add description to fix_4_2_convertor_proper_test.
-module(fix_4_2_record_generator). 

%%       
%% Include files  
%%
-include_lib("proper/include/proper.hrl").   
-include_lib("fix_convertor/include/FIX_4_2.hrl").

-compile([{no_auto_import, [date/0, time/0]}, 
          {parse_transform, a_generator_transform}, 
          export_all, debug_info]). 
 
-define(FIXVERSION, 'FIX 4.2').   
  
%% ====================================================================
%% API functions
%% ====================================================================
  
test_record() ->
    ?LET(M, msgType(), ?MODULE:M()).

resend_record(M) ->
    N= erlang:length(M),
    #resendRequest{standardHeader = #standardHeader{beginString = "FIX.4.2",
                                                      bodyLength = 97,
                                                      msgType = resendRequest,
                                                      senderCompID = "TEST",
                                                      targetCompID = "TEST1",
                                                      msgSeqNum = 2,
                                                      sendingTime = {{2011,8,2},{10,0,0}}},
                   beginSeqNo = 1, 
                   endSeqNo = N,
                  standardTrailer = #standardTrailer{}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

fix_string() -> 
    list(choose(36, 126)).

fix_binary() ->
    ?LET(Bin, binary(),
    erlang:list_to_binary([<<1>>,Bin,<<1>>])).

fix_float() -> 
    ?LET(F, float(),
         begin
             [Fr] = io_lib:format("~.6f",[F*10.0]),
             list_to_float(Fr)
         end).

uTCTimestamp() -> 
    ?LET(DateTime, {uTCDateOnly(), uTCTimeOnly()}, DateTime).

uTCTimeOnly() ->
    ?LET(Time,
         {choose(0, 23),
         choose(0, 59),
         choose(0,59)},
         Time).   

uTCDateOnly() ->
    ?LET(Date,
         {choose(1970, 2099),
         choose(1,12),
         choose(1,31)},
         Date).

localMktDate() ->
    ?LET(Date,
         {choose(1970, 2099),
         choose(1,12),
         choose(1,31)},
         Date).

monthYear() ->
    ?LET(Date,
         {choose(1970, 2099),
         choose(1,12),
         non_empty(elements([choose(1,31), undefined, w1,w2,w3,w4]))},
         Date).

is_eq(Rec1, Rec2) -> 
    L1 = erlang:tuple_to_list(Rec1),
    L2 = erlang:tuple_to_list(Rec2),
    H1 = lists:nth(2, L1),
    H2 = lists:nth(2, L2),
    T1 = lists:last(L1),
    T2 = lists:last(L2),
    lists:sublist(L1, 3, length(L1)-3) ==
        lists:sublist(L2, 3, length(L2)-3) andalso
    erlang:hd(L1)  == erlang:hd(L2) 
    andalso
    H1#standardHeader{origSendingTime= not_to_check,
                      beginString = not_to_check,
                      msgSeqNum = not_to_check,
                      bodyLength = not_to_check} == 
        H2#standardHeader{origSendingTime= not_to_check,
                          beginString = not_to_check,
                          msgSeqNum = not_to_check,
                          bodyLength = not_to_check}
    andalso
    T1#standardTrailer{signature=not_to_check,
                       checkSum = not_to_check} == 
        T2#standardTrailer{signature=not_to_check,
                           checkSum = not_to_check}.


