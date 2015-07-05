-include_lib("fix_convertor/include/fix_convertor.hrl").
-record(message_checks, {check_msgSeqNum = false :: boolean(),
                         check_CheckSum = true :: boolean(),
                         check_CompIds = true :: boolean(),
                         check_CompIdsAlways = false :: boolean()}).
-record(session_parameter, { id :: atom(), 
                             host :: inet:ip_address(),
                             port :: inet:port_number(),
                             max_reconnect = 10 :: non_neg_integer(),
                             reconnect_interval = 10 :: non_neg_integer(), 
                             role :: 'acceptor' | 'initiator',
                             callback :: {Module :: atom(), Function :: atom()},
                             callback_mode :: standard | all,
                             logon_callback :: {Module :: atom(), Function :: atom()},
                             fix_version :: fix_version(),
                             start_seqnum :: pos_integer(),
                             heartbeatInterval :: pos_integer(),
                             senderCompId :: string(),
                             targetCompId :: string(),
                             skip_by_resend_request = [logon, testRequest, heartbeat, logout, sequenceReset, resendRequest],
                             message_checks = #message_checks{} ::  #message_checks{}
                           }).
