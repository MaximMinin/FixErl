-include_lib("fix_convertor/include/fix_convertor.hrl").
-record(session_parameter, { id :: atom(), 
                             host :: inet:ip_address(),
                             port :: inet:port_number(),
                             max_reconnect :: non_neg_integer(),
                             reconnect_interval :: non_neg_integer(), 
                             role :: 'acceptor' | 'initiator',
                             callback :: {Module :: atom(), Function :: atom()},
                             callback_mode :: standard | all,
                             fix_version :: fix_version(),
                             start_seqnum :: pos_integer(),
                             heartbeatInterval :: pos_integer(),
                             senderCompId :: string(),
                             targetCompId :: string()
                           }).
