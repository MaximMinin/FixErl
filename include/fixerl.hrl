-include_lib("fix_convertor/include/fix_convertor.hrl").
-record(session_parameter, {
                             id, 
                             host, port, ip,
                             max_reconnect, reconnect_interval, 
                             senderCompId, targetCompId, 
                             fix_version :: fix_version(), role,
                             heartbeatInterval, start_seqnum,
                             callbackModule
                           }).
-record(last_startup_run, {key, time}).
