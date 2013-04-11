-record(session_parameter, {
                             id, 
                             host, port, max_reconnect, reconnect_interval, ip, 
                             senderCompId, targetCompId, fix_version, role,
                             heartbeatInterval, start_seqnum,
                             callbackModule
                           }).
-record(fix_in_messages, {number, message}).
-record(fix_out_messages, {number, message}).
-record(last_startup_run, {key, time}).
