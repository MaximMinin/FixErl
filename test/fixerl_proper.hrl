-compile([{no_auto_import, [date/0, time/0]}, 
          export_all, debug_info]).
-include("fixerl.hrl").
-include_lib("proper/include/proper.hrl").
-define(DUMMY, dummy).
-define(LOG(A, B), lager:info(A,B)).
-define(EMERGENCY(A, B), lager:emergency(A,B)).
