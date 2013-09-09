-module(posm_tasks).

-export([sync_reference_data/0, sync_pos_info/0]).

%% TODO implement reference data sync
sync_reference_data() ->
	error_logger:info_msg("Syncing reference data").

%% TODO implement sincronization of data recieved from POS to Core Database
sync_pos_info() ->
	error_logger:info_msg("Syncing pos info").