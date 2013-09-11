-module(posm_tasks).

-include("posm_data.hrl").

-export([sync_reference_data/0, sync_pos_info/0]).

%% TODO implement reference data sync
sync_reference_data() ->
	error_logger:info_msg("Syncing reference data..."),
	{ok, ConnString} = application:get_env(posm, conn_string),
	{ok, Ref} = odbc:connect(ConnString , []),
	sync_banks(Ref),
	odbc:disconnect(Ref),  
	error_logger:info_msg("Synced reference data.").

%% TODO implement sincronization of data recieved from POS to Core Database
sync_pos_info() ->
	error_logger:info_msg("Syncing pos info").


sync_banks(Ref) ->
	{ok, Query} = application:get_env(posm, select_banks),
	{selected, _, RawBanks} = odbc:sql_query(Ref, Query),
	Banks = lists:map(fun(B) -> create_bank(Ref, B) end, RawBanks),
	F = fun() ->
		lists:foreach(fun(B) -> mnesia:write(B) end, Banks)
	end,
	mnesia:activity(transaction, F).

create_bank(Ref, Bank) ->
	{Cubco, Nombco} = Bank,
	#posm_bank{cubco = Cubco, nombco = Nombco, accounts = []}.