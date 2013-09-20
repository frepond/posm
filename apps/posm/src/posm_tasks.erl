-module(posm_tasks).

-include("posm_data.hrl").

-export([sync_reference_data/0, sync_pos_info/0]).
-export([group_by/2]).

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
	{ok, QueryBanks} = application:get_env(posm, select_banks), 
	{selected, _, BankRows} = odbc:sql_query(Ref, QueryBanks),
	GroupedAccounts = grouped_accounts(Ref),
	error_logger:info_msg("groups: ~p~n", [GroupedAccounts]), 
	Banks = lists:map(fun(Row) -> 
						{K, _} = Row,
						error_logger:info_msg("key: ~p~n", [K]),
						Accounts = case dict:find(list_to_bitstring(K), GroupedAccounts) of
										error -> [];
										{ok, As} -> lists:map(fun({_, A}) -> A end, As)
								   end,
						error_logger:info_msg("accounts: ~p~n", [Accounts]), 
						create_bank(Row, Accounts)
					  end, BankRows),
	F = fun() ->
		lists:foreach(fun(B) -> mnesia:write(B) end, Banks)
	end,
	mnesia:transaction(F).

-spec grouped_accounts(any()) -> dict().
grouped_accounts(Ref) ->
	{ok, QueryAccounts} = application:get_env(posm, select_accounts),
	{selected, _, AccountRows} = odbc:sql_query(Ref, QueryAccounts),
	error_logger:info_msg("account rows ~p~n", [AccountRows]),
	Accounts = lists:map(fun(Row) -> create_account(Row) end, AccountRows),
	group_by(fun({K, _}) -> K end, Accounts).

-spec create_account(tuple()) -> {bitstring(), #posm_bank_account{}}.
create_account({Cubco, Cubcta, Nrocta, Nrocbu}) ->
	{list_to_bitstring(Cubco), 
		#posm_bank_account{cubcta = list_to_bitstring(Cubcta), 
							nro_cta = list_to_bitstring(Nrocta), 
							nro_cbu = list_to_bitstring(Nrocbu)
						}
	}.

-spec create_bank(tuple(), list(#posm_bank_account{})) -> #posm_bank{}.
create_bank({Cubco, Nombco}, Accounts) ->
	#posm_bank{cubco = list_to_bitstring(Cubco), nombco = list_to_bitstring(Nombco), accounts = Accounts}.

%% List utils
-spec group_by(function(), list()) -> dict().
group_by(F, L) -> lists:foldr(fun({K,V}, D) -> dict:append(K, V, D) end , dict:new(), [ {F(X), X} || X <- L ]).