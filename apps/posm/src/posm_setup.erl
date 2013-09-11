-module(posm_setup).

-include("posm_data.hrl").

-export([initialize/1, create_tables/1, delete_tables/1]).


initialize(Nodes) ->
	application:stop(mnesia),
	mnesia:create_schema(Nodes),
	application:start(mnesia),
	delete_tables(Nodes),
	create_tables(Nodes).


delete_tables(Nodes) ->
	mnesia:delete_table(posm_bank).

create_tables(Nodes) -> 
	mnesia:create_table(posm_bank, 
		[{disc_copies, [node() | Nodes]}, {attributes, record_info(fields, posm_bank)}]).