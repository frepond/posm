%% POSM basic protocol handler.

-module(posm_protocol).

-export([start_link/4, init/4, handle_ping/0, handle_banks/0]).

-include_lib("stdlib/include/qlc.hrl").

-include("posm_data.hrl").


start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport).

%% TODO sample command
-define (HANDLERS,
	[
		{"ping", fun() -> handle_ping() end},
		{"read", fun() -> handle_read() end},
		{"write", fun(Args) -> handle_write(Args) end},
		{"banks", fun() -> handle_banks() end}
	]).

%% Socket accept loop with the following protocol
%%
%% msg     ::= opcode"(|payload)*
%% opcode  ::= ?HANDLERS
%% table   ::= string()
%% payload ::= key"="value
%% key     ::= string()
%% value   ::= string()
loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 15000) of
		{ok, Data} ->
			error_logger:info_msg("Receiving...~p~n", [Data]),
			{_, Result} = handle_command(Data),
			error_logger:info_msg("Returning: ~p~n", [Result]),
			if 
				(not is_binary(Result)) -> Response = lists:flatten(io_lib:format("~p", [Result]))
				; true 					-> Response = Result
			end,
			Transport:send(Socket, Response),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.

-spec handle_command(bitstring()) -> {ok, bitstring()} | {error, string()}.
handle_command(Data) ->
	String = bitstring_to_list(Data), 
	[Action | Rest] = string:tokens(String, "|\r\n"),
	Args = [ {K,V} || [K,V] <- lists:map(fun(X) -> string:tokens(X, "=") end, Rest)],
	error_logger:info_msg("Handling... ~p: ~p~n", [Action, Args]),
	case lists:keyfind(Action, 1, ?HANDLERS) of
		false -> {error, "Unknown handler: " ++ Action};
		{_, Handler} ->
			if 
				Args =:= [] -> {ok, Handler()}
				; true 	    -> {ok, Handler(Args)}
			end
	end.

%% TODO sample command
handle_write(_Args) ->
	<<"write">>.

handle_read() -> 
	<<"read">>.

%% keep alive verification
-spec handle_ping() -> bitstring().
handle_ping() ->
	<<"pong">>.

-spec handle_banks() -> #posm_bank{}.
handle_banks() -> 
	Query = qlc:q([B || B <- mnesia:table(posm_bank)]),
	mnesia:sync_dirty(fun() -> qlc:e(Query) end).