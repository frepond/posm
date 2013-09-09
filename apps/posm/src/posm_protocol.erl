%% POSM basic protocol handler.

-module(posm_protocol).
-export([start_link/4, init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport).

%% TODO sample command
-define (HANDLERS,
	[
		{"ping", handle_ping()},
		{"read", handle_read()},
		{"write", handle_write()}
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
			{_, Response} = handle_command(Data),
			Transport:send(Socket, Response),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.

handle_command(Data) ->
	String = bitstring_to_list(Data), 
	[Action | Rest] = string:tokens(String, "|\r\n"),
	Args = [ {K,V} || [K,V] <- lists:map(fun(X) -> string:tokens(X, "=") end, Rest)],
	error_logger:info_msg("Handling... ~p: ~p~n", [Action, Args]),
	case lists:keyfind(Action, 1, ?HANDLERS) of
		false -> {error, "Unknown handler: " ++ Action};
		{_, Handler} ->
			if Args =:= [] -> {ok, Handler()}
			   ; true 	   -> {ok, Handler(Args)}
			end
	end.

%% TODO sample command
handle_write() -> 
	fun(Args) -> 
			"write\n"
	end.

handle_read() -> 
	fun() ->
			"read\n"
	end.

%% keep alive verification
handle_ping() ->
	fun() ->
		"pong"
	end.