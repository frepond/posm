-module(posm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("posm_data.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, TCP_POOL_SIZE} = application:get_env(posm, tcp_pool_size),
	{ok, POSM_PORT} = application:get_env(posm, posm_port),
 	{ok, _} = ranch:start_listener(tcp_echo, TCP_POOL_SIZE,
		ranch_tcp, [{port, POSM_PORT}], posm_protocol, []),
    posm_sup:start_link().

stop(_State) ->
    ok.


%% ===================================================================
%% Application tests
%% ===================================================================
-ifdef(TEST).

simple_test() ->
	ok = application:start(mnesia),
	ok = application:start(ranch),
    ok = application:start(posm),
    ?assertNot(undefined == whereis(posm_sup)).

-endif.
