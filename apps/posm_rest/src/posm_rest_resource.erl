%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(posm_rest_resource).
-export([init/1, content_types_provided/2, to_text/2, to_json/2]).
-import(posm_protocol, [handle_banks/0]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("posm/include/posm_data.hrl").

-compile([{parse_transform, rec2json}]).


init([]) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
	{[{"text/plain",to_text}, {"application/json", to_json}], ReqData, Context}.

to_text(ReqData, Context) ->
    Result = handle_request(ReqData, Context),
    if 
    	(not is_binary(Result)) -> Body = lists:flatten(io_lib:format("~p", [Result]))
    	; true 					-> Body = Result
    end,
    {Body, ReqData, Context}.

%% TODO record and record list encoding not working...
%% maybe, queries from mnesia should return porpslists
to_json(ReqData, Context) ->
	Result = handle_request(ReqData, Context),
	error_logger:info_msg("Return: ~p", [Result]), 
	if
		is_tuple(Result) ->
			Body = Result:to_json(),
			error_logger:info_msg("is_tuple: ~p", [Body])
		; is_list(Result) and not (Result =:= []) ->
			H = hd(Result),
			Body = H:to_json(),
			error_logger:info_msg("is_list: ~p", [Body])
		; true ->
			Body = jsx:encode(Result),
			error_logger:info_msg("is_true: ~p", [Body])
	end,
	{Body, ReqData, Context}.

handle_request(ReqData, Context) ->
	Path = wrq:disp_path(ReqData),
	case Path of
    	"banks" -> hd(handle_banks());
    	"ping" -> posm_protocol:handle_ping();
    	_      -> error
    end.

