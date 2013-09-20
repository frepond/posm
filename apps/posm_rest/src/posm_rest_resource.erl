%% @author Federico Repond <federico.repond@tecso.coop>
%% @copyright Tecso.
%% @doc Web Machine for REST services.

-module(posm_rest_resource).
-export([init/1, content_types_provided/2, to_text/2, to_json/2]).

-compile([{parse_transform, rec2json}]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("posm/include/posm_data.hrl").


init([]) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"text/plain",to_text}, {"application/json", to_json}], ReqData, Context}.

to_text(ReqData, Context) ->
    Result = handle_request(ReqData, Context),
    if 
        (not is_binary(Result)) -> Body = lists:flatten(io_lib:format("~p", [Result]))
        ; true                  -> Body = Result
    end,
    {Body, ReqData, Context}.

%% TODO record and record list encoding not working as expectedo...
%% maybe, queries from mnesia should return porpslists
to_json(ReqData, Context) ->
    Result = handle_request(ReqData, Context),
    error_logger:info_msg("Return: ~p~n", [Result]), 
    if
        is_tuple(Result) ->
            Body = tuple_to_json_binary(Result)
        ; is_list(Result) andalso Result =/= [] andalso is_tuple(hd(Result)) ->
            Body = [<<"[">>, list_to_json_binary(Result), <<"]">>]
        ; true ->
            Body = mochijson2:encode(Result)
    end,
    {Body, ReqData, Context}.

handle_request(ReqData, _Context) ->
    Path = wrq:disp_path(ReqData),
    case Path of
        "banks" -> posm_protocol:handle_banks();
        "ping" -> posm_protocol:handle_ping();
        _      -> error
    end.

%% Recursively traverse records converting it to json. In order to do this header containing 
%% record definition must be included and record accesor .beam should be generated and in the 
%% path:
%%   deps/rec2json/rec2json -src=apps/posm/include/*.hrl -dest=apps/posm/ebin -include=include
-spec tuple_to_json_binary(Record :: tuple()) -> iolist().
tuple_to_json_binary(Tuple) ->
    PropList = Tuple:to_json(),
    mochijson2:encode(PropList).

-spec list_to_json_binary(RecordList :: [tuple()]) -> iolist().
list_to_json_binary([H | []]) -> [tuple_to_json_binary(H)];
list_to_json_binary([H | T]) -> [tuple_to_json_binary(H) , <<",">> , list_to_json_binary(T)].
