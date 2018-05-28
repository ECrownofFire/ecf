-module(ecf_stream).
-behaviour(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-record(state, {
    next :: any()
}).

init(StreamID, Req, Opts) ->
    {Commands, Next} = cowboy_stream:init(StreamID, Req, Opts),
    {Commands, #state{next=Next}}.

data(StreamID, IsFin, Data, #state{next=Next0}) ->
    {Commands, Next} = cowboy_stream:data(StreamID, IsFin, Data, Next0),
    {Commands, #state{next=Next}}.

info(StreamID, Info, #state{next=Next0}) ->
    Base = application:get_env(ecf, base_url, ""),
    {Commands, Next} = cowboy_stream:info(StreamID, Info, Next0),
    {replace_base_url(Commands, Base), #state{next=Next}}.

terminate(StreamID, Reason, #state{next=Next}) ->
    cowboy_stream:terminate(StreamID, Reason, Next).

early_error(StreamID, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamID, Reason, PartialReq, Resp, Opts).

replace_base_url(Commands, Base) ->
    replace_base_url(Commands, Base, []).

replace_base_url([], _, Acc) ->
    lists:reverse(Acc);
replace_base_url([{response, Status, Headers, Body}|Tail], Base, Acc)
  when not is_tuple(Body) ->
    NewBody = string:replace(Body, "{{base}}", Base, all),
    NewHeaders = Headers#{<<"content-length">> => integer_to_binary(iolist_size(NewBody))},
    NewResp = {response, Status, NewHeaders, NewBody},
    replace_base_url(Tail, Base, [NewResp|Acc]);
replace_base_url([Head|Tail], Base, Acc) ->
    replace_base_url(Tail, Base, [Head|Acc]).

