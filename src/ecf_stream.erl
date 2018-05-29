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
    NewBody = string:replace(Body, <<"{{base}}">>, Base, all),
    NewSize = integer_to_binary(iolist_size(NewBody)),
    NewResp = case maps:get(<<"Location">>, Headers, undefined) of
                  undefined ->
                      NewHeaders = Headers#{<<"content-length">> => NewSize},
                      {response, Status, NewHeaders, NewBody};
                  OldLoc ->
                      NewLoc = string:replace(OldLoc, <<"{{base}}">>, Base),
                      NewHeaders = Headers#{<<"content-length">> => NewSize,
                                            <<"Location">> => NewLoc},
                      {response, Status, NewHeaders, NewBody}
              end,
    replace_base_url(Tail, Base, [NewResp|Acc]);
replace_base_url([Head|Tail], Base, Acc) ->
    replace_base_url(Tail, Base, [Head|Acc]).

