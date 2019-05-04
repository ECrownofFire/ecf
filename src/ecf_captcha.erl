-module(ecf_captcha).

-export([check_captcha/2, get_atom/0]).


-define(RECAPTCHA_URL, "https://www.google.com/recaptcha/api/siteverify").

-define(RESP_ATOM, 'g-recaptcha-response').

-spec get_atom() -> atom().
get_atom() ->
    ?RESP_ATOM.

-spec check_captcha(inet:ip_address(), binary()) -> boolean().
check_captcha(Ip, Resp) ->
    case Resp of
        <<"">> -> false;
        _ ->
            Data = build_data(Ip, Resp),
            {ok, {200, Body}} = do_request(Data),
            Map = jiffy:decode(Body, [return_maps]),
            maps:get(<<"success">>, Map)
    end.

build_data(Ip, Resp) ->
    IpBin = inet:ntoa(Ip),
    {ok, Secret} = application:get_env(ecf, recaptcha_secret),
    iolist_to_binary(["secret=", Secret,
                      "&response=", Resp,
                      "&remoteip=", IpBin]).

do_request(Data) ->
    httpc:request(post,
                  {?RECAPTCHA_URL, [], "application/x-www-form-urlencoded", Data},
                  [],
                  [{body_format, binary}, {full_result, false}]).

