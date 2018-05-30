-module(ecf_captcha).

-export([check_captcha/2]).


-define(RECAPTCHA_URL, "https://www.google.com/recaptcha/api/siteverify").


-spec check_captcha(inet:ip_address(), [{binary(),binary()}]) -> boolean().
check_captcha(Ip, KV) ->
    IpBin = inet:ntoa(Ip),
    case lists:keyfind(<<"g-recaptcha-response">>, 1, KV) of
        {_, Resp} ->
            {ok, Secret} = application:get_env(ecf, recaptcha_secret),
            Data = iolist_to_binary(["secret=", Secret,
                                     "&response=", Resp,
                                     "&remoteip=", IpBin]),
            {ok, {200, Body}} = httpc:request(post,
                                              {?RECAPTCHA_URL, [],
                                               "application/x-www-form-urlencoded",
                                               Data},
                                              [],
                                              [{body_format, binary},
                                               {full_result, false}]),
            Map = jiffy:decode(Body, [return_maps]),
            maps:get(<<"success">>, Map);
        false ->
            false
    end.

