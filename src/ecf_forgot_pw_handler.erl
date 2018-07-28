-module(ecf_forgot_pw_handler).

-export([init/2]).


init(Req = #{method := <<"GET">>}, State) ->
    Html = ecf_generators:generate(forgot_pw, undefined, forgot_pw_message),
    Req2 = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"text/html">>},
                            Html,
                            Req),
    {ok, Req2, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Email} = lists:keyfind(<<"email">>, 1, KV),
    Ip = ecf_utils:get_ip(Req),
    Req2 = try_reset(ecf_captcha:check_captcha(Ip, KV), Email, Req),
    {ok, Req2, State}.

try_reset(false, _, Req) ->
    Html = ecf_generators:generate(forgot_pw, undefined, forgot_pw_failed_captcha),
    cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, Html, Req);
try_reset(true, Email, Req) ->
    try_email(ecf_user:get_user_by_email(Email)),
    Html = ecf_generators:generate(forgot_pw, undefined, forgot_pw_confirm),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req).

try_email(undefined) ->
    ok;
try_email(User) ->
    ecf_email:send_password_reset_email(User).

