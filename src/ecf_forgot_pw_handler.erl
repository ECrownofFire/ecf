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
    {Email, Resp, Req} = get_vars(Req0),
    Req2 = check_captcha(Email, Resp, Req),
    {ok, Req2, State}.

-spec get_vars(cowboy_req:req()) -> {binary(), binary(), cowboy_req:req()}.
get_vars(Req0) ->
    Atom = ecf_captcha:get_atom(),
    L = [email, {Atom, [], <<"">>}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{email := Email, Atom := Resp} = M,
    {Email, Resp, Req}.

check_captcha(Email, Resp, Req) ->
    Ip = ecf_utils:get_ip(Req),
    case ecf_captcha:check_captcha(Ip, Resp) of
        true ->
            try_reset(Email, Req);
        false ->
            Html = ecf_generators:generate(forgot_pw, undefined, failed_captcha),
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, Html, Req)
    end.

try_reset(Email, Req) ->
    try_email(ecf_user:get_user_by_email(Email)),
    Html = ecf_generators:generate(forgot_pw, undefined, forgot_pw_confirm),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req).

try_email(undefined) ->
    ok;
try_email(User) ->
    ecf_email:send_password_reset_email(User).

