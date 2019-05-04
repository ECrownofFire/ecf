-module(ecf_reset_pw_handler).

-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    #{code := Code} = cowboy_req:match_qs([{code, [], <<>>}], Req),
    Html = ecf_generators:generate(reset_pw, User,
                                   {ecf_log:check_log(<<>>, ecf_utils:get_ip(Req)),
                                    reset_pw_message, Code}),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                            Html, Req),
    {ok, Req2, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    {Email, Code, Password, Resp, Req} = get_reset_vars(Req0),
    Req2 = check_log(Email, Code, Password, Resp, Req),
    {ok, Req2, State}.

get_reset_vars(Req0) ->
    Atom = ecf_captcha:get_atom(),
    L = [email, code, password, {Atom, [], <<"">>}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{email := Email, code := Code, password := Password, Atom := Resp} = M,
    {Email, Code, Password, Resp, Req}.

check_log(Email, Code, Password, Resp, Req) ->
    Ip = ecf_utils:get_ip(Req),
    case ecf_log:check_log(Email, Ip) of
        true ->
            check_captcha(Email, Password, Code, Resp, Req);
        false ->
            check_password(Email, Password, Code, Req)
    end.

check_captcha(Email, Password, Code, Resp, Req) ->
    Ip = ecf_utils:get_ip(Req),
    case ecf_captcha:check_captcha(Ip, Resp) of
        true ->
            check_password(Email, Password, Code, Req);
        false ->
            Html = ecf_generators:generate(reset_pw, undefined,
                                           {true, failed_captcha, Code}),
            cowboy_req:reply(429, #{<<"content-type">> => <<"text/html">>},
                             Html, Req)
    end.

check_password(Email, Password, Code, Req) ->
    case ecf_utils:valid_password(Password) of
        true ->
            check_user(Email, Password, Code, Req);
        false ->
            reply_fail(Email, Code, Req, invalid_password)
    end.

check_user(Email, Password, Code, Req) ->
    case ecf_user:get_user_by_email(Email) of
        undefined ->
            fail(Email, Code, Req);
        User ->
            Id = ecf_user:id(User),
            check_code(Id, Email, Password, Code, Req)
    end.

check_code(Id, Email, Password, Code, Req) ->
    case ecf_email:get_password_reset_code(Id) of
        Code ->
            do_reset_pw(Id, Password, Email, Req);
        _ ->
            fail(Email, Code, Req)
    end.

do_reset_pw(Id, Password, Email, Req0) ->
    Ip = ecf_utils:get_ip(Req0),
    ecf_log:clear_log(Email, Ip),
    ecf_email:clear_password_reset_code(Id),
    Sess = ecf_user:edit_pass(Id, Password),
    Req = ecf_utils:set_login_cookies(Req0, Id, Sess),
    ecf_utils:reply_redirect(303, "/", Req).

fail(Email, Code, Req) ->
    Ip = ecf_utils:get_ip(Req),
    ok = ecf_user:fake_hash(),
    ecf_log:log(Email, Ip),
    reply_fail(Email, Code, Req, reset_pw_fail).

reply_fail(Email, Code, Req, Type) ->
    Ip = ecf_utils:get_ip(Req),
    Html = ecf_generators:generate(reset_pw, undefined,
                                   {ecf_log:check_log(Email, Ip),
                                    Type, Code}),
    cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, Html, Req).

