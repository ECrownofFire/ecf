-module(ecf_login_handler).
-behaviour(cowboy_handler).

-export([init/2]).


init(Req = #{method := <<"GET">>}, State) ->
    #{url := Url} = cowboy_req:match_qs([{url, [], <<"">>}], Req),
    User = ecf_utils:check_user_session(Req),
    Html = ecf_generators:generate(login, User,
                                   {ecf_log:check_log(<<>>, ecf_utils:get_ip(Req)),
                                    Url, login_message}),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                            Html, Req),
    {ok, Req2, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    #{url := Url} = cowboy_req:match_qs([{url, [], <<"">>}], Req0),
    {Email, Password, Resp, Req} = get_login_vars(Req0),
    Req2 = do_login(Email, Password, Url, Resp, Req),
    {ok, Req2, State}.

get_login_vars(Req0) ->
    Atom = ecf_captcha:get_atom(),
    L = [email, password, {Atom, [], <<"">>}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{email := Email, password := Password, Atom := Resp} = M,
    {Email, Password, Resp, Req}.

do_login(Email, Password, Url, Resp, Req) ->
    Ip = ecf_utils:get_ip(Req),
    case ecf_log:check_log(Email, Ip) of
        true ->
            case ecf_captcha:check_captcha(Ip, Resp) of
                true ->
                    try_login(Email, Password, Url, Req);
                false ->
                    Html = ecf_generators:generate(login, undefined,
                                                   {true, Url,
                                                    failed_captcha}),
                    cowboy_req:reply(429, #{<<"content-type">>
                                            => <<"text/html">>},
                                     Html, Req)
            end;
        false ->
            try_login(Email, Password, Url, Req)
    end.

try_login(Email, Password, Url, Req) ->
    User = ecf_user:get_user_by_email(Email),
    try_login(User, Password, Url, Req, Email).


try_login(undefined, _, _, Req, Email) ->
    ok = ecf_user:fake_hash(),
    login_fail(Req, Email, login_fail);
try_login(User, Password, Url, Req, Email) ->
    case {ecf_user:enabled(User), ecf_user:check_pass(User, Password)} of
        {true, true} ->
            ecf_log:clear_log(Email, ecf_utils:get_ip(Req)),
            Session = ecf_user:new_session(ecf_user:id(User)),
            Req2 = ecf_utils:set_login_cookies(Req, ecf_user:id(User), Session),
            cowboy_req:reply(303,
                             #{<<"location">> => ["/", Url]},
                             Req2);
        {true, false} ->
            login_fail(Req, Email, login_fail);
        {false, _} ->
            login_fail(Req, Email, login_disabled)
    end.

login_fail(Req, Email, Type) ->
    #{url := Url} = cowboy_req:match_qs([{url, [], <<"">>}], Req),
    Ip = ecf_utils:get_ip(Req),
    ecf_log:log(Email, Ip),
    Html = ecf_generators:generate(login, undefined, {ecf_log:check_log(Email, Ip),
                                                      Url,
                                                      Type}),
    cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, Html, Req).

