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
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Email} = lists:keyfind(<<"email">>, 1, KV),
    Ip = ecf_utils:get_ip(Req),
    Req2 = check_captcha(ecf_log:check_log(Email, Ip), KV, Ip, Req),
    {ok, Req2, State}.


check_captcha(true, KV, Ip, Req) ->
    case ecf_captcha:check_captcha(Ip, KV) of
        true ->
            try_reset_pw(KV, Ip, Req);
        false ->
            {_, Code} = lists:keyfind(<<"code">>, 1, KV),
            Html = ecf_generators:generate(reset_pw, undefined,
                                           {true, reset_pw_failed_captcha, Code}),
            cowboy_req:reply(429, #{<<"content-type">> => <<"text/html">>},
                             Html, Req)
    end;
check_captcha(false, KV, Ip, Req) ->
    try_reset_pw(KV, Ip, Req).


try_reset_pw(KV, Ip, Req) ->
    {_, Email} = lists:keyfind(<<"email">>, 1, KV),
    {_, Password} = lists:keyfind(<<"password">>, 1, KV),
    {_, Code} = lists:keyfind(<<"code">>, 1, KV),
    case check_password(Password) of
        true ->
            case ecf_user:get_user_by_email(Email) of
                undefined ->
                    fail(Email, Ip, Code, Req);
                User ->
                    Id = ecf_user:id(User),
                    Valid = (Code =:= ecf_email:get_password_reset_code(Id)),
                    reset_pw(Valid, Id, Password, Email, Ip, Code, Req)
            end;
        false ->
            reply_fail(Email, Ip, Code, Req, reset_pw_invalid_pw)
    end.


reset_pw(true, Id, Password, Email, Ip, _, Req) ->
    ecf_log:clear_log(Email, Ip),
    ecf_email:clear_password_reset_code(Id),
    Sess = ecf_user:edit_pass(Id, Password),
    Req2 = ecf_login_handler:set_login_cookies(Req, Id, Sess),
    Base = application:get_env(ecf, base_url, ""),
    cowboy_req:reply(303, #{<<"location">> => [Base, "/"]}, Req2);
reset_pw(false, _, _, Email, Ip, Code, Req) ->
    fail(Email, Ip, Code, Req).


fail(Email, Ip, Code, Req) ->
    ok = ecf_user:fake_hash(),
    ecf_log:log(Email, Ip),
    reply_fail(Email, Ip, Code, Req, reset_pw_fail).


reply_fail(Email, Ip, Code, Req, Type) ->
    Html = ecf_generators:generate(reset_pw, undefined,
                                   {ecf_log:check_log(Email, Ip),
                                    Type, Code}),
    cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, Html, Req).


check_password(Password) ->
    Min = application:get_env(ecf, min_password_length, 8),
    Max = application:get_env(ecf, max_password_length, 64),
    case byte_size(Password) of
        N when N >= Min, N =< Max ->
            true;
        _ ->
            false
    end.

