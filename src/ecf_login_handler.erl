-module(ecf_login_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

-export([set_login_cookies/3]).

-define(SESSION_TIME, 604800). % one week

init(Req = #{method := <<"GET">>}, State) ->
    #{url := Url} = cowboy_req:match_qs([{url, [], <<"">>}], Req),
    User = ecf_utils:check_user_session(Req),
    Html = ecf_generators:generate(login, User,
                                   {ecf_log:check_log("", ecf_utils:get_ip(Req)),
                                    Url, login_message}),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                            Html, Req),
    {ok, Req2, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    #{url := Url} = cowboy_req:match_qs([{url, [], <<"">>}], Req0),
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Email} = lists:keyfind(<<"email">>, 1, KV),
    {_, Password} = lists:keyfind(<<"password">>, 1, KV),
    Ip = ecf_utils:get_ip(Req),
    case ecf_log:check_log(Email, Ip) of
        true ->
            case ecf_captcha:check_captcha(Ip, KV) of
                true ->
                    try_login(Email, Password, Url, Req, State);
                false ->
                    Html = ecf_generators:generate(login, undefined,
                                                   {true, Url, login_fail_captcha}),
                    Req2 = cowboy_req:reply(429,
                                            #{<<"content-type">>
                                              => <<"text/html">>},
                                            Html, Req),
                    {ok, Req2, State}
            end;
        false ->
            try_login(Email, Password, Url, Req, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

try_login(Email, Password, Url, Req, State) ->
    User = ecf_user:get_user_by_email(Email),
    try_login(User, Password, Url, Req, State, Email).


try_login(undefined, _, _, Req, State, Email) ->
    ok = ecf_user:fake_hash(),
    login_fail(Req, State, Email);
try_login(User, Password, Url, Req, State, Email) ->
    case ecf_user:check_pass(User, Password) of
        true ->
            ecf_log:clear_log(Email, ecf_utils:get_ip(Req)),
            Session = ecf_user:new_session(ecf_user:id(User)),
            Req2 = set_login_cookies(Req, ecf_user:id(User), Session),
            Base = application:get_env(ecf, base_url, ""),
            Req3 = cowboy_req:reply(303, #{<<"location">> => [Base, "/", Url]},
                                    Req2),
            {ok, Req3, State};
        false ->
            login_fail(Req, State, Email)
    end.

login_fail(Req, State, Email) ->
    #{url := Url} = cowboy_req:match_qs([{url, [], <<"">>}], Req),
    Ip = ecf_utils:get_ip(Req),
    ecf_log:log(Email, Ip),
    Html = ecf_generators:generate(login, undefined, {ecf_log:check_log(Email, Ip),
                                                      Url,
                                                      login_fail}),
    Req2 = cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>},
                            Html, Req),
    {ok, Req2, State}.

set_login_cookies(Req, Id, Session) ->
    SessionEncoded = base64:encode(Session),
    Req2 = cowboy_req:set_resp_cookie(<<"session">>, SessionEncoded,
                                      Req,
                                      #{http_only => true,
                                        secure => true,
                                        max_age => ?SESSION_TIME}),
    cowboy_req:set_resp_cookie(<<"user">>,
                               integer_to_list(Id),
                               Req2,
                               #{max_age => ?SESSION_TIME}).

