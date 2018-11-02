-module(ecf_login_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).


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
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Email} = lists:keyfind(<<"email">>, 1, KV),
    {_, Password} = lists:keyfind(<<"password">>, 1, KV),
    Ip = ecf_utils:get_ip(Req),
    Req2 = case ecf_log:check_log(Email, Ip) of
               true ->
                   case ecf_captcha:check_captcha(Ip, KV) of
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
           end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

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

