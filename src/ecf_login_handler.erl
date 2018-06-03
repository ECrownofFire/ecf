-module(ecf_login_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

-export([set_login_cookies/3]).

-define(SESSION_TIME, 604800). % one week

init(Req0, State) ->
    #{url := Url} = cowboy_req:match_qs([{url, [], <<"">>}], Req0),
    case maps:get(method, Req0) of
        <<"POST">> ->
            {ok, KeyValues, Req} = cowboy_req:read_urlencoded_body(Req0),
            {_, Username} = lists:keyfind(<<"username">>, 1, KeyValues),
            {_, Password} = lists:keyfind(<<"password">>, 1, KeyValues),
            Ip = ecf_utils:get_ip(Req),
            case ecf_log:check_log(Username, Ip) of
                true ->
                    case ecf_captcha:check_captcha(Ip, KeyValues) of
                        true ->
                            try_login(Username, Password, Url, Req, State);
                        false ->
                            Text = application:get_env(ecf, login_fail_captcha,
                                               <<"You need to solve a CAPTCHA">>),
                            Map = #{<<"result">> => <<"captcha">>,
                                    <<"text">> => Text},
                            Json = jiffy:encode(Map),
                            Req2 = cowboy_req:reply(400,
                                                    #{<<"content-type">>
                                                      => <<"application/json">>},
                                                    Json,
                                                    Req),
                            {ok, Req2, State}
                    end;
                false ->
                    try_login(Username, Password, Url, Req, State)
            end;
        <<"GET">> ->
            User = ecf_utils:check_user_session(Req0),
            Html = ecf_generators:generate(login, User, Url),
            Req = cowboy_req:reply(200,
                                   #{<<"content-type">> => <<"text/html">>},
                                   Html,
                                   Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405,
                                   #{<<"content-type">> => <<"text/html">>},
                                   ecf_generators:generate(405, undefined,
                                                           login),
                                   Req0),
            {ok, Req, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

try_login(Username, Password, Url, Req, State) ->
    User = ecf_user:get_user_by_name(Username),
    try_login(User, Password, Url, Req, State, Username).


try_login({error, user_not_found}, _, _, Req, State, Username) ->
    ok = ecf_user:fake_hash(),
    login_fail(Req, State, Username);
try_login(User, Password, Url, Req, State, Username) ->
    case ecf_user:check_pass(User, Password) of
        true ->
            ecf_log:clear_log(Username, ecf_utils:get_ip(Req)),
            Session = ecf_user:new_session(ecf_user:id(User)),
            Req2 = set_login_cookies(Req, ecf_user:id(User), Session),
            Map = #{<<"result">> => <<"success">>,
                    <<"newLocation">> => list_to_binary([<<"{{base}}/">>, Url])},
            Json = jiffy:encode(Map),
            Req3 = cowboy_req:reply(200,
                                    #{<<"content-type">> => <<"application/json">>},
                                    Json,
                                    Req2),
            {ok, Req3, State};
        false ->
            login_fail(Req, State, Username)
    end.

login_fail(Req, State, Username) ->
    ecf_log:log(Username, ecf_utils:get_ip(Req)),
    Text = application:get_env(ecf, login_fail_message,
                               <<"Invalid username or password, try again.">>),
    Map = #{<<"result">> => <<"failed">>,
            <<"text">> => Text},
    Json = jiffy:encode(Map),
    Req2 = cowboy_req:reply(400,
                            #{<<"content-type">> => <<"application/json">>},
                            Json,
                            Req),
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

