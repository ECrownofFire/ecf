-module(ecf_logout_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req = #{method := <<"POST">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = try_logout(User, Req),
    {ok, Req2, State};
init(Req = #{method := <<"GET">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    #{url := Url} = cowboy_req:match_qs([{url, [], <<"">>}], Req),
    Html = ecf_generators:generate(logout, User, Url),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                            Html, Req),
    {ok, Req2, State};
init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = cowboy_req:reply(405,
                            #{<<"content-type">> => <<"text/html">>,
                              <<"allow">> => <<"GET, POST">>},
                            ecf_generators:generate(405, User, {logout, false}),
                            Req),
    {ok, Req2, State}.

try_logout(undefined, Req) ->
    Base = application:get_env(ecf, base_url, <<"">>),
    cowboy_req:reply(303, #{<<"location">> => [Base, "/"]}, Req);
try_logout(User, Req) ->
    #{url := Url} = cowboy_req:match_qs([{url, [], <<"">>}], Req),
    {_Id, Sess} = ecf_utils:get_session_cookies(Req),
    ok = ecf_user:reset_session(ecf_user:id(User), Sess),
    Req2 = ecf_utils:clear_login_cookies(Req),
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"text/html">>},
                     ecf_generators:generate(logout, undefined, Url),
                     Req2).

terminate(_Reason, _Req, _State) ->
    ok.

