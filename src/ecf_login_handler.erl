-module(ecf_login_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

-define(SESSION_TIME, 604800). % one week

init(Req0, State) ->
    #{url := Url} = cowboy_req:match_qs([{url, [], <<"">>}], Req0),
    case maps:get(method, Req0) of
        <<"POST">> ->
            {ok, KeyValues, Req} = cowboy_req:read_urlencoded_body(Req0),
            {_, Username} = lists:keyfind(<<"username">>, 1, KeyValues),
            {_, Password} = lists:keyfind(<<"password">>, 1, KeyValues),
            % TODO: persist cookie option?
            {_, _Persist}  = lists:keyfind(<<"persist">>,  1, KeyValues),
            % TODO: limit login attempts
            % probably with a table of login attempts?
            % could just use ETS, no need to persist it in mnesia
            User = ecf_user:get_user_by_name(Username),
            try_login(User, Password, Url, Req, State);
        <<"GET">> ->
            User = ecf_utils:check_user_session(Req0),
            Message = application:get_env(ecf, login_message,
                                          <<"Please login.">>),
            Html = ecf_generators:generate(login, User, {Message, Url}),
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



try_login({error, user_not_found}, _, Url, Req, State) ->
    login_fail(Url, Req, State);
try_login(User, Password, Url, Req, State) ->
    case ecf_user:check_pass(User, Password) of
        true ->
            Session = ecf_user:new_session(ecf_user:id(User)),
            SessionEncoded = base64:encode(Session),
            Req2 = cowboy_req:set_resp_cookie(<<"session">>, SessionEncoded,
                                              Req,
                                              #{http_only => true,
                                                max_age => ?SESSION_TIME}),
            Req3 = cowboy_req:set_resp_cookie(<<"user">>,
                                              integer_to_list(ecf_user:id(User)),
                                              Req2),
            Base = application:get_env(ecf, base_url, ""),
            Req4 = cowboy_req:reply(302,
                                    #{<<"Location">> => [Base, "/", Url]},
                                    Req3),
            {ok, Req4, State};
        false ->
            login_fail(Url, Req, State)
    end.

login_fail(Url, Req, State) ->
    Message = application:get_env(ecf, login_fail_message,
                                  <<"Login Failed! Try again">>),
    Req2 = cowboy_req:reply(400,
                            #{<<"content-type">> => <<"text/html">>},
                            ecf_generators:generate(login, undefined,
                                                    {Message, Url}),
                            Req),
    {ok, Req2, State}.

