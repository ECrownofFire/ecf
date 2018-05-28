-module(ecf_thread_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    case maps:get(method, Req) of
        <<"POST">> ->
            case User of
                undefined ->
                    Html = ecf_generators:generate(401, undefined, new_thread_401),
                    Req2 = cowboy_req:reply(401,
                                            #{<<"content-type">> =>
                                              <<"text/html">>,
                                              <<"WWW-Authenticate">> =>
                                              <<"FormBased">>},
                                            Html,
                                            Req),
                    {ok, Req2, State};
                _ ->
                    {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
                    {_, Forum0} = lists:keyfind(<<"forum">>, 1, KV),
                    Forum = binary_to_integer(Forum0),
                    Title = ecf_utils:get_and_sanitize(KV, <<"title">>),
                    Text = ecf_utils:get_and_sanitize(KV, <<"text">>),
                    Thread = ecf_thread:create_thread(Forum, Title,
                                                      erlang:timestamp(),
                                                      ecf_user:id(User), Text),
                    Base = application:get_env(ecf, base_url, ""),
                    ThreadId = integer_to_list(ecf_thread:id(Thread)),
                    Req3 = cowboy_req:reply(302,
                                            #{<<"Location">> =>
                                              [Base, "/thread/", ThreadId]},
                                            Req2),
                    {ok, Req3, State}
            end;
        _ ->
            Html = ecf_generators:generate(405, User, thread),
            Req2 = cowboy_req:reply(405,
                                    #{<<"content-type">> => <<"text/html">>,
                                      <<"Allow">> => <<"POST">>},
                                    Html,
                                    Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
