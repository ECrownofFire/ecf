-module(ecf_thread_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    case maps:get(method, Req) of
        <<"POST">> ->
            post_reply(Req, State, User);
        _ ->
            Req2 = ecf_utils:reply_status(404, User, ignored, Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

post_reply(Req, State, undefined) ->
    Req2 = ecf_utils:reply_status(401, undefined, create_thread_401, Req),
    {ok, Req2, State};
post_reply(Req, State, User) ->
    {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
    {_, Forum0} = lists:keyfind(<<"forum">>, 1, KV),
    Forum = binary_to_integer(Forum0),
    case ecf_perms:check_perm_forum(User, ecf_forum:get_forum(Forum),
                                    create_thread) of
        true ->
            Limit = application:get_env(ecf, post_limit_seconds, 30) * 1000000,
            Time = timer:now_diff(erlang:timestamp(), ecf_user:last_post(User)),
            case Limit > Time of
                true ->
                    Title = ecf_utils:get_and_sanitize(KV, <<"title">>),
                    Text = ecf_utils:get_and_sanitize(KV, <<"text">>),
                    Thread = ecf_thread:create_thread(Forum, Title,
                                                      erlang:timestamp(),
                                                      ecf_user:id(User),
                                                      Text),
                    ThreadId = integer_to_list(ecf_thread:id(Thread)),
                    Req3 = cowboy_req:reply(303,
                                            #{<<"Location">> =>
                                              [<<"{{base}}/thread/">>,
                                               ThreadId]},
                                            Req2),
                    {ok, Req3, State};
                false ->
                    Req3 = ecf_utils:reply_status(429, User,
                                                  create_thread_429, Req2),
                    {ok, Req3, State}
            end;
        false ->
            Req3 = ecf_utils:reply_status(403, User, create_thread_403, Req2),
            {ok, Req3, State}
    end.

