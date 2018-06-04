-module(ecf_post_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    case ecf_utils:check_user_session(Req) of
        undefined ->
            Req2 = ecf_utils:reply_status(401, undefined, new_post_401, Req),
            {ok, Req2, State};
        User ->
            % convert to microseconds
            Limit = application:get_env(ecf, post_limit_seconds, 30) * 1000000,
            Time = timer:now_diff(erlang:timestamp(), ecf_user:last_post(User)),
            try_post(Req, User, State, Time > Limit)
    end.

terminate(_Reason, _Req, _State) ->
    ok.


try_post(Req, User, State, true) ->
    case maps:get(method, Req) of
        <<"POST">> ->
            {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
            {_, Thread0} = lists:keyfind(<<"thread">>, 1, KV),
            Thread = binary_to_integer(Thread0),
            case ecf_perms:check_perm_thread(User,
                                             ecf_thread:get_thread(Thread),
                                             create_post) of
                true ->
                    Text = ecf_utils:get_and_sanitize(KV, <<"text">>),
                    Post = ecf_post:new_post(Thread, ecf_user:id(User),
                                             erlang:timestamp(), Text),
                    Thread2 = integer_to_list(Thread),
                    Req3 = cowboy_req:reply(303,
                                            #{<<"Location">> =>
                                              [<<"{{base}}/thread/">>, Thread2,
                                               <<"#post-">>, integer_to_list(Post)]},
                                            Req2),
                    {ok, Req3, State};
                false ->
                    Req2 = ecf_utils:reply_status(403, User,
                                                  create_post_403, Req),
                    {ok, Req2, State}
            end;
        _ ->
            Req2 = ecf_utils:reply_status(404, User, ignored, Req),
            {ok, Req2, State}
    end;
try_post(Req, User, State, false) ->
    Req2 = ecf_utils:reply_status(429, User, post_limit_message, Req),
    {ok, Req2, State}.

