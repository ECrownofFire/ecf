-module(ecf_thread_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = handle_reply(Req, User),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_reply(Req = #{method := <<"POST">>}, undefined) ->
    ecf_utils:reply_status(401, undefined, create_thread_401, Req);
handle_reply(Req = #{method := <<"POST">>}, User) ->
    {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
    {_, Forum0} = lists:keyfind(<<"forum">>, 1, KV),
    Forum = binary_to_integer(Forum0),
    case ecf_perms:check_perm_forum(User, ecf_forum:get_forum(Forum),
                                    create_thread) of
        true ->
            Limit = application:get_env(ecf, post_limit_seconds, 30) * 1000000,
            Time = timer:now_diff(erlang:timestamp(), ecf_user:last_post(User)),
            case Time > Limit of
                true ->
                    Title = ecf_utils:get_and_sanitize(KV, <<"title">>),
                    Text = ecf_utils:get_and_sanitize(KV, <<"text">>),
                    Thread = ecf_thread:create_thread(Forum, Title,
                                                      erlang:timestamp(),
                                                      ecf_user:id(User),
                                                      Text),
                    ThreadId = integer_to_list(ecf_thread:id(Thread)),
                    cowboy_req:reply(303,
                                     #{<<"Location">>
                                       => [<<"{{base}}/thread/">>, ThreadId]},
                                     Req2);
                false ->
                    ecf_utils:reply_status(429, User, create_thread_429, Req2)
            end;
        false ->
            ecf_utils:reply_status(403, User, create_thread_403, Req2)
    end;
handle_reply(Req = #{method := <<"PATCH">>}, User) ->
    Id = cowboy_req:binding(id, Req, -1),
    case ecf_thread:get_thread(Id) of
        {error, thread_not_found} ->
            cowboy_req:reply(404, Req);
        Thread ->
            case ecf_perms:check_perm_thread(User, Thread, edit_thread) of
                false ->
                    cowboy_req:reply(403, Req);
                true ->
                    {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
                    Title = ecf_utils:get_and_sanitize(KV, <<"title">>),
                    ok = ecf_thread:edit_title(Id, Title),
                    cowboy_req:reply(204, Req2)
            end
    end;
handle_reply(Req = #{method := <<"DELETE">>}, User) ->
    Id = cowboy_req:binding(id, Req, -1),
    case ecf_thread:get_thread(Id) of
        {error, thread_not_found} ->
            cowboy_req:reply(404, Req);
        Thread ->
            case ecf_perms:check_perm_thread(User, Thread, delete_thread) of
                false ->
                    cowboy_req:reply(403, Req);
                true ->
                    ok = ecf_thread:delete_thread(Id),
                    cowboy_req:reply(204, Req)
            end
    end;
handle_reply(Req = #{method := <<"GET">>}, User) ->
    Id = cowboy_req:binding(id, Req, -1),
    case ecf_thread:get_thread(Id) of
        {error, thread_not_found} ->
            ecf_utils:reply_status(404, User, ignored, Req);
        Thread ->
            case ecf_perms:check_perm_thread(User, Thread,
                                             view_thread) of
                true ->
                    Forum = ecf_forum:get_forum( ecf_thread:forum(Thread)),
                    Posts = ecf_post:get_posts(Id),
                    Html = ecf_generators:generate(thread, User,
                                                   {Forum, Thread, Posts}),
                    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                     Html, Req);
                false ->
                    ecf_utils:reply_status(403, User, view_thread_403, Req)
            end
    end;
handle_reply(Req, User) ->
    ecf_utils:reply_status(404, User, ignored, Req).

