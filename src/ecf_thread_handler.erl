-module(ecf_thread_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = case cowboy_req:method(Req) of
               <<"GET">> ->
                   Id = cowboy_req:binding(id, Req, -1),
                   handle_get(Req, User, Id);
               <<"POST">> ->
                   Action = cowboy_req:binding(action, Req),
                   handle_post(Req, User, Action)
           end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_get(Req, User, Id) ->
    case ecf_thread:get_thread(Id) of
        undefined ->
            ecf_utils:reply_status(404, User, false, Req);
        Thread ->
            case ecf_perms:check_perm_thread(User, Thread, view_thread) of
                true ->
                    Forum = ecf_forum:get_forum( ecf_thread:forum(Thread)),
                    Posts = ecf_post:get_posts(Id),
                    Html = ecf_generators:generate(thread, User,
                                                   {Forum, Thread, Posts}),
                    cowboy_req:reply(200,
                                     #{<<"content-type">> => <<"text/html">>},
                                     Html, Req);
                false ->
                    ecf_utils:reply_status(403, User, view_thread_403, Req)
            end
    end.


handle_post(Req, undefined, _Action) ->
    ecf_utils:reply_status(401, undefined, thread_401, Req);
handle_post(Req, User, undefined) ->
    ecf_utils:reply_status(400, User, thread_400, Req);
handle_post(Req0, User, <<"create">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Forum0} = lists:keyfind(<<"forum">>, 1, KV),
    Forum = binary_to_integer(Forum0),
    case ecf_perms:check_perm_forum(User, ecf_forum:get_forum(Forum),
                                    create_thread) of
        true ->
            Limit = application:get_env(ecf, post_limit_seconds, 20) * 1000000,
            Time = timer:now_diff(erlang:timestamp(), ecf_user:last_post(User)),
            case Time > Limit of
                true ->
                    {_, Title} = lists:keyfind(<<"title">>, 1, KV),
                    {_, Text} = lists:keyfind(<<"text">>, 1, KV),
                    Id = ecf_thread:create_thread(Forum, Title,
                                                  erlang:timestamp(),
                                                  ecf_user:id(User),
                                                  Text),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303,
                                     #{<<"location">> => [Base, "/thread/",
                                                          integer_to_list(Id)]},
                                     Req);
                false ->
                    ecf_utils:reply_status(429, User, create_thread_429, Req, true)
            end;
        false ->
            ecf_utils:reply_status(403, User, create_thread_403, Req)
    end;
handle_post(Req0, User, <<"delete">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Id0} = lists:keyfind(<<"id">>, 1, KV),
    Id = binary_to_integer(Id0),
    case ecf_thread:get_thread(Id) of
        undefined ->
            ecf_utils:reply(400, User, thread_400, Req);
        Thread ->
            case ecf_perms:check_perm_thread(User, Thread, delete_thread) of
                true ->
                    ok = ecf_thread:delete_thread(Id),
                    Base = application:get_env(ecf, base_url, ""),
                    Forum = ecf_thread:forum(Thread),
                    cowboy_req:reply(303,
                                     #{<<"location">>
                                       => [Base, "/forum/",
                                           integer_to_list(Forum)]},
                                     Req);
                false ->
                    ecf_utils:reply(403, User, delete_thread_403, Req)
            end
    end;
handle_post(Req0, User, <<"edit">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Id0} = lists:keyfind(<<"id">>, 1, KV),
    Id = binary_to_integer(Id0),
    case ecf_thread:get_thread(Id) of
        undefined ->
            ecf_utils:reply(400, User, thread_400, Req);
        Thread ->
            case ecf_perms:check_perm_thread(User, Thread, edit_thread) of
                true ->
                    {_, Title} = lists:keyfind(<<"title">>, 1, KV),
                    ok = ecf_thread:edit_title(Id, Title),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303,
                                     #{<<"location">> => [Base, "/thread/",
                                                          Id0]},
                                     Req);
                false ->
                    ecf_utils:reply(403, User, edit_thread_403, Req)
            end
    end.

