-module(ecf_post_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    Action = cowboy_req:binding(action, Req),
    Req2 = try_post(Req, User, Action),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.


try_post(Req, undefined, _) ->
    ecf_utils:reply_status(401, undefined, post_401, Req);
try_post(Req = #{method := <<"GET">>}, User, <<"edit">>) ->
    #{thread := TId, post := Id} = cowboy_req:match_qs([{thread, int},
                                                        {post, int}], Req),
    case ecf_thread:get_thread(TId) of
        undefined ->
            ecf_utils:reply_status(404, User, thread_404, Req);
        Thread ->
            case ecf_post:get_post(TId, Id) of
                undefined ->
                    ecf_utils:reply_status(404, User, post_404, Req);
                Post ->
                    get_edit(User, Thread, Post, Req)
            end
    end;
try_post(Req0 = #{method := <<"POST">>}, User, <<"edit">>) ->
    L = [{thread, int}, {post, int}, text],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{thread := TId, post := Id, text := Text} = M,
    case ecf_thread:get_thread(TId) of
        undefined ->
            ecf_utils:reply_status(404, User, thread_404, Req);
        Thread ->
            case ecf_post:get_post(TId, Id) of
                undefined ->
                    ecf_utils:reply_status(404, User, post_404, Req);
                Post ->
                    post_edit(User, Thread, Post, Text, Req)
            end
    end;
try_post(Req0 = #{method := <<"POST">>}, User, <<"create">>) ->
    LimitSec = application:get_env(ecf, post_limit_seconds, 20),
    Limit = LimitSec * 1000000,
    Time = timer:now_diff(erlang:timestamp(), ecf_user:last_post(User)),
    case Time > Limit of
        true ->
            L = [{thread, int}, text],
            {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
            #{thread := Thread, text := Text} = M,
            case ecf_perms:check_perm_thread(User, ecf_thread:get_thread(Thread),
                                             create_post) of
                true ->
                    Post = ecf_post:new_post(Thread, ecf_user:id(User),
                                             erlang:timestamp(), Text),
                    redirect_post(Thread, Post, Req);
                false ->
                    ecf_utils:reply_status(403, User, create_post_403, Req)
            end;
        false ->
            ecf_utils:reply_status(429, User, create_post_429, Req0, true)
    end;
try_post(Req0 = #{method := <<"POST">>}, User, <<"delete">>) ->
    L = [{thread, int}, {post, int}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{thread := TId, post := Id} = M,
    case ecf_thread:get_thread(TId) of
        undefined ->
            ecf_utils:reply_status(400, User, thread_404, Req);
        Thread ->
            case ecf_perms:check_perm_thread(User, Thread, delete_post) of
                true ->
                    case Id of
                        N when N > 1 ->
                            ok = ecf_post:delete_post(TId, Id),
                            redirect_thread(TId, Req);
                        _ ->
                            ecf_utils:reply_status(400, User, delete_post_400, Req)
                    end;
                false ->
                    ecf_utils:reply_status(403, User, delete_post_403, Req)
            end
    end;
try_post(Req, User, _) ->
    ecf_utils:reply_status(405, User, post_405, Req).


get_edit(User, Thread, Post, Req) ->
    case check_edit_perms(User, Thread, Post) of
        false ->
            ecf_utils:reply_status(403, User, edit_post_403, Req);
        true ->
            Html = ecf_generators:generate(post_edit, User, {Thread, Post}),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                             Html, Req)
    end.

post_edit(User, Thread, Post, Text, Req) ->
    case check_edit_perms(User, Thread, Post) of
        false ->
            ecf_utils:reply_status(403, User, edit_post_403, Req);
        true ->
            TId = ecf_thread:id(Thread),
            Id = ecf_post:id(Post),
            ecf_post:edit_post(TId, Id, ecf_user:id(User),
                               erlang:timestamp(), Text),
            redirect_post(TId, Id, Req)
    end.

check_edit_perms(User, Thread, Post) ->
    ecf_perms:check_perm_thread(User, Thread, edit_post)
    orelse (ecf_post:poster(Post) =:= ecf_user:id(User)
            andalso ecf_perms:check_perm_thread(User, Thread, edit_own_post)).

redirect_thread(Thread, Req) ->
    ecf_utils:reply_redirect(303,
                             [<<"/thread/">>, integer_to_binary(Thread)],
                             Req).

redirect_post(Thread, Post, Req) ->
    ecf_utils:reply_redirect(303,
                             [<<"/thread/">>, integer_to_binary(Thread),
                              <<"/post/">>, integer_to_binary(Post)],
                             Req).

