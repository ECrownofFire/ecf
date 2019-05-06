-module(ecf_msg_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).


init(Req = #{method := <<"POST">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Action = cowboy_req:binding(action, Req),
    Req2 = do_reply(User, Action, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.


do_reply(undefined, _, Req) ->
    ecf_utils:reply_status(401, undefined, msg_401, Req);
do_reply(User, Action, Req) ->
    case Action of
        <<"create">> ->
            create(User, Req);
        <<"add">> ->
            do_perm(add, User, Req);
        <<"remove">> ->
            do_perm(remove, User, Req)
    end.


create(User, Req0) ->
    L = [{to, nonempty}, subject, text],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{to := ToName, subject := Subject, text := Text} = M,
    Forum = ecf_forum:get_forum(0),
    From = ecf_user:id(User),
    case ecf_user:get_user_by_name(ToName) of
        undefined ->
            ecf_utils:reply_status(400, User, invalid_user, Req, true);
        To ->
            case ecf_thread:create(User, Forum, Subject, erlang:timestamp(),
                                   Text) of
                post_limit ->
                    ecf_utils:reply_status(429, User, create_thread_429, Req,
                                           true);
                denied ->
                    ecf_utils:reply_status(403, User, create_thread_403, Req);
                Id ->
                    ToId = ecf_user:id(To),
                    ecf_thread:edit_perm(Id, {user, From}, view_thread, allow),
                    ecf_thread:edit_perm(Id, {user, ToId}, view_thread, allow),
                    ecf_thread:edit_perm(Id, {user, From}, create_post, allow),
                    ecf_thread:edit_perm(Id, {user, ToId}, create_post, allow),
                    ecf_thread:edit_perm(Id, {user, From}, edit_perms, allow),
                    reply(Id, Req)
            end
    end.


do_perm(Act, User, Req0) ->
    L = [{id, int}, {user, [], <<"">>}, {uid, int, 0}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{id := ThreadId} = M,
    case check_perm(User, ThreadId, Req) of
        ok ->
            case get_user(Act, M) of
                undefined ->
                    ecf_utils:reply_status(400, User, invalid_user, Req);
                NewUser ->
                    Id = ecf_user:id(NewUser),
                    case ecf_user:id(User) of
                        Id -> % stop users from removing themselves
                            reply(ThreadId, Req);
                        _ ->
                            case Act of
                                add ->
                                    do_add(Id, ThreadId);
                                remove ->
                                    do_remove(Id, ThreadId)
                            end,
                            reply(ThreadId, Req)
                    end
            end;
        Req2 ->
            Req2
    end.

get_user(add, M) ->
    ecf_user:get_user_by_name(maps:get(user, M));
get_user(remove, M) ->
    ecf_user:get_user(maps:get(uid, M)).

check_perm(User, ThreadId, Req) ->
    case ecf_thread:get_thread(ThreadId) of
        undefined ->
            ecf_utils:reply_status(404, User, thread_404, Req);
        Thread ->
            case ecf_perms:check_perm_thread(User, Thread, edit_perms) of
                false ->
                    ecf_utils:reply_status(403, User, msg_perms_403, Req);
                true ->
                    ok
            end
    end.

do_add(Id, Thread) ->
    ecf_thread:edit_perm(Thread, {user, Id}, view_thread, allow),
    ecf_thread:edit_perm(Thread, {user, Id}, create_post, allow).

do_remove(Id, Thread) ->
    ecf_thread:remove_perm(Thread, {user, Id}, view_thread),
    ecf_thread:remove_perm(Thread, {user, Id}, create_post).

reply(Id, Req) ->
    ecf_utils:reply_redirect(303, ["/thread/", integer_to_binary(Id)], Req).

