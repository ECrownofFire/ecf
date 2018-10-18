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
    {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
    case Action of
        <<"create">> ->
            create(User, KV, Req2);
        <<"add">> ->
            do_perm(add, User, KV, Req2);
        <<"remove">> ->
            do_perm(remove, User, KV, Req2)
    end.


create(User, KV, Req) ->
    Forum = ecf_forum:get_forum(0),
    From = ecf_user:id(User),
    case get_to(KV) of
        undefined ->
            ecf_utils:reply_status(400, User, invalid_user, Req, true);
        To ->
            Subject = get_subject(KV),
            Text = get_text(KV),
            case ecf_thread:create(User, Forum, Subject, erlang:timestamp(),
                                   Text) of
                post_limit ->
                    ecf_utils:reply_status(429, User, create_thread_429, Req,
                                           true);
                denied ->
                    ecf_utils:reply_status(403, User, create_thread_403, Req);
                Id ->
                    ecf_thread:edit_perm(Id, {user, From}, view_thread, allow),
                    ecf_thread:edit_perm(Id, {user, To}, view_thread, allow),
                    ecf_thread:edit_perm(Id, {user, From}, create_post, allow),
                    ecf_thread:edit_perm(Id, {user, To}, create_post, allow),
                    ecf_thread:edit_perm(Id, {user, From}, edit_perms, allow),
                    reply(Id, Req)
            end
    end.


do_perm(Act, User, KV, Req) ->
    {_, Thread0} = lists:keyfind(<<"id">>, 1, KV),
    ThreadId = binary_to_integer(Thread0),
    case check_perm(User, ThreadId, Req) of
        ok ->
            {_, New} = lists:keyfind(<<"user">>, 1, KV),
            case ecf_user:get_user_by_name(New) of
                undefined ->
                    ecf_utils:reply_status(400, User, invalid_user, Req, true);
                NewUser ->
                    Id = ecf_user:id(NewUser),
                    case Act of
                        add ->
                            do_add(Id, ThreadId);
                        remove ->
                            do_remove(Id, ThreadId)
                    end,
                    reply(ThreadId, Req)
            end;
        Req2 ->
            Req2
    end.

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
    Base = application:get_env(ecf, base_url, ""),
    cowboy_req:reply(303,
                     #{<<"location">> => [Base, "/thread/", integer_to_list(Id)]},
                     Req).

get_to(KV) ->
    {_, To} = lists:keyfind(<<"to">>, 1, KV),
    case ecf_user:get_user_by_name(To) of
        undefined ->
            undefined;
        User ->
            ecf_user:id(User)
    end.

get_subject(KV) ->
    {_, Subject} = lists:keyfind(<<"subject">>, 1, KV),
    Subject.

get_text(KV) ->
    {_, Text} = lists:keyfind(<<"text">>, 1, KV),
    Text.

