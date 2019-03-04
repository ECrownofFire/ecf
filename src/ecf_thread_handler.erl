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
            Perm = ecf_perms:check_perm_thread(User, Thread, view_thread),
            Post = cowboy_req:binding(post, Req),
            do_get(Req, User, Thread, Perm, Post)
    end.

do_get(Req, User, _, false, _) ->
    ecf_utils:reply_status(403, User, view_thread_403, Req);
do_get(Req, User, Thread, true, undefined) ->
    Id = ecf_thread:id(Thread),
    ecf_thread:view_thread(ecf_thread:id(Thread)),
    #{page := Page} = cowboy_req:match_qs([{page, int, 1}], Req),
    PerPage = application:get_env(ecf, posts_per_page, 40),
    Forum = ecf_forum:get_forum(ecf_thread:forum(Thread)),
    First = (Page-1) * PerPage + 1,
    Posts = ecf_post:get_posts(Id, First, First + PerPage - 1),
    LastPage = (ecf_thread:last(Thread) - 1) div PerPage + 1,
    Html = ecf_generators:generate(thread, User,
                                   {Forum, Thread, Posts, Page, LastPage}),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                     Html, Req);
do_get(Req, _, Thread, true, Post) ->
    Id = ecf_thread:id(Thread),
    PerPage = application:get_env(ecf, posts_per_page, 40),
    Page = (Post-1) div PerPage + 1,
    Base = application:get_env(ecf, base_url, ""),
    cowboy_req:reply(301, #{<<"location">>
                            => [Base, "/thread/", integer_to_binary(Id),
                                "?page=", integer_to_binary(Page),
                                "#post-", integer_to_binary(Post)]},
                     Req).


handle_post(Req, undefined, _Action) ->
    ecf_utils:reply_status(401, undefined, thread_401, Req);
handle_post(Req, User, undefined) ->
    ecf_utils:reply_status(400, User, thread_400, Req);
handle_post(Req0, User, <<"create">>) ->
    L = [{forum, int}, title, text],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{forum := ForumId, title := Title, text := Text} = M,
    Forum = ecf_forum:get_forum(ForumId),
    case ecf_thread:create(User, Forum, Title, erlang:timestamp(), Text) of
        post_limit ->
            ecf_utils:reply_status(429, User, create_thread_429, Req, true);
        denied ->
            ecf_utils:reply_status(403, User, create_thread_403, Req);
        Id ->
            do_303(Req, Id)
    end;
handle_post(Req0, User, <<"delete">>) ->
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body([{id, int}], Req0),
    #{id := Id} = M,
    case get_and_check_perm(Req, Id, User, delete_thread) of
        {ok, Thread} ->
            ok = ecf_thread:delete_thread(Id),
            Base = application:get_env(ecf, base_url, ""),
            Forum = ecf_thread:forum(Thread),
            cowboy_req:reply(303, #{<<"location">> => [Base, "/forum/",
                                                       integer_to_list(Forum)]},
                             Req);
        R ->
            R
    end;
handle_post(Req0, User, <<"edit">>) ->
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body([{id, int}, title], Req0),
    #{id := Id, title := Title} = M,
    case get_and_check_perm(Req, Id, User, edit_thread) of
        {ok, _Thread} ->
            ok = ecf_thread:edit_title(Id, Title),
            do_303(Req, Id);
        R ->
            R
    end;
handle_post(Req0, User, <<"pin">>) ->
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body([{id, int}], Req0),
    #{id := Id} = M,
    case get_and_check_perm(Req, Id, User, pin_thread) of
        {ok, Thread} ->
            ok = ecf_forum:pin_thread(ecf_thread:forum(Thread), Id),
            do_303(Req, Id);
        R ->
            R
    end;
handle_post(Req0, User, <<"unpin">>) ->
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body([{id, int}], Req0),
    #{id := Id} = M,
    case get_and_check_perm(Req, Id, User, pin_thread) of
        {ok, Thread} ->
            ok = ecf_forum:unpin_thread(ecf_thread:forum(Thread), Id),
            do_303(Req, Id);
        R ->
            R
    end;
handle_post(Req0, User, <<"tag">>) ->
    L = [{id, int}, tag],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{id := Id, tag := Tag} = M,
    case get_and_check_perm(Req, Id, User, tag_thread) of
        {ok, Thread} ->
            case ecf_tag:tag_thread(Tag, Id) of
                ok ->
                    do_303(Req, Id);
                % if the user has permission to create tags, do it anyway
                {error, tag_not_found} ->
                    case check_perm(Req, User, Thread, create_tag) of
                        ok ->
                            ok = ecf_tag:new_tag(Tag),
                            ok = ecf_tag:tag_thread(Tag, Id),
                            do_303(Req, Id);
                        E ->
                            E
                    end
            end;
        R ->
            R
    end;
handle_post(Req0, User, <<"untag">>) ->
    L = [{id, int}, tag],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{id := Id, tag := Tag} = M,
    case get_and_check_perm(Req, Id, User, tag_thread) of
        {ok, _Thread} ->
            ok = ecf_tag:untag_thread(Tag, Id),
            do_303(Req, Id);
        R ->
            R
    end.

get_and_check_perm(Req, Id, User, Mode) ->
    case ecf_thread:get_thread(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, thread_400, Req);
        Thread ->
            case check_perm(Req, User, Thread, Mode) of
                ok ->
                    {ok, Thread};
                R -> R
            end
    end.

check_perm(Req, User, Thread, Mode) ->
    case ecf_perms:check_perm_thread(User, Thread, Mode) of
        false ->
            % view_thread -> view_thread_403
            Fail0 = atom_to_binary(Mode, latin1),
            Fail = binary_to_atom(<<Fail0/binary,"_403">>, latin1),
            ecf_utils:reply_status(403, User, Fail, Req);
        true ->
            ok
    end.

do_303(Req, Id) ->
    Base = application:get_env(ecf, base_url, ""),
    cowboy_req:reply(303,
                     #{<<"location">> => [Base, "/thread/", integer_to_binary(Id)]},
                     Req).

