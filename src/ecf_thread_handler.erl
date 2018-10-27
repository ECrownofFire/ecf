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
                    ecf_thread:view_thread(Id),
                    #{page := Page} = cowboy_req:match_qs([{page, int, 1}], Req),
                    PerPage = application:get_env(ecf, posts_per_page, 40),
                    Forum = ecf_forum:get_forum(ecf_thread:forum(Thread)),
                    First = (Page-1) * PerPage + 1,
                    Posts = ecf_post:get_posts(Id, First, First + PerPage - 1),
                    LastPage = (ecf_thread:last(Thread) - 1) div PerPage + 1,
                    Html = ecf_generators:generate(thread, User,
                                                   {Forum, Thread, Posts,
                                                    Page, LastPage}),
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
            Base = application:get_env(ecf, base_url, ""),
            cowboy_req:reply(303,
                             #{<<"location">> => [Base, "/thread/",
                                                  integer_to_list(Id)]},
                             Req)
    end;
handle_post(Req0, User, <<"delete">>) ->
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body([{id, int}], Req0),
    #{id := Id} = M,
    case ecf_thread:get_thread(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, thread_400, Req);
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
                    ecf_utils:reply_status(403, User, delete_thread_403, Req)
            end
    end;
handle_post(Req0, User, <<"edit">>) ->
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body([{id, int}, title], Req0),
    #{id := Id, title := Title} = M,
    case ecf_thread:get_thread(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, thread_400, Req);
        Thread ->
            case ecf_perms:check_perm_thread(User, Thread, edit_thread) of
                true ->
                    ok = ecf_thread:edit_title(Id, Title),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303,
                                     #{<<"location">> => [Base, "/thread/",
                                                          integer_to_binary(Id)]},
                                     Req);
                false ->
                    ecf_utils:reply_status(403, User, edit_thread_403, Req)
            end
    end.

