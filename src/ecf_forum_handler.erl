-module(ecf_forum_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).


init(Req, State) ->
    Id = cowboy_req:binding(id, Req, -1),
    User = ecf_utils:check_user_session(Req),
    Req2 = handle_reply(User, Id, Req, ecf_forum:get_forum(Id)),
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.

handle_reply(User, -1, Req = #{method := <<"PUT">>}, _) ->
    case ecf_perms:check_perm_global(User, create_forum) of
        false ->
            cowboy_req:reply(403, Req);
        true ->
            {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
            Name = ecf_utils:get_and_sanitize(KV, <<"name">>),
            Desc = ecf_utils:get_and_sanitize(KV, <<"desc">>),
            Id = ecf_forum:new_forum(Name, Desc, 0),
            cowboy_req:reply(201,
                             #{<<"content-location">>
                               => [<<"{{base}}/forum/">>,integer_to_list(Id)]},
                             Req2)
    end;
handle_reply(_User, _Id, Req = #{method := <<"PUT">>}, _) ->
    cowboy_req:reply(409, Req);
handle_reply(User, Id, Req = #{method := <<"PATCH">>}, Forum) ->
    case ecf_perms:check_perm_forum(User, Forum, edit_forum) of
        false ->
            cowboy_req:reply(403, Req);
        true ->
            {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
            Name = ecf_utils:get_and_sanitize(KV, <<"name">>),
            Desc = ecf_utils:get_and_sanitize(KV, <<"desc">>),
            ok = ecf_forum:edit_name(Id, Name),
            ok = ecf_forum:edit_desc(Id, Desc),
            cowboy_req:reply(204,
                             #{<<"content-location">>
                               => [<<"{{base}}/forum/">>,integer_to_list(Id)]},
                             Req2)
    end;
handle_reply(User, _Id, Req, {error, forum_not_found}) ->
    ecf_utils:reply_status(404, User, ignored, Req);
handle_reply(User, Id, Req = #{method := <<"GET">>}, Forum) ->
    case ecf_perms:check_perm_forum(User, Forum, view_forum) of
        false ->
            ecf_utils:reply_status(403, User, view_forum_403, Req);
        true ->
            Threads = ecf_thread:get_forum_threads(Id),
            Html = ecf_generators:generate(forum, User, {Forum, Threads}),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                             Html, Req)
    end;
handle_reply(User, Id, Req = #{method := <<"DELETE">>}, Forum) ->
    case ecf_perms:check_perm_forum(User, Forum, delete_forum) of
        false ->
            cowboy_req:reply(403, Req);
        true ->
            ok = ecf_forum:delete_forum(Id),
            cowboy_req:reply(204, Req)
    end.

