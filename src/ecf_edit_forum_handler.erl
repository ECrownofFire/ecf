-module(ecf_edit_forum_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).


init(Req, State) ->
    Id = cowboy_req:binding(id, Req),
    User = ecf_utils:check_user_session(Req),
    case ecf_forum:get_forum(Id) of
        {error, forum_not_found} ->
            Req2 = ecf_utils:reply_status(404, User, ignored, Req),
            {ok, Req2, State};
        Forum ->
            case ecf_perms:check_perm_forum(User, Forum, edit_forum) of
                false ->
                    Req2 = ecf_utils:reply_status(403, User, edit_forum_403, Req),
                    {ok, Req2, State};
                true ->
                    case cowboy_req:method(Req) of
                        <<"GET">> ->
                            Html = ecf_generators:generate(edit_forum, User, Forum),
                            Req2 = cowboy_req:reply(200,
                                                    #{<<"content-type">> =>
                                                      <<"text/html">>},
                                                    Html, Req),
                            {ok, Req2, State};
                        <<"POST">> ->
                            Req2 = post_reply(Id, User, Req),
                            {ok, Req2, State}
                    end
            end
    end.


terminate(_Reason, _Req, _State) ->
    ok.

post_reply(Id, User, Req0) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    Name = ecf_utils:get_and_sanitize(KV, <<"name">>),
    Desc = ecf_utils:get_and_sanitize(KV, <<"desc">>),
    ok = ecf_forum:edit_name(Id, Name),
    ok = ecf_forum:edit_desc(Id, Desc),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                     ecf_generators:generate(edit_forum, User,
                                             ecf_forum:get_forum(Id)),
                     Req).

