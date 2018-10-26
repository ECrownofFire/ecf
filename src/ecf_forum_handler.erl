-module(ecf_forum_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).


init(Req = #{method := <<"GET">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Id = cowboy_req:binding(id, Req, -1),
    Req2 = handle_get(Req, User, Id),
    {ok, Req2, State};
init(Req = #{method := <<"POST">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Action = cowboy_req:binding(action, Req),
    Req2 = handle_post(Req, User, Action),
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.

handle_get(Req, User, -1) ->
    ecf_utils:reply_status(404, User, false, Req);
handle_get(Req, User, Id) ->
    case ecf_forum:get_forum(Id) of
        undefined ->
            ecf_utils:reply_status(404, User, false, Req);
        Forum ->
            case ecf_perms:check_perm_forum(User, Forum, view_forum) of
                false ->
                    ecf_utils:reply_status(403, User, view_forum_403, Req);
                true ->
                    #{page := Page} = cowboy_req:match_qs([{page, int, 1}], Req),
                    PerPage = application:get_env(ecf, threads_per_page, 40),
                    % TODO: threads per page could be a user preference?
                    Threads = ecf_thread:get_forum_threads(Id),
                    Threads2 = ecf_thread:visible_threads(Threads, User),
                    Threads3 = lists:sublist(Threads2,
                                             (Page-1) * PerPage + 1, PerPage),
                    LastPage = length(Threads2) div PerPage + 1,
                    Html = ecf_generators:generate(forum, User, {Forum, Threads3,
                                                                 Page, LastPage}),
                    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                     Html, Req)
            end
    end.


handle_post(Req, User, <<"create">>) ->
    case ecf_perms:check_perm_global(User, create_forum) of
        false ->
            ecf_utils:reply_status(403, User, create_forum_403, Req);
        true ->
            L = [name, desc],
            {ok, M, Req2} = cowboy_req:read_and_match_urlencoded_body(L, Req),
            #{name := Name, desc := Desc} = M,
            Id = ecf_forum:new_forum(Name, Desc),
            Base = application:get_env(ecf, base_url, ""),
            cowboy_req:reply(303,
                             #{<<"location">>
                               => [Base, <<"/forum/">>, integer_to_list(Id)]},
                             Req2)
    end;
handle_post(Req0, User, <<"delete">>) ->
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body([{id, int}], Req0),
    #{id := Id} = M,
    case ecf_forum:get_forum(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, forum_400, Req);
        Forum ->
            case ecf_perms:check_perm_forum(User, Forum, delete_forum) of
                false ->
                    ecf_utils:reply_status(403, User, delete_forum_403, Req);
                true ->
                    ok = ecf_forum:delete_forum(Id),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303,
                                     #{<<"location">> => [Base, "/"]},
                                     Req)
            end
    end;
handle_post(Req0, User, <<"edit">>) ->
    L = [{id, int}, name, desc],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{id := Id, name := Name, desc := Desc} = M,
    case ecf_forum:get_forum(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, forum_400, Req);
        Forum ->
            case ecf_perms:check_perm_forum(User, Forum, edit_forum) of
                false ->
                    ecf_utils:reply_status(403, User, edit_forum_403, Req);
                true ->
                    ok = ecf_forum:edit_name(Id, Name),
                    ok = ecf_forum:edit_desc(Id, Desc),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303,
                                     #{<<"location">>
                                       => [Base, <<"/forum/">>,
                                           integer_to_binary(Id)]},
                                     Req)
            end
    end;
handle_post(Req0, User, <<"reorder">>) ->
    L = [{id, int}, {action, fun reorder_con/2}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{id := Id, action := Action} = M,
    case ecf_forum:get_forum(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, forum_400, Req);
        _Forum ->
            case ecf_perms:check_perm_global(User, reorder_forums) of
                false ->
                    ecf_utils:reply_status(403, User, reorder_forum_403, Req);
                true ->
                    try ecf_forum:reorder(Id, Action) of
                        ok ->
                            Base = application:get_env(ecf, base_url, ""),
                            cowboy_req:reply(303,
                                             #{<<"location">>
                                               => [Base, <<"/admin">>]},
                                             Req)
                    catch
                        error:function_clause ->
                            ecf_utils:reply_status(400, User, reorder_forum_400,
                                                   Req)
                    end
            end
    end;
handle_post(Req, User, _) ->
    ecf_utils:reply_status(400, User, forum_400, Req).

reorder_con(forward, Action) ->
    case lists:member(Action, [<<"up">>, <<"down">>, <<"top">>, <<"bottom">>]) of
        true ->
            {ok, binary_to_atom(Action, latin1)};
        false ->
            {error, invalid_action}
    end;
reorder_con(reverse, Action) ->
    case lists:member(Action, [up, down, top, bottom]) of
        true ->
            {ok, atom_to_binary(Action, latin1)};
        false ->
            {error, invalid_action}
    end;
reorder_con(format_error, {invalid_action, Val}) ->
    io_lib:format("~p is not a valid reorder action.", [Val]).

