-module(ecf_group_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = case cowboy_req:binding(id, Req, -1) of
               -1 ->
                   Groups = ecf_group:filter_groups(User, ecf_group:get_groups()),
                   Html = ecf_generators:generate(groups, User, Groups),
                   cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    Html, Req);
               Id ->
                   case ecf_group:get_group(Id) of
                       undefined ->
                           ecf_utils:reply_status(404, User, false, Req);
                       Group ->
                           case ecf_perms:check_perm_group(User, Group,
                                                           view_group) of
                               true ->
                                   Html = ecf_generators:generate(group, User, Group),
                                   cowboy_req:reply(200, #{<<"content-type">>
                                                           => <<"text/html">>},
                                                    Html, Req);
                               false ->
                                   ecf_utils:reply_status(403, User,
                                                          view_group_403, Req)
                           end
                   end
           end,
    {ok, Req2, State};
init(Req = #{method := <<"POST">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = handle_post(User, Req, cowboy_req:binding(action, Req)),
    {ok, Req2, State}.

handle_post(undefined, Req, _) ->
    ecf_utils:reply_status(401, undefined, group_401, Req);
handle_post(User, Req, undefined) ->
    ecf_utils:reply_status(404, User, false, Req);
handle_post(User, Req, <<"create">>) ->
    case ecf_perms:check_perm_global(User, create_group) of
        true ->
            {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
            {_, Name} = lists:keyfind(<<"name">>, 1, KV),
            {_, Desc} = lists:keyfind(<<"desc">>, 1, KV),
            Id = ecf_group:new_group(Name, Desc),
            Base = application:get_env(ecf, base_url, ""),
            cowboy_req:reply(303, #{<<"location">>
                                    => [Base, "/group/", integer_to_list(Id)]},
                             Req2);
        false ->
            ecf_utils:reply_status(403, User, create_group_403, Req)
    end;
handle_post(User, Req0, <<"delete">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Id0} = lists:keyfind(<<"id">>, 1, KV),
    Id = binary_to_integer(Id0),
    case ecf_group:get_group(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, invalid_group, Req);
        Group ->
            case Id >= 3
                 andalso ecf_perms:check_perm_group(User, Group, delete_group) of
                true ->
                    ok = ecf_group:delete_group(Id),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303, #{<<"location">>
                                            => [Base, "/group"]},
                                     Req);
                false ->
                    ecf_utils:reply_status(403, User, delete_group_403, Req)
            end
    end;
handle_post(User, Req0, <<"edit">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Id0} = lists:keyfind(<<"id">>, 1, KV),
    Id = binary_to_integer(Id0),
    {_, Name} = lists:keyfind(<<"name">>, 1, KV),
    {_, Desc} = lists:keyfind(<<"desc">>, 1, KV),
    case ecf_group:get_group(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, invalid_group, Req);
        Group ->
            case ecf_perms:check_perm_group(User, Group, edit_group) of
                true ->
                    ok = ecf_group:edit_name(Id, Name),
                    ok = ecf_group:edit_desc(Id, Desc),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303, #{<<"location">>
                                            => [Base, "/group/", Id0]},
                                     Req);
                false ->
                    ecf_utils:reply_status(403, User, edit_group_403, Req)
            end
    end;
handle_post(User, Req0, <<"join">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Id0} = lists:keyfind(<<"id">>, 1, KV),
    Id = binary_to_integer(Id0),
    case ecf_group:get_group(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, invalid_group, Req);
        Group ->
            case Id >= 3
                 andalso ecf_perms:check_perm_group(User, Group, join_group) of
                true ->
                    ok = ecf_group:add_member(Id, ecf_user:id(User)),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303, #{<<"location">>
                                            => [Base, "/group/", Id0]},
                                     Req);
                false ->
                    ecf_utils:reply_status(403, User, join_group_403, Req)
            end
    end;
handle_post(User, Req0, <<"leave">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Id0} = lists:keyfind(<<"id">>, 1, KV),
    Id = binary_to_integer(Id0),
    case ecf_group:get_group(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, invalid_group, Req);
        Group ->
            case Id >= 3
                 andalso ecf_perms:check_perm_group(User, Group, leave_group) of
                true ->
                    ok = ecf_group:remove_member(Id, ecf_user:id(User)),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303, #{<<"location">>
                                            => [Base, "/group/", Id0]},
                                     Req);
                false ->
                    ecf_utils:reply_status(403, User, leave_group_403, Req)
            end
    end;
handle_post(User, Req0, <<"add">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Id0} = lists:keyfind(<<"id">>, 1, KV),
    Id = binary_to_integer(Id0),
    {_, U0} = lists:keyfind(<<"user">>, 1, KV),
    U = binary_to_integer(U0),
    case ecf_group:get_group(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, invalid_group, Req);
        Group ->
            case Id =/= 1
                 andalso ecf_perms:check_perm_group(User, Group, manage_group) of
                true ->
                    case ecf_user:get_user(U) of
                        undefined ->
                            ecf_utils:reply_status(400, User, invalid_user, Req);
                        _ ->
                            ok = ecf_group:add_member(Id, U),
                            Base = application:get_env(ecf, base_url, ""),
                            cowboy_req:reply(303, #{<<"location">>
                                                    => [Base, "/user/", U0]},
                                             Req)
                    end;
                false ->
                    ecf_utils:reply_status(403, User, manage_group_403, Req)
            end
    end;
handle_post(User, Req0, <<"remove">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Id0} = lists:keyfind(<<"id">>, 1, KV),
    Id = binary_to_integer(Id0),
    {_, U0} = lists:keyfind(<<"user">>, 1, KV),
    U = binary_to_integer(U0),
    case ecf_group:get_group(Id) of
        undefined ->
            ecf_utils:reply_status(400, User, invalid_group, Req);
        Group ->
            case Id =/= 1
                 andalso ecf_perms:check_perm_group(User, Group, manage_group) of
                true ->
                    case ecf_user:get_user(U) of
                        undefined ->
                            ecf_utils:reply_status(400, User, invalid_user, Req);
                        _ ->
                            ok = ecf_group:remove_member(Id, U),
                            Base = application:get_env(ecf, base_url, ""),
                            cowboy_req:reply(303, #{<<"location">>
                                                    => [Base, "/user/", U0]},
                                             Req)
                    end;
                false ->
                    ecf_utils:reply_status(403, User, manage_group_403, Req)
            end
    end.

