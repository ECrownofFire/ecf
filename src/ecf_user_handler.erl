-module(ecf_user_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Id = cowboy_req:binding(id, Req, -1),
    Action = cowboy_req:binding(action, Req),
    Req2 = handle_get(Req, User, Id, Action),
    {ok, Req2, State};
init(Req = #{method := <<"POST">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Action = cowboy_req:binding(action, Req),
    Req2 = handle_post(Req, User, Action),
    {ok, Req2, State}.

handle_get(Req, User, -1, undefined) ->
    Html = ecf_generators:generate(user, User, User),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                     Html, Req);
handle_get(Req, User, Id, undefined) ->
    case ecf_user:get_user(Id) of
        undefined ->
            ecf_utils:reply_status(404, User, false, Req);
        Profile ->
            Html = ecf_generators:generate(user, User, Profile),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                             Html, Req)
    end;
handle_get(Req, undefined, _, <<"edit">>) ->
    ecf_utils:reply_status(401, undefined, edit_user_401, Req);
handle_get(Req, User, _, <<"edit">>) ->
    #{id := Id} = cowboy_req:match_qs([{id, int, ecf_user:id(User)}], Req),
    case ecf_user:get_user(Id) of
        undefined ->
            ecf_utils:reply_status(404, User, false, Req);
        Profile ->
            case ecf_user:id(User) =:= Id
                 orelse ecf_perms:check_perm_global(User, edit_user) of
                false ->
                    ecf_utils:reply_status(403, User, edit_user_403, Req);
                true ->
                    Html = ecf_generators:generate(user_edit, User, Profile),
                    cowboy_req:reply(200,
                                     #{<<"content-type">> => <<"text/html">>},
                                     Html, Req)
            end
    end;
handle_get(Req, User, _, _) ->
    ecf_utils:reply_status(404, User, false, Req).


handle_post(Req, User, undefined) ->
    ecf_utils:reply_status(405, User, user_405, Req);
handle_post(Req, undefined, _) ->
    ecf_utils:reply_status(401, undefined, user_401, Req);
handle_post(Req0, User, <<"edit">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Id0} = lists:keyfind(<<"id">>, 1, KV),
    Id = binary_to_integer(Id0),
    case ecf_user:id(User) =:= Id
         orelse ecf_perms:check_perm_global(User, edit_user) of
        true ->
            Bday = case lists:keyfind(<<"bday">>, 1, KV) of
                       {_, <<"">>} -> undefined;
                       {_, Bday0} -> iso8601:parse(Bday0);
                       _ -> undefined
                   end,
            {_, Bio} = lists:keyfind(<<"bio">>, 1, KV),
            {_, Loc} = lists:keyfind(<<"loc">>, 1, KV),
            ecf_user:edit_bday(Id, Bday),
            ecf_user:edit_bio(Id, Bio),
            ecf_user:edit_loc(Id, Loc),
            Base = application:get_env(ecf, base_url, ""),
            cowboy_req:reply(303,
                             #{<<"location">>
                               => [Base, "/user/", integer_to_list(Id)]},
                             Req);
        false ->
            ecf_utils:reply_status(403, User, edit_user_403, Req0)
    end.


