-module(ecf_user_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = handle_get(Req, User, cowboy_req:binding(action, Req)),
    {ok, Req2, State};
init(Req = #{method := <<"POST">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Id = cowboy_req:binding(id, Req, -1),
    Req2 = handle_post(Req, User, Id, cowboy_req:binding(action, Req)),
    {ok, Req2, State}.

handle_get(Req, User, undefined) ->
    case cowboy_req:binding(id, Req, -1) of
        -1 ->
            Html = ecf_generators:generate(user, User, User),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                             Html, Req);
        Id ->
            case ecf_user:get_user(Id) of
                undefined ->
                    ecf_utils:reply_status(404, User, false, Req);
                Profile ->
                    Html = ecf_generators:generate(user, User, Profile),
                    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                     Html, Req)
            end
    end;
handle_get(Req, undefined, <<"edit">>) ->
    ecf_utils:reply_status(401, undefined, edit_user_401, Req);
handle_get(Req, User, <<"edit">>) ->
    case cowboy_req:binding(id, Req, -1) of
        -1 ->
            Html = ecf_generators:generate(user_edit, User, User),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                             Html, Req);
        Id ->
            case ecf_user:get_user(Id) of
                undefined ->
                    ecf_utils:reply(404, User, false, Req);
                Profile ->
                    case ecf_perms:check_perm_global(User, edit_user) of
                        false ->
                            ecf_utils:reply(403, User, edit_user_403, Req);
                        true ->
                            Html = ecf_generators:generate(user_edit,
                                                           User, Profile),
                            cowboy_req:reply(200,
                                             #{<<"content-type">>
                                               => <<"text/html">>},
                                             Html, Req)
                    end
            end
    end;
handle_get(Req, User, _) ->
    ecf_utils:reply_status(404, User, false, Req).


handle_post(Req, User, _, undefined) ->
    ecf_utils:reply_status(405, User, user_405, Req);
handle_post(Req, undefined, _, _) ->
    ecf_utils:reply_status(401, undefined, user_401, Req);
handle_post(Req, User, -1, <<"edit">>) ->
    ecf_utils:reply_status(400, User, edit_user_401, Req);
handle_post(Req0, User, Id, <<"edit">>) ->
    case ecf_user:id(User) =:= Id
         orelse ecf_perms:check_perm_global(User, edit_user) of
        true ->
            {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
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


