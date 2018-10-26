-module(ecf_perms_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).


init(Req, State) ->
    Action = cowboy_req:binding(action, Req),
    Req2 = do_reply(ecf_utils:check_user_session(Req), Action, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

do_reply(undefined, _, Req) ->
    ecf_utils:reply_status(401, undefined, perms_401, Req);
do_reply(User, Action, Req0) ->
    L = [{class, fun ecf_perms:class_constraint/2}, {class_id, int, 0},
         {set, fun ecf_perms:set_constraint/2},
         {mode, fun ecf_perms:mode_constraint/2}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{set := Set, mode := Mode} = M,
    Class = get_class(M),
    case ecf_perms:check_perm_global(User, edit_perms) of
        true ->
            case Action of
                <<"remove">> ->
                    ecf_perms:remove_global_perm(Class, Mode);
                <<"add">> ->
                    ecf_perms:edit_global_perm(Class, Mode, Set)
            end,
            Base = application:get_env(ecf, base_url, ""),
            cowboy_req:reply(303,
                             #{<<"location">> => [Base, <<"/admin">>]},
                             Req);
        false ->
            ecf_utils:reply_status(403, User, edit_perms_403, Req)
    end.


%% Utilities
get_class(M) ->
    case maps:get(class, M) of
        user -> {user, maps:get(class_id, M)};
        group -> {group, maps:get(class_id, M)};
        others -> others
    end.

