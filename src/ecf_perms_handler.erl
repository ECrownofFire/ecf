-module(ecf_perms_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).


init(Req, State) ->
    Action = cowboy_req:binding(action, Req),
    Req2 = do_reply(ecf_utils:check_user_session(Req), Action, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% Internal functions
do_reply(undefined, _, Req) ->
    ecf_utils:reply_status(401, undefined, perms_401, Req);
do_reply(User, Act, Req) ->
    {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
    A = binary_to_atom(Act, latin1),
    do_edit(User, KV, A, Req2).

do_edit(User, KV, Set, Req) ->
    {Class, Mode} = perm_from_kv(KV),
    case ecf_perms:check_perm_global(User, edit_perms) of
        true ->
            case Set of
                remove ->
                    ecf_perms:remove_global_perm(Class, Mode);
                S when S =:= allow; S =:= deny ->
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

perm_from_kv(KV) ->
    {_, Class0} = lists:keyfind(<<"class">>, 1, KV),
    Class = case binary_to_existing_atom(Class0, latin1) of
                others -> others;
                C when C =:= user; C =:= group ->
                    {_, ClassId0} = lists:keyfind(<<"class_id">>, 1, KV),
                    ClassId = binary_to_integer(ClassId0),
                    {C, ClassId}
            end,
    {_, Mode0} = lists:keyfind(<<"mode">>, 1, KV),
    Mode = binary_to_existing_atom(Mode0, latin1),
    {Class, Mode}.

