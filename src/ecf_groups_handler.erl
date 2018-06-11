-module(ecf_groups_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).


init(Req, State) ->
    Req2 = do_reply(ecf_utils:check_user_session(Req), Req),
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.

do_reply(undefined, Req) ->
    ecf_utils:reply_status(401, undefined, view_groups_401, Req);
do_reply(User, Req = #{method := <<"GET">>}) ->
    Groups = ecf_group:filter_groups(User, ecf_group:get_groups()),
    Html = ecf_generators:generate(groups, User, Groups),
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"text/html">>},
                     Html,
                     Req);
do_reply(User, Req = #{method := <<"POST">>}) ->
    {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
    {_, Id0} = lists:keyfind(<<"id">>, 1, KV),
    case binary_to_integer(Id0) of
        I when I =< 1 -> cowboy_req:reply(400, Req2);
        Id ->
            {_, Action} = lists:keyfind(<<"action">>, 1, KV),
            case Action of
                <<"Join">> ->
                    case ecf_perms:check_perm_group(User,
                                                    ecf_group:get_group(Id),
                                                    join_group) of
                        true ->
                            ok = ecf_group:add_member(Id, ecf_user:id(User)),
                            cowboy_req:reply(204, Req2);
                        false ->
                            cowboy_req:reply(403, Req2)
                    end;
                <<"Leave">> ->
                    case ecf_perms:check_perm_group(User,
                                                    ecf_group:get_group(Id),
                                                    leave_group) of
                        true ->
                            ok = ecf_group:remove_member(Id, ecf_user:id(User)),
                            cowboy_req:reply(204, Req2);
                        false ->
                            cowboy_req:reply(403, Req2)
                    end
            end
    end.

