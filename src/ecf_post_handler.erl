-module(ecf_post_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    Action = cowboy_req:binding(action, Req),
    Req2 = try_post(Req, User, Action),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.


try_post(Req, undefined, _) ->
    ecf_utils:reply_status(401, undefined, post_401, Req);
try_post(Req0 = #{method := <<"POST">>}, User, <<"create">>) ->
    LimitSec = application:get_env(ecf, post_limit_seconds, 20),
    Limit = LimitSec * 1000000,
    Time = timer:now_diff(erlang:timestamp(), ecf_user:last_post(User)),
    case Time > Limit of
        true ->
            {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
            {_, Thread0} = lists:keyfind(<<"thread">>, 1, KV),
            Thread = binary_to_integer(Thread0),
            case ecf_perms:check_perm_thread(User, ecf_thread:get_thread(Thread),
                                             create_post) of
                true ->
                    {_, Text} = lists:keyfind(<<"text">>, 1, KV),
                    Post = ecf_post:new_post(Thread, ecf_user:id(User),
                                             erlang:timestamp(), Text),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303,
                                     #{<<"location">>
                                       => [Base, <<"/thread/">>, Thread0,
                                           <<"#post-">>, integer_to_list(Post)]},
                                     Req);
                false ->
                    ecf_utils:reply_status(403, User, create_post_403, Req)
            end;
        false ->
            ecf_utils:reply_status(429, User, create_post_429, Req0, true)
    end;
try_post(Req0 = #{method := <<"POST">>}, User, <<"delete">>) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, TId0} = lists:keyfind(<<"thread">>, 1, KV),
    TId = binary_to_integer(TId0),
    {_, Id0} = lists:keyfind(<<"post">>, 1, KV),
    Id = binary_to_integer(Id0),
    case ecf_thread:get_thread(TId) of
        undefined ->
            ecf_utils:reply_status(400, User, thread_404, Req);
        Thread ->
            case ecf_perms:check_perm_thread(User, Thread, delete_post) of
                true ->
                    ok = ecf_post:delete_post(TId, Id),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303,
                                     #{<<"location">>
                                       => [Base, <<"/thread/">>, TId0]},
                                     Req);
                false ->
                    ecf_utils:reply_status(403, User, delete_post_403, Req)
            end
    end;
try_post(Req, User, _) ->
    ecf_utils:reply_status(405, User, post_405, Req).

