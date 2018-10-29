-module(ecf_ban_handler).

-export([init/2]).


init(Req = #{method := <<"GET">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = do_get(User, Req),
    {ok, Req2, State};
init(Req = #{method := <<"POST">>}, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = case cowboy_req:binding(action, Req) of
               <<"ban">> ->
                   do_ban(User, Req);
               <<"unban">> ->
                   do_unban(User, Req)
           end,
    {ok, Req2, State}.


do_get(User, Req) ->
    #{id := Id} = cowboy_req:match_qs([{id, int, 0}], Req),
    Action = binary_to_atom(cowboy_req:binding(action, Req), latin1),
    case check(User, Id, Req) of
        {ok, Target} ->
            Ban = ecf_ban:check_ban(Id),
            Html = ecf_generators:generate(Action, User, {Target, Ban}),
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                             Html, Req);
        R ->
            R
    end.

do_ban(User, Req0) ->
    L = [{id, int}, reason, {time, fun time_constraint/2}, {length, int, 1}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{id := Id, reason := Reason, time := Time, length := Length} = M,
    case check(User, Id, Req) of
        {ok, _Target} ->
            BanTime = get_time(Time, Length),
            ok = ecf_ban:new_ban(Id, ecf_user:id(User), Reason,
                                 erlang:timestamp(), BanTime),
            Base = application:get_env(ecf, base_url, ""),
            cowboy_req:reply(303, #{<<"location">> => [Base, "/user/",
                                                       integer_to_binary(Id)]},
                             Req);
        R ->
            R
    end.

do_unban(User, Req0) ->
    L = [{id, int}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{id := Id} = M,
    case check(User, Id, Req) of
        {ok, _Target} ->
            ok = ecf_ban:delete_ban(Id),
            Base = application:get_env(ecf, base_url, ""),
            cowboy_req:reply(303, #{<<"location">> => [Base, "/user/",
                                                       integer_to_binary(Id)]},
                             Req);
        R ->
            R
    end.

get_time(<<"hours">>, Num) ->
    Micro = timer:hours(Num) * 1000,
    get_time(Micro);
get_time(<<"days">>, Num) ->
    Micro = timer:hours(Num * 24) * 1000,
    get_time(Micro);
get_time(<<"weeks">>, Num) ->
    Micro = timer:hours(Num * 24 * 7) * 1000,
    get_time(Micro);
get_time(<<"years">>, Num) ->
    Micro = timer:hours(Num * 24 * 365.25) * 1000,
    get_time(Micro);
get_time(<<"permanent">>, _) ->
    permanent.

get_time(MicroSeconds) ->
    {Mega, Sec, Mic} = erlang:timestamp(),
    Micros = Mega * 1000000 * 1000000 + Sec * 1000000 + Mic,
    Time = round(Micros + MicroSeconds),
    {Time div 1000000 div 1000000,
     Time div 1000000 rem 1000000,
     Time rem 1000000}.


check(User, Id, Req) ->
    case ecf_user:get_user(Id) of
        undefined ->
            ecf_utils:reply_status(404, User, user_404, Req);
        Target ->
            case ecf_ban:check_perm(User, Target) of
                false ->
                    ecf_utils:reply_status(403, User, ban_403, Req);
                true ->
                    {ok, Target}
            end
    end.

time_constraint(A, Bin) when A =:= forward; A =:= reverse ->
    List = [<<"hours">>, <<"days">>, <<"weeks">>, <<"years">>, <<"permanent">>],
    case lists:member(Bin, List) of
        true ->
            {ok, Bin};
        false ->
            {error, invalid_time}
    end;
time_constraint(format_error, {invalid_time, Val}) ->
    io_lib:format("~p is not a valid time.", [Val]).

