-module(ecf_admin_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    case User of
        undefined ->
            Req2 = ecf_utils:reply_status(401, undefined, admin_401, Req),
            {ok, Req2, State};
        _ ->
            case lists:member(0, ecf_user:groups(User)) of
                false ->
                    Req2 = ecf_utils:reply_status(403, User, admin_403, Req),
                    {ok, Req2, State};
                true ->
                    Html = ecf_generators:generate(admin, User, ignored),
                    Req2 = cowboy_req:reply(200,
                                            #{<<"content-type">> => <<"text/html">>},
                                            Html,
                                            Req),
                    {ok, Req2, State}
            end
    end.


terminate(_Reason, _Req, _State) ->
    ok.

