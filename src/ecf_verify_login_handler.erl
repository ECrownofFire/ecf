-module(ecf_verify_login_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    #{user := User} = cowboy_req:match_qs([{user, [], <<"">>}], Req),
    case User of
        <<"">> ->
            Req2 = cowboy_req:reply(400, Req),
            {ok, Req2, State};
        _ ->
            {Ip, _} = cowboy_req:peer(Req),
            Ans = ecf_log:check_log(User, Ip),
            Req2 = cowboy_req:reply(200, #{}, atom_to_binary(Ans, utf8), Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

