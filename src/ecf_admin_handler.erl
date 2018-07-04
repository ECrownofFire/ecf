-module(ecf_admin_handler).

-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = handle_reply(User, Req),
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.

handle_reply(undefined, Req) ->
    ecf_utils:reply_status(401, undefined, admin_401, Req);
handle_reply(User, Req) ->
    Html = ecf_generators:generate(admin, User, ignored),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req).

