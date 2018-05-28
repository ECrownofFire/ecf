-module(ecf_404_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    Html = ecf_generators:generate(404, User, ignored),
    Req2 = cowboy_req:reply(404,
                            #{<<"content-type">> => <<"text/html">>},
                            Html,
                            Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

