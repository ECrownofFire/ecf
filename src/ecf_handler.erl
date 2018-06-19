-module(ecf_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

% -record(state, { }).


init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    Forums = ecf_forum:visible_forums(ecf_forum:get_forums(), User),
    Html = ecf_generators:generate(main, User, Forums),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                            Html, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

