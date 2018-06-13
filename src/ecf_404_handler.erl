-module(ecf_404_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

init(Req, State) ->
    User = ecf_utils:check_user_session(Req),
    Req2 = ecf_utils:reply_status(404, User, false, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

