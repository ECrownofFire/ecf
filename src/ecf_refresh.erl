-module(ecf_refresh).

-behaviour(cowboy_middleware).

-export([execute/2]).


execute(Req, Env) ->
    case ecf_utils:get_session_cookies(Req) of
        undefined ->
            {ok, Req, Env};
        {Id, Old} ->
            case ecf_user:refresh_session(Id, Old) of
                ok ->
                    {ok, Req, Env};
                false ->
                    % session expired, redirect user to login
                    Url0 = cowboy_req:uri(Req, #{host => undefined}),
                    <<$/, Url/binary>> = iolist_to_binary(Url0),
                    Req2 = ecf_utils:clear_login_cookies(Req),
                    Req3 = ecf_utils:reply_redirect(302,
                                                    [<<"/login?url=">>,
                                                     cow_uri:urlencode(Url)],
                                                    Req2),
                    {stop, Req3};
                New ->
                    Req2 = ecf_utils:set_login_cookies(Req, Id, New),
                    {ok, Req2, Env}
            end
    end.

