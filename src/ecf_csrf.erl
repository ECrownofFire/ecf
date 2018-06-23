-module(ecf_csrf).

-behaviour(cowboy_middleware).

-export([execute/2]).

% it may be worth adding a CSRF token at some point
execute(Req = #{method := <<"POST">>}, Env) ->
    User = ecf_utils:check_user_session(Req),
    Host = get_source(Req),
    case get_target(Req) of
        Host ->
            {ok, Req, Env};
        _ ->
            reply_400(Req, User)
    end;
execute(Req, Env) ->
    {ok, Req, Env}.


get_source(Req) ->
    % cowboy doesn't forward header parsing for these,
    % so they have to be parsed manually
    Ref = cowboy_req:header(<<"referer">>, Req),
    % important to always default to origin if possible
    Uri = cowboy_req:header(<<"origin">>, Req, Ref),
    {ok, {_, _, Host, _, _, _}} = http_uri:parse(Uri),
    Host.

get_target(Req) ->
    case cowboy_req:header(<<"host">>, Req) of
        undefined ->
            false;
        H ->
            % just call cowlib directly to parse this
            {Host, _} = cow_http_hd:parse_host(H),
            Host
    end.

reply_400(Req, User) ->
    Req2 = ecf_utils:reply_status(400, User, csrf_400, Req),
    {stop, Req2}.

