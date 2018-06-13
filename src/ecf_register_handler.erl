-module(ecf_register_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

-define(SESSION_TIME, 604800). % one week

init(Req0, State) ->
    case maps:get(method, Req0) of
        <<"POST">> ->
            {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
            {_, Username} = lists:keyfind(<<"username">>, 1, KV),
            {_, Password} = lists:keyfind(<<"password">>, 1, KV),
            {_, Email}    = lists:keyfind(<<"email">>, 1, KV),
            Bday = case lists:keyfind(<<"bday">>, 1, KV) of
                       {_, <<"">>} -> undefined;
                       {_, Bday0} -> iso8601:parse(Bday0);
                       _ -> undefined
                   end,
            {_, Bio} = lists:keyfind(<<"bio">>, 1, KV),
            Ip = ecf_utils:get_ip(Req),
            case ecf_captcha:check_captcha(Ip, KV) of
                true ->
                    try_register(check_username(Username)
                                 and check_password(Password),
                                 {Username, Password, Email, Bday, Bio},
                                 Req, State);
                false ->
                    Html = ecf_generators:generate(register, undefined,
                                                   register_failed_captcha),
                    Req2 = cowboy_req:reply(400,
                                            #{<<"content-type">> => <<"text/html">>},
                                            Html,
                                            Req),
                    {ok, Req2, State}
            end;
        <<"GET">> ->
            Html = ecf_generators:generate(register, undefined, register_message),
            Req = cowboy_req:reply(200,
                                   #{<<"content-type">> => <<"text/html">>},
                                   Html,
                                   Req0),
            {ok, Req, State};
        _ ->
            Req = cowboy_req:reply(405, Req0),
            {ok, Req, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

try_register(false, _, Req, State) ->
    Html = ecf_generators:generate(register, undefined, invalid_username),
    Req2 = cowboy_req:reply(400,
                            #{<<"content-type">> => <<"text/html">>},
                            Html,
                            Req),
    {ok, Req2, State};
try_register(true, {Username, Password, Email, Bday, Bio}, Req, State) ->
    Time = erlang:timestamp(),
    case ecf_user:new_user(Username, Password, Email, Time, Bday) of
        {error, Reason} ->
            Html = ecf_generators:generate(register, undefined, Reason),
            Req2 = cowboy_req:reply(400,
                                    #{<<"content-type">> => <<"text/html">>},
                                    Html,
                                    Req),
            {ok, Req2, State};
        {Id, Session} ->
            ok = ecf_user:edit_bio(Id, Bio),
            Req2 = ecf_login_handler:set_login_cookies(Req, Id, Session),
            Req3 = cowboy_req:reply(302,
                                    #{<<"Location">> => <<"{{base}}/">>},
                                    Req2),
            {ok, Req3, State}
    end.


-spec check_username(binary()) -> boolean().
check_username(Username) ->
    MaxLength = application:get_env(ecf, max_username_length, 32),
    case byte_size(Username) of
        N when N >= 1, N =< MaxLength ->
            % allow dash, underscore, and ASCII letters/numbers only
            List = lists:flatten([$_, $-, lists:seq($A, $Z), lists:seq($a, $z),
                                  lists:seq($0, $9)]),
            Check = string:trim(Username, leading, List),
            string:is_empty(Check);
        _ ->
            false
    end.

-spec check_password(binary()) -> boolean().
check_password(Password) ->
    case byte_size(Password) of
        N when N >= 8, N =< 64 ->
            true;
        _ ->
            false
    end.

