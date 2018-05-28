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
            {_, Bday0}    = lists:keyfind(<<"bday">>, 1, KV),
            Bio = ecf_utils:get_and_sanitize(KV, <<"bio">>),
            Bday = iso8601:parse(Bday0),
            try_register(check_username(Username) and check_password(Password),
                         {Username, Password, Email, Bday, Bio},
                         Req, State);
        <<"GET">> ->
            Message = application:get_env(ecf, register_message,
                                          <<"Please register.">>),
            Html = ecf_generators:generate(register, ignored, Message),
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
    Message = application:get_env(ecf, invalid_username,
                                  <<"Invalid username, try again!">>),
    Html = ecf_generators:generate(register, undefined, Message),
    Req2 = cowboy_req:reply(400,
                            #{<<"content-type">> => <<"text/html">>},
                            Html,
                            Req),
    {ok, Req2, State};
try_register(true, {Username, Password, Email, Bday, Bio}, Req, State) ->
    Base = application:get_env(ecf, base_url, ""),
    Url = [Base, "/"],
    Time = erlang:timestamp(),
    case ecf_user:new_user(Username, Password, Email, Time, Bday) of
        {error, Reason} ->
            Message = application:get_env(ecf, Reason,
              <<"Username or email already taken, please use another">>),
            Html = ecf_generators:generate(register, undefined, Message),
            Req2 = cowboy_req:reply(400,
                                    #{<<"content-type">> => <<"text/html">>},
                                    Html,
                                    Req),
            {ok, Req2, State};
        Id ->
            ok = ecf_user:edit_bio(Id, Bio),
            Req2 = cowboy_req:reply(302,
                                    #{<<"Location">> => Url},
                                    Req),
            {ok, Req2, State}
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

