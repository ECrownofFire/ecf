-module(ecf_register_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

-define(SESSION_TIME, 604800). % one week

init(Req = #{method := <<"GET">>}, State) ->
    Html = ecf_generators:generate(register, undefined, {register_message, []}),
    Req2 = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"text/html">>},
                            Html,
                            Req),
    {ok, Req2, State};
init(Req0 = #{method := <<"POST">>}, State) ->
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
    Req2 = case ecf_captcha:check_captcha(Ip, KV) of
               true ->
                   try_register(check_username(Username)
                                and ecf_utils:valid_password(Password),
                                {Username, Password, Email, Bday, Bio},
                                Req);
               false ->
                   Vars = [{username, Username}, {email, Email},
                           {bday, Bday}, {bio, Bio}],
                   Html = ecf_generators:generate(register, undefined,
                                                  {register_failed_captcha,
                                                   Vars}),
                   cowboy_req:reply(400,
                                    #{<<"content-type">> => <<"text/html">>},
                                    Html,
                                    Req)
           end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

try_register(false, {Username, _, Email, Bday, Bio}, Req) ->
    BaseV = [{username, Username}, {email, Email}, {bday, Bday}, {bio, Bio}],
    Html = ecf_generators:generate(register, undefined, {invalid_username, BaseV}),
    cowboy_req:reply(400,
                     #{<<"content-type">> => <<"text/html">>},
                     Html,
                     Req);
try_register(true, {Username, Password, Email, Bday, Bio}, Req) ->
    case ecf_user:new_user(Username, Password, Email, Bday) of
        {error, Reason} ->
            BaseV = [{username, Username}, {email, Email},
                     {bday, Bday}, {bio, Bio}],
            Html = ecf_generators:generate(register, undefined, {Reason, BaseV}),
            cowboy_req:reply(400,
                             #{<<"content-type">> => <<"text/html">>},
                             Html,
                             Req);
        {Id, Session} ->
            ok = ecf_user:edit_bio(Id, Bio),
            Req2 = ecf_utils:set_login_cookies(Req, Id, Session),
            Base = application:get_env(ecf, base_url, ""),
            % TODO: flash here
            cowboy_req:reply(303,
                             #{<<"location">> => [Base, "/"]},
                             Req2)
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

