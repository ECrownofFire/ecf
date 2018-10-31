-module(ecf_register_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

-define(SESSION_TIME, 604800). % one week

init(Req = #{method := <<"GET">>}, State) ->
    Html = ecf_generators:generate(register, undefined, {register_message, []}),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
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
                   try_register(ecf_utils:valid_username(Username),
                                ecf_utils:valid_password(Password),
                                ecf_utils:valid_email(Email),
                                Username, Password, Email, Bday, Bio,
                                Req);
               false ->
                   Vars = [{username, Username}, {email, Email},
                           {bday, Bday}, {bio, Bio}],
                   Html = ecf_generators:generate(register, undefined,
                                                  {failed_captcha, Vars}),
                   cowboy_req:reply(400,
                                    #{<<"content-type">> => <<"text/html">>},
                                    Html,
                                    Req)
           end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

try_register(true, true, true, Username, Password, Email, Bday, Bio, Req) ->
    case ecf_user:new_user(Username, Password, Email) of
        {error, Reason} ->
            BaseV = [{username, Username}, {email, Email},
                     {bday, Bday}, {bio, Bio}],
            Html = ecf_generators:generate(register, undefined, {Reason, BaseV}),
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>},
                             Html,
                             Req);
        {Id, Session} ->
            ok = ecf_user:edit_bday(Id, Bday),
            ok = ecf_user:edit_bio(Id, Bio),
            Req2 = ecf_utils:set_login_cookies(Req, Id, Session),
            Base = application:get_env(ecf, base_url, ""),
            % TODO: flash here
            cowboy_req:reply(303, #{<<"location">> => [Base, "/"]}, Req2)
    end;
try_register(false, _, _, Username, _, Email, Bday, Bio, Req) ->
    fail(invalid_username, Username, Email, Bday, Bio, Req);
try_register(_, false, _, Username, _, Email, Bday, Bio, Req) ->
    fail(invalid_password, Username, Email, Bday, Bio, Req);
try_register(_, _, false, Username, _, Email, Bday, Bio, Req) ->
    fail(invalid_email, Username, Email, Bday, Bio, Req).

fail(Reason, Username, Email, Bday, Bio, Req) ->
    BaseV = [{username, Username}, {email, Email}, {bday, Bday}, {bio, Bio}],
    Html = ecf_generators:generate(register, undefined, {Reason, BaseV}),
    cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, Html, Req).

