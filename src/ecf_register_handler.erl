-module(ecf_register_handler).
-behaviour(cowboy_handler).

-export([init/2]).


init(Req = #{method := <<"GET">>}, State) ->
    Html = ecf_generators:generate(register, undefined, {register_message, []}),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                            Html,
                            Req),
    {ok, Req2, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    {Username, Password, Email, Bday, Bio, Resp, Req} = get_register_vars(Req0),
    Ip = ecf_utils:get_ip(Req),
    Req2 = case ecf_captcha:check_captcha(Ip, Resp) of
               true ->
                   try_register(Username, Password, Email, Bday, Bio, Req);
               false ->
                   fail(failed_captcha, Username, Email, Bday, Bio, Req)
           end,
    {ok, Req2, State}.

-spec get_register_vars(cowboy_req:req()) ->
    {binary(), binary(), binary(),
     undefined | calendar:datetime(), binary(), binary(), cowboy_req:req()}.
get_register_vars(Req0) ->
    Atom = ecf_captcha:get_atom(),
    L = [username, password, email, {bday, [], <<"">>}, {bio, [], <<"">>},
         {Atom, [], <<"">>}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{username := Username, password := Password, email := Email,
      bday := Bday0, bio := Bio, Atom := Resp} = M,
    Bday = case Bday0 of
               <<"">> -> undefined;
               _ when is_binary(Bday0) -> iso8601:parse(Bday0)
           end,
    {Username, Password, Email, Bday, Bio, Resp, Req}.

try_register(Username, Password, Email, Bday, Bio, Req) ->
    case check_register(Username, Password, Email) of
        ok ->
            do_register(Username, Password, Email, Bday, Bio, Req);
        Res ->
            fail(Res, Username, Email, Bday, Bio, Req)
    end.

check_register(Username, Password, Email) ->
    case {ecf_utils:valid_username(Username),
          ecf_utils:valid_password(Password),
          ecf_utils:valid_email(Email)} of
        {true, true, true} ->
            ok;
        {false, _, _} ->
            invalid_username;
        {_, false, _} ->
            invalid_password;
        {_, _, false} ->
            invalid_email
    end.

do_register(Username, Password, Email, Bday, Bio, Req) ->
    case ecf_user:new_user(Username, Password, Email) of
        {error, Reason} ->
            fail(Reason, Username, Email, Bday, Bio, Req);
        {Id, Session} ->
            ok = ecf_user:edit_bday(Id, Bday),
            ok = ecf_user:edit_bio(Id, Bio),
            Req2 = ecf_utils:set_login_cookies(Req, Id, Session),
            % TODO: flash here
            ecf_utils:reply_redirect(303, <<"/">>, Req2)
    end.

fail(Reason, Username, Email, Bday, Bio, Req) ->
    BaseV = [{username, Username}, {email, Email}, {bday, Bday}, {bio, Bio}],
    Html = ecf_generators:generate(register, undefined, {Reason, BaseV}),
    cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, Html, Req).

