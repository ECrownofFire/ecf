-module(ecf_change_pw_handler).

-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
    Req2 = case ecf_utils:check_user_session(Req) of
               undefined ->
                   ecf_utils:reply_status(401, undefined, change_pw_401, Req);
               User ->
                   Html = ecf_generators:generate(change_pw, User, change_pw_message),
                   cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    Html, Req)
           end,
    {ok, Req2, State};
init(Req0 = #{method := <<"POST">>}, State) ->
    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, OldPassword} = lists:keyfind(<<"old-password">>, 1, KV),
    {_, Password} = lists:keyfind(<<"password">>, 1, KV),
    User = ecf_utils:check_user_session(Req),
    Req2 = change_pw(User, OldPassword, Password, Req),
    {ok, Req2, State}.


change_pw(undefined, _, _, Req) ->
    ecf_utils:reply_status(401, undefined, change_pw_401, Req);
change_pw(User, Old, Password, Req) ->
    case ecf_utils:valid_password(Password) of
        false ->
            Html = ecf_generators:generate(change_pw, User, change_pw_invalid_pw),
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>},
                             Html, Req);
        true ->
            case ecf_user:check_pass(User, Old) of
                false ->
                    Html = ecf_generators:generate(change_pw, User, change_pw_fail),
                    cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>},
                                     Html, Req);
                true ->
                    Id = ecf_user:id(User),
                    Sess = ecf_user:edit_pass(Id, Password),
                    Req2 = ecf_login_handler:set_login_cookies(Req, Id, Sess),
                    Base = application:get_env(ecf, base_url, ""),
                    cowboy_req:reply(303, #{<<"location">> => [Base, "/"]}, Req2)
            end
    end.

