-module(ecf_utils).

-export([check_user_session/1,
         valid_password/1, valid_username/1, valid_email/1,
         get_ip/1, set_login_cookies/3, reply_status/4, reply_status/5]).

%%% Contains a few utility functions

-spec check_user_session(cowboy_req:req()) -> ecf_user:user() | undefined.
check_user_session(Req) ->
    #{session := SessionEncoded,
      user := UID} = cowboy_req:match_cookies([{session, [], undefined},
                                               {user, int, -1}],
                                              Req),
    check_user_session(UID, SessionEncoded).

-spec check_user_session(integer(),
                         binary() | undefined) -> ecf_user:user() | undefined.
check_user_session(_, undefined) ->
    undefined;
check_user_session(-1, _) ->
    undefined;
check_user_session(ID, SessionEncoded) ->
    case ecf_user:get_user(ID) of
        undefined ->
            undefined;
        User ->
            Session = base64:decode(SessionEncoded),
            case ecf_user:check_session(User, Session) of
                true ->
                    User;
                false ->
                    undefined
            end
    end.


% wrapper to get IP in case reverse proxying is in use
-spec get_ip(cowboy_req:req()) -> inet:ip_address().
get_ip(Req) ->
    {ok, Http} = application:get_env(ecf, http),
    get_ip(Http, Req).

get_ip(false, Req) ->
    {Addr, _} = cowboy_req:peer(Req),
    Addr;
get_ip(_, Req) ->
    case cowboy_req:parse_header(<<"x-forwarded-for">>, Req) of
        undefined ->
            {Addr, _} = cowboy_req:peer(Req),
            Addr;
        List ->
            {ok, Ip} = inet:parse_address(binary_to_list(lists:last(List))),
            Ip
    end.

-spec reply_status(integer(), ecf_user:user() | undefined, atom(),
                   cowboy_req:req()) -> cowboy_req:req().
reply_status(Status, User, Type, Req) ->
    reply_status(Status, User, Type, Req, false).

-spec reply_status(integer(), ecf_user:user() | undefined, atom(),
                   cowboy_req:req(), boolean()) -> cowboy_req:req().
reply_status(Status, User, Type, Req, Storage) ->
    Html = ecf_generators:generate(Status, User, {Type, Storage}),
    Map = case Status of
              401 ->
                  #{<<"content-type">> => <<"text/html">>,
                    <<"www-authenticate">> => <<"FormBased">>};
              _ -> #{<<"content-type">> => <<"text/html">>}
          end,
    cowboy_req:reply(Status, Map, Html, Req).


-spec valid_password(binary()) -> boolean().
valid_password(Password) ->
    Min = application:get_env(ecf, min_password_length, 8),
    Max = application:get_env(ecf, max_password_length, 64),
    case byte_size(Password) of
        N when N >= Min, N =< Max ->
            true;
        _ ->
            false
    end.


-spec valid_username(binary()) -> boolean().
valid_username(Username) ->
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

-spec valid_email(binary()) -> boolean().
valid_email(Email) ->
    Re = <<"^\\V+@\\V+$">>, % reject newlines to avoid dumb breakage
    re:run(Email, Re, [unicode, {capture, none}]) =:= match.


-spec set_login_cookies(cowboy_req:req(), ecf_user:id(), binary())
        -> cowboy_req:req().
set_login_cookies(Req, Id, Session) ->
    {ok, SessionMins} = application:get_env(ecf, minutes_session),
    SessionSecs = SessionMins * 60,
    SessionEncoded = base64:encode(Session),
    Req2 = cowboy_req:set_resp_cookie(<<"session">>, SessionEncoded,
                                      Req,
                                      #{http_only => true,
                                        secure => true,
                                        max_age => SessionSecs}),
    cowboy_req:set_resp_cookie(<<"user">>,
                               integer_to_list(Id),
                               Req2,
                               #{max_age => SessionSecs}).

