-module(ecf_utils).

-export([check_user_session/1, get_ip/1, reply_status/4, reply_status/5]).

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
% TODO: support x-forwarded-for in case of multiple forwards
-spec get_ip(cowboy_req:req()) -> inet:ip_address().
get_ip(Req) ->
    case cowboy_req:header(<<"x-forwarded-for">>, Req) of
        undefined ->
            {Addr, _} = cowboy_req:peer(Req),
            Addr;
        I ->
            {ok, Ip} = inet:parse_address(binary_to_list(I)),
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
                    <<"www-authenticate">> => <<"FormBase">>};
              _ -> #{<<"content-type">> => <<"text/html">>}
          end,
    cowboy_req:reply(Status, Map, Html, Req).

