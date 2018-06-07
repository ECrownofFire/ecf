-module(ecf_utils).

-export([check_user_session/1, sanitize/1, get_and_sanitize/2, get_ip/1,
         reply_status/4]).

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
    U = ecf_user:get_user(ID),
    case U of
        {error, user_not_found} ->
            undefined;
        _ ->
            Session = base64:decode(SessionEncoded),
            case ecf_user:check_session(U, Session) of
                true ->
                    U;
                false ->
                    undefined
            end
    end.


sanitize(Document) ->
    Replace = fun({Char, Rep}, String) ->
                      string:replace(String, Char, Rep, all)
              end,
    Replacements = [{<<"\x00">>, <<"\x{FFFD}">>},
                    {<<"&">>,<<"&amp;">>},
                    {<<"{{">>,<<"&#123;&#123;">>}, % for template safety
                    {<<"<">>,<<"&lt;">>},
                    {<<">">>,<<"&gt;">>},
                    {<<"\"">>,<<"&quot;">>},
                    {<<"\'">>,<<"&#39;">>},
                    {<<"/">>,<<"&#47;">>}],
    lists:foldl(Replace, Document, Replacements).

get_and_sanitize(KV, Target) ->
    case lists:keyfind(Target, 1, KV) of
        {_, S} ->
            iolist_to_binary(sanitize(S));
        _ ->
            <<"">>
    end.

% wrapper to get IP in case reverse proxying is in use
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
    Html = ecf_generators:generate(Status, User, Type),
    Map = case Status of
              401 ->
                  #{<<"content-type">> => <<"text/html">>,
                    <<"www-authenticate">> => <<"FormBase">>};
              _ -> #{<<"content-type">> => <<"text/html">>}
          end,
    cowboy_req:reply(Status, Map, Html, Req).

