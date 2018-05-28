-module(ecf_utils).

-export([check_user_session/1, sanitize/1, get_and_sanitize/2]).

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

