-module(ecf_profile_edit_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req0, State) ->
    User = ecf_utils:check_user_session(Req0),
    case User of
        undefined ->
            Req = ecf_utils:reply_401(Req0, edit_profile_401),
            {ok, Req, State};
        _ ->
            case maps:get(method, Req0) of
                <<"POST">> ->
                    Id = ecf_user:id(User),
                    {ok, KV, Req} = cowboy_req:read_urlencoded_body(Req0),
                    Bday = case lists:keyfind(<<"bday">>, 1, KV) of
                               {_, <<"">>} -> undefined;
                               {_, Bday0} -> iso8601:parse(Bday0);
                               _ -> undefined
                           end,
                    Bio = ecf_utils:get_and_sanitize(KV, <<"bio">>),
                    Loc = ecf_utils:get_and_sanitize(KV, <<"loc">>),
                    ecf_user:edit_bday(Id, Bday),
                    ecf_user:edit_bio(Id, Bio),
                    ecf_user:edit_loc(Id, Loc),
                    Req2 = cowboy_req:reply(302,
                                            #{<<"Location">> =>
                                              [<<"{{base}}/user/">>,
                                               integer_to_list(Id)]},
                                            Req),
                    {ok, Req2, State};
                <<"GET">> ->
                    Html = ecf_generators:generate(edit_profile, User, ignored),
                    Req = cowboy_req:reply(200,
                                           #{<<"content-type">> => <<"text/html">>},
                                           Html,
                                           Req0),
                    {ok, Req, State}
            end
    end.

terminate(_Reason, _Req, _State) ->
    ok.

