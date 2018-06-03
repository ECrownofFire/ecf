-module(ecf_post_handler).
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

init(Req, State) ->
    case ecf_utils:check_user_session(Req) of
        undefined ->
            Req2 = ecf_utils:reply_401(Req, new_post_401),
            {ok, Req2, State};
        User ->
            case maps:get(method, Req) of
                <<"POST">> ->
                    {ok, KV, Req2} = cowboy_req:read_urlencoded_body(Req),
                    {_, Thread0} = lists:keyfind(<<"thread">>, 1, KV),
                    Thread = binary_to_integer(Thread0),
                    Text = ecf_utils:get_and_sanitize(KV, <<"text">>),
                    Post = ecf_post:new_post(Thread, ecf_user:id(User),
                                             erlang:timestamp(), Text),
                    Thread2 = integer_to_list(Thread),
                    Req3 = cowboy_req:reply(303,
                                            #{<<"Location">> =>
                                              [<<"{{base}}/thread/">>, Thread2,
                                               <<"#post-">>, integer_to_list(Post)]},
                                            Req2),
                    {ok, Req3, State};
                _ ->
                    Html = ecf_generators:generate(404, User, ignored),
                    Req2 = cowboy_req:reply(404,
                                            #{<<"content-type">> => <<"text/html">>},
                                            Html,
                                            Req),
                    {ok, Req2, State}
            end
    end.

terminate(_Reason, _Req, _State) ->
    ok.
