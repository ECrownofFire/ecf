-module(ecf_confirm_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    Req2 = case ecf_utils:check_user_session(Req) of
               undefined ->
                   ecf_utils:reply_status(401, undefined, confirm_401, Req);
               User ->
                   #{code := Code} = cowboy_req:match_qs([code], Req),
                   Id = ecf_user:id(User),
                   case ecf_email:get_confirm_code(Id) of
                       undefined ->
                           ecf_utils:reply_status(400, User, confirm_400, Req);
                       Code ->
                           ok = ecf_user:confirm_email(Id),
                           ok = ecf_email:clear_confirm_code(Id),
                           Html = ecf_generators:generate(confirmed_email,
                                                          User,
                                                          ignored),
                           cowboy_req:reply(200,
                                            #{<<"content-type">> => <<"text/html">>},
                                            Html,
                                            Req)
                   end
           end,
    {ok, Req2, State}.

