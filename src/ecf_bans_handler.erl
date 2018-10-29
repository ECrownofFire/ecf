-module(ecf_bans_handler).

-export([init/2]).

init(Req, State) ->
    Req2 = case ecf_utils:check_user_session(Req) of
               undefined ->
                   ecf_utils:reply_status(401, undefined, bans_401, Req);
               User ->
                   case ecf_perms:check_global_perm(User, view_bans) of
                       false ->
                           ecf_utils:reply_status(403, User, bans_403, Req);
                       true ->
                           Bans = ecf_ban:get_bans(),
                           Html = ecf_generators:generate(bans, User, Bans),
                           cowboy_req:reply(200, #{<<"content-type">>
                                                   => <<"text/html">>},
                                            Html, Req)
                   end
           end,
    {ok, Req2, State}.

