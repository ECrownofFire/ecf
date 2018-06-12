-module(ecf_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

% -record(state, { }).


init(Req, State) ->
    Type = cowboy_req:binding(type, Req),
    Id = cowboy_req:binding(id, Req),
    User = ecf_utils:check_user_session(Req),
    Req2 = case Type of
               undefined ->
                   Forums = ecf_forum:get_forums(),
                   Html = ecf_generators:generate(main, User, Forums),
                   reply_200(Html, Req);
               <<"user">> ->
                   case ecf_user:get_user(Id) of
                       {error, user_not_found} ->
                           ecf_utils:reply_status(404, User, ignored, Req);
                       Profile ->
                           case ecf_perms:check_perm_global(User, view_user) of
                               true ->
                                   Html = ecf_generators:generate(user, User,
                                                                  Profile),
                                   reply_200(Html, Req);
                               false ->
                                   ecf_utils:reply_status(403, User,
                                                          view_user_403, Req)
                           end
                   end;
               <<"group">> ->
                   case ecf_group:get_group(Id) of
                       {error, group_not_found} ->
                           ecf_utils:reply_status(404, User, ignored, Req);
                       Group ->
                           case ecf_perms:check_perm_group(User, Group,
                                                           view_group) of
                               true ->
                                    Html = ecf_generators:generate(group, User,
                                                                   Group),
                               reply_200(Html, Req);
                               false ->
                                   ecf_utils:reply_status(403, User,
                                                          view_group_403, Req)
                           end
                   end
           end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

reply_200(Html, Req) ->
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"text/html">>},
                     Html,
                     Req).

