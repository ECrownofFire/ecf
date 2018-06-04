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
                           Html = ecf_generators:generate(user, User, Profile),
                           reply_200(Html, Req)
                   end;
               <<"forum">> ->
                   case ecf_forum:get_forum(Id) of
                       {error, forum_not_found} ->
                           ecf_utils:reply_status(404, User, ignored, Req);
                       Forum ->
                           case ecf_perms:check_perm_forum(User, Forum,
                                                           view_forum) of
                               true ->
                                   Threads = ecf_thread:get_forum_threads(Id),
                                   Html = ecf_generators:generate(forum, User,
                                                                  {Forum, Threads}),
                                   reply_200(Html, Req);
                               false ->
                                   ecf_utils:reply_status(403, User,
                                                          view_forum_403, Req)
                           end
                   end;
               <<"thread">> ->
                   case ecf_thread:get_thread(Id) of
                       {error, thread_not_found} ->
                           ecf_utils:reply_status(404, User, ignored, Req);
                       Thread ->
                           case ecf_perms:check_perm_thread(User, Thread,
                                                     view_thread) of
                               true ->
                                   Forum = ecf_forum:get_forum(
                                             ecf_thread:forum(Thread)),
                                   Posts = ecf_post:get_posts(Id),
                                   Html = ecf_generators:generate(thread,
                                                                  User,
                                                                  {Forum,
                                                                   Thread,
                                                                   Posts}),
                                   reply_200(Html, Req);
                               false ->
                                   ecf_utils:reply_status(403, User,
                                                          view_thread_403, Req)
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

