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
                           reply_404(User, Req);
                       Profile ->
                           Html = ecf_generators:generate(user, User, Profile),
                           reply_200(Html, Req)
                   end;
               <<"forum">> ->
                   Forum = ecf_forum:get_forum(Id),
                   Threads = ecf_thread:get_forum_threads(Id),
                   Html = ecf_generators:generate(forum, User, {Forum, Threads}),
                   reply_200(Html, Req);
               <<"thread">> ->
                   case ecf_thread:get_thread(Id) of
                       {error, thread_not_found} ->
                           reply_404(User, Req);
                       Thread ->
                           Posts = ecf_post:get_posts(Id),
                           Html = ecf_generators:generate(thread, User,
                                                          {Thread, Posts}),
                           reply_200(Html, Req)
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

reply_404(User, Req) ->
    cowboy_req:reply(404,
                     #{<<"content-type">> => <<"text/html">>},
                     ecf_generators:generate(404, User, ignored),
                     Req).

