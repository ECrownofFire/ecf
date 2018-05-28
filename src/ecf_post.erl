-module(ecf_post).

-export_type([id/0, post/0]).

-export([create_table/1,
         new_post/4,
         get_posts/1, delete_posts/1,
         get_post/2, delete_post/2,
         edit_post/5,
         id/1, thread/1, poster/1, time/1, text/1, edited/1]).

-include_lib("stdlib/include/qlc.hrl").

%%% Post type wrapper

-type id() :: non_neg_integer().


-record(ecf_post,
        {thread :: ecf_thread:id(),
         id     :: id(),
         poster :: ecf_user:id(),
         time   :: erlang:timestamp(),
         text   :: binary(),
         edited :: undefined | {ecf_user:id(), erlang:timestamp()}}).
-type post() :: #ecf_post{}.

-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_post,
                        [{attributes, record_info(fields, ecf_post)},
                         {type, bag},
                         {index, [#ecf_post.id]},
                         {disc_copies, Nodes}]),
    ok.

-spec new_post(ecf_thread:id(), ecf_user:id(), erlang:timestamp(),
               binary()) -> ok.
new_post(Thread, Poster, Time, Text) ->
    F = fun() ->
                _ = ecf_thread:get_thread(Thread),
                Id = ecf_thread:new_post(Thread, Time),
                mnesia:write(#ecf_post{thread=Thread,id=Id,poster=Poster,
                                       time=Time,text=Text})
        end,
    mnesia:activity(transaction, F).

-spec delete_posts(ecf_thread:id()) -> ok.
delete_posts(Thread) ->
    F = fun() ->
                mnesia:delete({ecf_post, Thread})
        end,
    mnesia:activity(transaction, F).

-spec delete_post(ecf_thread:id(), id()) -> ok.
delete_post(Thread, Id) ->
    F = fun() ->
                Post = get_post(Thread, Id),
                mnesia:delete_object(Post)
        end,
    mnesia:activity(transaction, F).

-spec get_post(ecf_thread:id(), id()) -> post().
get_post(Thread, Id) ->
    F = fun() ->
                [Post] = qlc:eval(qlc:q(
                                    [X || X = #ecf_post{thread=T, id=I}
                                          <- mnesia:table(ecf_post),
                                          T =:= Thread, I =:= Id])),
                Post
        end,
    mnesia:activity(transaction, F).

-spec get_posts(ecf_thread:id()) -> [post()].
get_posts(Thread) ->
    F = fun() ->
                mnesia:read({ecf_post, Thread})
        end,
    mnesia:activity(transaction, F).

-spec edit_post(ecf_thread:id(), id(), ecf_user:id(),
                erlang:timestamp(),binary()) -> ok.
edit_post(Thread, Id, User, Time, Text) ->
    F = fun() ->
                P = get_post(Thread, Id),
                mnesia:write(P#ecf_post{text=Text,edited={User,Time}})
        end,
    mnesia:activity(transaction, F).

%% Wrapper functions

-spec thread(post()) -> ecf_thread:id().
thread(Post) ->
    Post#ecf_post.thread.

-spec id(post()) -> id().
id(Post) ->
    Post#ecf_post.id.

-spec poster(post()) -> ecf_user:id().
poster(Post) ->
    Post#ecf_post.poster.

-spec time(post()) -> erlang:timestamp().
time(Post) ->
    Post#ecf_post.time.

-spec text(post()) -> binary().
text(Post) ->
    Post#ecf_post.text.

-spec edited(post()) -> {ecf_user:id(), erlang:timestamp()}.
edited(Post) ->
    Post#ecf_post.edited.

