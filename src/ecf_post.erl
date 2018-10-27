-module(ecf_post).

-export_type([id/0, post/0]).

-export([create_table/1,
         new_post/4,
         get_posts/1, delete_posts/1,
         get_posts/3,
         get_post/2, delete_post/2,
         edit_post/5,
         id/1, thread/1, poster/1, time/1, text/1, edited/1]).


%%% Post type wrapper

-type id() :: non_neg_integer().


-record(ecf_post,
        {id     :: {ecf_thread:id(), id()},
         poster :: ecf_user:id(),
         time   :: erlang:timestamp(),
         text   :: binary(),
         edited :: undefined | {ecf_user:id(), erlang:timestamp()}}).
-type post() :: #ecf_post{}.

-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_post,
                        [{attributes, record_info(fields, ecf_post)},
                         {type, set},
                         {disc_copies, Nodes}]),
    ok.

% Main method to create a new post
-spec new_post(ecf_thread:id(), ecf_user:id(), erlang:timestamp(),
               binary()) -> id().
new_post(Thread, Poster, Time, Text) ->
    F = fun() ->
                _ = ecf_thread:get_thread(Thread),
                Id = ecf_thread:new_post(Thread, Time),
                ecf_user:add_post(Poster, Time),
                mnesia:write(#ecf_post{id={Thread,Id},poster=Poster,
                                       time=Time,text=Text}),
                Id
        end,
    mnesia:activity(transaction, F).

-spec delete_posts(ecf_thread:id()) -> ok.
delete_posts(Thread) ->
    F = fun() ->
                N = ecf_thread:last(ecf_thread:get_thread(Thread)),
                [mnesia:delete({ecf_post, {Thread, X}}) || X <- lists:seq(1, N)]
        end,
    mnesia:activity(transaction, F).

-spec delete_post(ecf_thread:id(), id()) -> ok.
delete_post(Thread, Id) ->
    F = fun() ->
                mnesia:delete({ecf_post, {Thread, Id}}),
                ecf_thread:delete_post(Thread, Id)
        end,
    mnesia:activity(transaction, F).

-spec get_post(ecf_thread:id(), id()) -> post() | undefined.
get_post(Thread, Id) ->
    F = fun() ->
                case mnesia:read({ecf_post, {Thread,Id}}) of
                    [Post] -> Post;
                    _ -> undefined
                end
        end,
    mnesia:activity(transaction, F).

-spec get_posts(ecf_thread:id()) -> [post()].
get_posts(Thread) ->
    F = fun() ->
                mnesia:match_object(#ecf_post{id={Thread,'_'},_='_'})
        end,
    lists:keysort(#ecf_post.id, mnesia:activity(transaction, F)).

%% Get from [First, Last]
-spec get_posts(ecf_thread:id(), id(), id()) -> [post()].
get_posts(Thread, First, Last) ->
    F = fun() ->
                [mnesia:read({ecf_post, {Thread,I}}) || I <- lists:seq(First, Last)]
        end,
    lists:flatten(mnesia:activity(transaction, F)).


-spec edit_post(ecf_thread:id(), id(), ecf_user:id(),
                erlang:timestamp(),binary()) -> ok.
edit_post(Thread, Id, User, Time, Text) ->
    F = fun() ->
                P = get_post(Thread, Id),
                mnesia:delete_object(P),
                mnesia:write(P#ecf_post{text=Text,edited={User,Time}})
        end,
    mnesia:activity(transaction, F).

%% Wrapper functions

-spec thread(post()) -> ecf_thread:id().
thread(Post) ->
    {T,_I} = Post#ecf_post.id,
    T.

-spec id(post()) -> id().
id(Post) ->
    {_T,I} = Post#ecf_post.id,
    I.

-spec poster(post()) -> ecf_user:id().
poster(Post) ->
    Post#ecf_post.poster.

-spec time(post()) -> erlang:timestamp().
time(Post) ->
    Post#ecf_post.time.

-spec text(post()) -> binary().
text(Post) ->
    Post#ecf_post.text.

-spec edited(post()) -> {ecf_user:id(), erlang:timestamp()} | undefined.
edited(Post) ->
    Post#ecf_post.edited.

