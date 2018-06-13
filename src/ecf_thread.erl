-module(ecf_thread).

%%% Wrapper for the thread type

-export_type([id/0, thread/0]).

-type id() :: non_neg_integer().

-export([create_table/1,
         create_thread/5,
         get_thread/1, get_forum_threads/1,
         edit_title/2,
         edit_perms/2, edit_perm/4, remove_perm/3, lock_thread/1,
         new_post/2,
         delete_thread/1, delete_forum_threads/1,
         visible_threads/2,
         filter_threads/2, order_threads/1,
         id/1, forum/1, title/1, time/1, last/1, last_time/1, creator/1,
         views/1, perms/1]).

-record(ecf_thread,
        {id        :: id(),
         forum     :: ecf_forum:id(),
         title     :: string(),
         time      :: erlang:timestamp(),
         last_time :: erlang:timestamp(),
         last = 0  :: ecf_post:id(),
         creator   :: ecf_user:id(),
         views = 0 :: non_neg_integer(),
         perms = []:: [ecf_perms:perms()]}).
-type thread() :: #ecf_thread{}.

-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_thread,
                        [{attributes, record_info(fields, ecf_thread)},
                         {index, [#ecf_thread.forum]},
                         {disc_copies, Nodes}]),
    ok.

-spec get_thread(id()) -> thread() | {error, thread_not_found}.
get_thread(Id) ->
    F = fun() ->
                case mnesia:read({ecf_thread, Id}) of
                    [] -> {error, thread_not_found};
                    [T] -> T
                end
        end,
    mnesia:activity(transaction, F).

-spec get_forum_threads(ecf_forum:id()) -> [thread()].
get_forum_threads(Forum) ->
    F = fun() ->
                mnesia:index_read(ecf_thread, Forum, #ecf_thread.forum)
        end,
    mnesia:activity(transaction, F).

%% Thread actions
-spec create_thread(ecf_forum:id(), binary(), erlang:timestamp(),
                    ecf_user:id(), binary()) -> thread().
create_thread(Forum, Title, Time, Creator, Text) ->
    F = fun() ->
                Id = ecf_db:get_new_id(ecf_thread),
                mnesia:write(#ecf_thread{id=Id,forum=Forum,title=Title,
                                         time=Time,last_time=Time,
                                         creator=Creator,perms=[]}),
                ecf_post:new_post(Id, Creator, Time, Text),
                get_thread(Id)
        end,
    mnesia:activity(transaction, F).

-spec edit_title(id(), binary()) -> ok.
edit_title(Id, Title) ->
    F = fun() ->
                [Thread] = mnesia:wread({ecf_thread, Id}),
                mnesia:write(Thread#ecf_thread{title=Title})
        end,
    mnesia:activity(transaction, F).

-spec edit_perms(id(), [ecf_perms:perms()]) -> ok.
edit_perms(Id, Perms) ->
    F = fun() ->
                [Thread] = mnesia:wread({ecf_thread, Id}),
                mnesia:write(Thread#ecf_thread{perms=Perms})
        end,
    mnesia:activity(transaction, F).

-spec edit_perm(id(), ecf_perms:class(), ecf_perms:mode(), allow | deny) -> ok.
edit_perm(Id, Class, Mode, Set) ->
    F = fun() ->
                [Thread] = mnesia:wread({ecf_thread, Id}),
                NewPerms = ecf_perms:edit_perm(perms(Thread), Class, Mode, Set),
                mnesia:write(Thread#ecf_thread{perms=NewPerms})
        end,
    mnesia:activity(transaction, F).

-spec remove_perm(id(), ecf_perms:class(), ecf_perms:mode()) -> ok.
remove_perm(Id, Class, Mode) ->
    F = fun() ->
                [Thread] = mnesia:wread({ecf_thread, Id}),
                NewPerms = ecf_perms:remove_perm(perms(Thread), Class, Mode),
                mnesia:write(Thread#ecf_thread{perms=NewPerms})
        end,
    mnesia:activity(transaction, F).

% convenience function, just denies creating new posts from the base user group
-spec lock_thread(id()) -> ok.
lock_thread(Id) ->
    edit_perm(Id, {group, 1}, create_post, deny).

% Should only be called by ecf_post:new_post/4
-spec new_post(id(), erlang:timestamp()) -> ecf_post:id().
new_post(Id, Time) ->
    F = fun() ->
                [Thread] = mnesia:wread({ecf_thread, Id}),
                Last = last(Thread),
                mnesia:write(Thread#ecf_thread{last=Last+1,last_time=Time}),
                Last + 1
        end,
    mnesia:activity(transaction, F).

-spec delete_thread(id()) -> ok.
delete_thread(Id) ->
    F = fun() ->
                [_] = mnesia:wread({ecf_thread, Id}),
                ecf_post:delete_posts(Id),
                mnesia:delete({ecf_thread, Id})
        end,
    mnesia:activity(transaction, F).

-spec delete_forum_threads(ecf_forum:id()) -> ok.
delete_forum_threads(Forum) ->
    F = fun() ->
                Threads = mnesia:index_read(ecf_thread, Forum,
                                            #ecf_thread.forum),
                [delete_thread(T#ecf_thread.id) || T <- Threads]
        end,
    mnesia:activity(transaction, F).

%% Utilities

-spec visible_threads([thread()], ecf_user:user()) -> [thread()].
visible_threads(Threads, User) ->
    order_threads(filter_threads(Threads, User)).

-spec filter_threads([thread()], ecf_user:user()) -> [thread()].
filter_threads(Threads, User) ->
    lists:filter(fun(T) -> ecf_perms:check_perm_thread(User, T, view_thread)
                 end,
                 Threads).

-spec order_threads([thread()]) -> [thread()].
order_threads(Threads) ->
    F = fun(A, B) -> A#ecf_thread.last_time >= B#ecf_thread.last_time end,
    lists:sort(F, Threads).

%% Wrapper functions

-spec id(thread()) -> id().
id(Thread) ->
    Thread#ecf_thread.id.

-spec forum(thread()) -> ecf_forum:id().
forum(Thread) ->
    Thread#ecf_thread.forum.

-spec title(thread()) -> string().
title(Thread) ->
    Thread#ecf_thread.title.

-spec time(thread()) -> erlang:timestamp().
time(Thread) ->
    Thread#ecf_thread.time.

-spec last(thread()) -> ecf_post:id().
last(Thread) ->
    Thread#ecf_thread.last.

-spec last_time(thread()) -> erlang:timestamp().
last_time(Thread) ->
    Thread#ecf_thread.last_time.

-spec creator(thread()) -> ecf_user:id().
creator(Thread) ->
    Thread#ecf_thread.creator.

-spec views(thread()) -> non_neg_integer().
views(Thread) ->
    Thread#ecf_thread.views.

-spec perms(thread()) -> [ecf_perms:perms()].
perms(Thread) ->
    Thread#ecf_thread.perms.

