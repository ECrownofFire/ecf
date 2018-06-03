-module(ecf_forum).

%%% Wrapper for the forum type

-export_type([id/0, forum/0]).

-export([create_table/1,
         get_forum/1, get_forums/0,
         new_forum/4,
         edit_name/2, edit_desc/2, edit_order/2,
         edit_perms/2, edit_perm/4, remove_perm/3,
         delete_forum/1,
         visible_forums/2,
         filter_forums/2, order_forums/1,
         id/1, name/1, desc/1, order/1, perms/1]).

-export([new_forum/3]).


-type id()  :: non_neg_integer().

-record(ecf_forum,
        {id    :: id(),
         order :: non_neg_integer(),
         name  :: binary(),
         desc  :: binary(),
         perms :: [ecf_perms:perms()]}).
-type forum() :: #ecf_forum{}.


-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_forum,
                        [{attributes, record_info(fields, ecf_forum)},
                         {disc_copies, Nodes}]),
    ok.

%% Forum actions

-spec get_forum(id()) -> forum() | {error, forum_not_found}.
get_forum(Id) ->
    F = fun() ->
                case mnesia:read({ecf_forum, Id}) of
                    [] -> {error, forum_not_found};
                    [Forum] -> Forum
                end
        end,
    mnesia:activity(transaction, F).

-spec get_forums() -> [forum()].
get_forums() ->
    F = fun() ->
                mnesia:read_lock_table(ecf_forum),
                mnesia:select(ecf_forum,[{'_',[],['$_']}])
        end,
    mnesia:activity(transaction, F).

% should only be used for testing
-spec new_forum(binary(), binary(), id()) -> ok.
new_forum(Name, Desc, Order) ->
    new_forum(Name, Desc, Order, []).

-spec new_forum(binary(), binary(), id(), [ecf_perms:perms()]) -> ok.
new_forum(Name, Desc, Order, Perms) ->
    F = fun() ->
                Id = ecf_db:get_new_id(ecf_forum),
                mnesia:write(#ecf_forum{id=Id, name=Name, desc=Desc,
                                        order=Order, perms=Perms})
        end,
    mnesia:activity(transaction, F).

-spec delete_forum(id()) -> ok.
delete_forum(Id) ->
    F = fun() ->
                % bail out fast if the forum doesn't exist
                [_] = mnesia:wread({ecf_forum, Id}),
                ecf_thread:delete_forum_threads(Id),
                mnesia:delete({ecf_forum, Id})
        end,
    mnesia:activity(transaction, F).

%% Forum editing

-spec edit_name(id(), binary()) -> ok.
edit_name(Id, Name) ->
    F = fun() ->
                [Forum] = mnesia:wread({ecf_forum, Id}),
                mnesia:write(Forum#ecf_forum{name=Name})
        end,
    mnesia:activity(transaction, F).

-spec edit_desc(id(), binary()) -> ok.
edit_desc(Id, Desc) ->
    F = fun() ->
                [Forum] = mnesia:wread({ecf_forum, Id}),
                mnesia:write(Forum#ecf_forum{desc=Desc})
        end,
    mnesia:activity(transaction, F).

-spec edit_order(id(), non_neg_integer()) -> ok.
edit_order(Id, Order) ->
    F = fun() ->
                [Forum] = mnesia:wread({ecf_forum, Id}),
                mnesia:write(Forum#ecf_forum{order=Order})
        end,
    mnesia:activity(transaction, F).

-spec edit_perms(id(), ecf_perms:perms()) -> ok.
edit_perms(Id, Perms) ->
    F = fun() ->
                [Forum] = mnesia:wread({ecf_forum, Id}),
                mnesia:write(Forum#ecf_forum{perms=Perms})
        end,
    mnesia:activity(transaction, F).

-spec edit_perm(id(), ecf_perms:class(), ecf_perms:mode(), allow | deny) -> ok.
edit_perm(Id, Class, Mode, Set) ->
    F = fun() ->
                [Forum] = mnesia:wread({ecf_forum, Id}),
                NewPerms = ecf_perms:edit_perm(perms(Forum), Class, Mode, Set),
                mnesia:write(Forum#ecf_forum{perms=NewPerms})
        end,
    mnesia:activity(transaction, F).

-spec remove_perm(id(), ecf_perms:class(), ecf_perms:mode()) -> ok.
remove_perm(Id, Class, Mode) ->
    F = fun() ->
                [Forum] = mnesia:wread({ecf_forum, Id}),
                NewPerms = ecf_perms:remove_perm(perms(Forum), Class, Mode),
                mnesia:write(Forum#ecf_forum{perms=NewPerms})
        end,
    mnesia:activity(transaction, F).

%% Utilities

-spec visible_forums([forum()], ecf_user:user()) -> [forum()].
visible_forums(Forums, User) ->
    order_forums(filter_forums(Forums, User)).

-spec filter_forums([forum()], ecf_user:user()) -> [forum()].
filter_forums(Forums, User) ->
    lists:filter(fun(F) -> ecf_perms:check_perm_forum(User, F, view_forum)
                 end,
                 Forums).

-spec order_forums([forum()]) -> [forum()].
order_forums(Forums) ->
    lists:keysort(#ecf_forum.order, Forums).


%% Wrapper functions

-spec id(forum()) -> id().
id(Forum) ->
    Forum#ecf_forum.id.

-spec order(forum()) -> non_neg_integer().
order(Forum) ->
    Forum#ecf_forum.order.

-spec name(forum()) -> binary().
name(Forum) ->
    Forum#ecf_forum.name.

-spec desc(forum()) -> binary().
desc(Forum) ->
    Forum#ecf_forum.desc.

-spec perms(forum()) -> [ecf_perms:perms()].
perms(Forum) ->
    Forum#ecf_forum.perms.

