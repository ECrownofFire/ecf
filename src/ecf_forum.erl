-module(ecf_forum).

%%% Wrapper for the forum type

-export_type([id/0, forum/0]).

-export([create_table/1,
         get_forum/1, get_forums/0,
         new_forum/4,
         edit_name/2, edit_desc/2, edit_perms/2, edit_order/2,
         delete_forum/1,
         order_forums/1,
         id/1, name/1, desc/1, order/1, perms/1]).


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

-spec get_forum(id()) -> forum().
get_forum(Id) ->
    F = fun() ->
                [Forum] = mnesia:read({ecf_forum, Id}),
                Forum
        end,
    mnesia:activity(transaction, F).

-spec get_forums() -> [forum()].
get_forums() ->
    F = fun() ->
                mnesia:read_lock_table(ecf_forum),
                mnesia:select(ecf_forum,[{'_',[],['$_']}])
        end,
    mnesia:activity(transaction, F).

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

%% Utilities

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

