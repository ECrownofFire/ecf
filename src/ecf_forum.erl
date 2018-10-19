-module(ecf_forum).

%%% Wrapper for the forum type

-export_type([id/0, forum/0]).

-export([create_table/1,
         get_forum/1, get_forums/0,
         new_forum/2,
         edit_name/2, edit_desc/2, reorder/2,
         edit_perm/4, remove_perm/3,
         delete_forum/1,
         visible_forums/2,
         filter_forums/2, order_forums/1,
         id/1, name/1, desc/1, order/1, perms/1]).


-type id()  :: non_neg_integer().

-record(ecf_forum,
        {id    :: id(),
         order :: integer(),
         name  :: binary(),
         desc  :: binary(),
         perms = [] :: [ecf_perms:perm()]}).
-type forum() :: #ecf_forum{}.


-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_forum,
                        [{attributes, record_info(fields, ecf_forum)},
                         {disc_copies, Nodes}]),
    F = fun() ->
                mnesia:write(#ecf_forum{id = 0,
                                        order = -999,
                                        name = <<"Private Messages">>,
                                        desc = <<"Your private messages">>})
        end,
    ok = mnesia:activity(transaction, F),
    % defaults for PM forum, overriden by PM threads
    ok = edit_perm(0, {group, 1}, view_forum, allow),
    ok = edit_perm(0, others, view_thread, deny),
    ok = edit_perm(0, {group, 2}, create_thread, allow),
    ok = edit_perm(0, {group, 3}, create_thread, deny),
    ok = edit_perm(0, others, create_post, deny).

%% Forum actions

-spec get_forum(id()) -> forum() | undefined.
get_forum(Id) ->
    F = fun() ->
                case mnesia:read({ecf_forum, Id}) of
                    [] -> undefined;
                    [Forum] -> Forum
                end
        end,
    mnesia:activity(transaction, F).

-spec get_forums() -> [forum()].
get_forums() ->
    F = fun() ->
                mnesia:read_lock_table(ecf_forum),
                Head = {ecf_forum, '$1', '_', '_', '_', '_'},
                Guard = {'>', '$1', 0}, % skip PM forum
                mnesia:select(ecf_forum,[{Head,[Guard],['$_']}])
        end,
    mnesia:activity(transaction, F).


-spec new_forum(binary(), binary()) -> id().
new_forum(Name, Desc) ->
    F = fun() ->
                Order = max_order(get_forums()) + 1,
                Id = ecf_db:get_new_id(ecf_forum),
                mnesia:write(#ecf_forum{id=Id, name=Name, desc=Desc,
                                        order=Order}),
                Id
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


-spec reorder(id(), top | bottom | up | down) -> ok.
reorder(Id, Action) when Action =:= top; Action =:= bottom ->
    F = fun() ->
                [Forum] = mnesia:wread({ecf_forum, Id}),
                Old = order(Forum),
                case get_reorder(Action, get_forums()) of
                    Old ->
                        ok;
                    O ->
                        mnesia:write(Forum#ecf_forum{order=O})
                end
        end,
    mnesia:activity(transaction, F);
reorder(Id, Action) when Action =:= up; Action =:= down ->
    F = fun() ->
                [Forum] = mnesia:wread({ecf_forum, Id}),
                Old = order(Forum),
                Target = get_reorder(Action, Old, order_forums(get_forums())),
                New = order(Target),
                mnesia:write(Forum#ecf_forum{order=New}),
                mnesia:write(Target#ecf_forum{order=Old})
        end,
    mnesia:activity(transaction, F).


-spec get_reorder(top | bottom, [forum()]) -> integer().
get_reorder(top, Forums) ->
    min_order(Forums) - 1;
get_reorder(bottom, Forums) ->
    max_order(Forums) + 1.

-spec get_reorder(up | down, integer(), [forum()]) -> forum().
get_reorder(up, Old, Forums) ->
    case order(hd(Forums)) of
        Old -> % already at top
            hd(Forums);
        _ ->
            lists:last(lists:takewhile(fun(X) -> order(X) =/= Old end, Forums))
    end;
get_reorder(down, Old, Forums) ->
    case lists:splitwith(fun(X) -> order(X) =/= Old end, Forums) of
        {_, [_,T|_]} -> % hd is the forum we're reordering
            T;
        {_, [H]} -> % already at bottom
            H
    end.

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

-spec order(forum()) -> integer().
order(Forum) ->
    Forum#ecf_forum.order.

-spec name(forum()) -> binary().
name(Forum) ->
    Forum#ecf_forum.name.

-spec desc(forum()) -> binary().
desc(Forum) ->
    Forum#ecf_forum.desc.

-spec perms(forum()) -> [ecf_perms:perm()].
perms(Forum) ->
    Forum#ecf_forum.perms.

%% Utilities
max_order([]) ->
    0;
max_order(Forums) ->
    Max = fun(X, M) -> case X#ecf_forum.order of
                           O when O > M -> O;
                           _ -> M
                       end
          end,
    % -INF doesn't exist in erlang, so just grab the order of the first one
    lists:foldl(Max, order(hd(Forums)), Forums).

min_order([]) ->
    0;
min_order(Forums) ->
    Max = fun(X, M) -> case X#ecf_forum.order of
                           O when O < M -> O;
                           _ -> M
                       end
          end,
    lists:foldl(Max, infinity, Forums). % numbers < atoms

