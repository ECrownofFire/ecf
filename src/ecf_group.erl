-module(ecf_group).

-export_type([id/0, group/0]).

-export([create_table/1,
         new_group/2, delete_group/1,
         get_groups/0, get_group/1,
         filter_groups/2,
         edit_name/2, edit_desc/2,
         edit_perm/4, remove_perm/3,
         add_member/2, remove_member/2,
         id/1, name/1, desc/1, member/2, members/1, perms/1]).

%%% Wrapper for group type

-type id() :: non_neg_integer().


-record(ecf_group,
        {id   :: id(),
         name :: binary(),
         desc :: binary(),
         members = [] :: [ecf_user:id()],
         perms = [] :: [ecf_perms:perm()]}).
-opaque group() :: #ecf_group{}.

-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_group,
                        [{attributes, record_info(fields, ecf_group)},
                         {disc_copies, Nodes}]),
    F = fun() ->
                % this one has to be manually created since the ID is 0, and the
                % automatic stuff starts from 1
                mnesia:write(#ecf_group{id=0,
                                        name = <<"Administrators">>,
                                        desc = <<"Administrator group">>})
        end,
    ok = mnesia:activity(transaction, F),
    1 = new_group(<<"Registered Users">>, <<"Default user group">>),
    2 = new_group(<<"Confirmed Email Users">>, <<"Users with a confirmed email">>),
    3 = new_group(<<"Banned Users">>, <<"Banned users">>),
    ok.


-spec new_group(binary(), binary()) -> id().
new_group(Name, Desc) when is_binary(Name), is_binary(Desc) ->
    F = fun() ->
                Id = ecf_db:get_new_id(ecf_group),
                ok = mnesia:write(#ecf_group{id=Id,name=Name,desc=Desc}),
                Id
        end,
    mnesia:activity(transaction, F).

% TODO: search for group in permissions to delete it
-spec delete_group(id()) -> ok.
delete_group(Id) when is_integer(Id), Id >= 4 ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                [ecf_user:remove_group(U, Id) || U <- members(G)],
                mnesia:delete({ecf_group, Id})
        end,
    mnesia:activity(transaction, F).


-spec get_groups() -> [group()].
get_groups() ->
    F = fun() ->
                mnesia:read_lock_table(ecf_group),
                mnesia:select(ecf_group, [{'_',[],['$_']}])
        end,
    mnesia:activity(transaction, F).

-spec get_group(id()) -> group() | undefined.
get_group(Id) ->
    F = fun() ->
                case mnesia:read({ecf_group, Id}) of
                    [] -> undefined;
                    [Group] -> Group
                end
        end,
    mnesia:activity(transaction, F).

-spec filter_groups(ecf_user:user(), [group()]) -> [group()].
filter_groups(User, Groups) ->
    F = fun(X) -> ecf_perms:check_perm_group(User, X, view_group) end,
    lists:filter(F, Groups).

-spec edit_name(id(), binary()) -> ok.
edit_name(Id, Name) when is_binary(Name) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                mnesia:write(G#ecf_group{name=Name})
        end,
    mnesia:activity(transaction, F).

-spec edit_desc(id(), binary()) -> ok.
edit_desc(Id, Desc) when is_binary(Desc) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                mnesia:write(G#ecf_group{desc=Desc})
        end,
    mnesia:activity(transaction, F).

-spec add_member(id(), ecf_user:id()) -> ok.
add_member(Id, User) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                case lists:member(User, members(G)) of
                    true ->
                        ok;
                    false ->
                        ecf_user:add_group(User, Id),
                        NewList = [User|members(G)],
                        mnesia:write(G#ecf_group{members=NewList})
                end
        end,
    mnesia:activity(transaction, F).

-spec remove_member(id(), ecf_user:id()) -> ok.
remove_member(Id, User) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                ecf_user:remove_group(User, Id),
                NewList = lists:delete(User, G#ecf_group.members),
                mnesia:write(G#ecf_group{members=NewList})
        end,
    mnesia:activity(transaction, F).

-spec edit_perm(id(), ecf_perms:class(), ecf_perms:mode(), allow | deny) -> ok.
edit_perm(Id, Class, Mode, Set) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                New = ecf_perms:edit_perm(perms(G), Class, Mode, Set),
                mnesia:write(G#ecf_group{perms=New})
        end,
    mnesia:activity(transaction, F).

-spec remove_perm(id(), ecf_perms:class(), ecf_perms:mode()) -> ok.
remove_perm(Id, Class, Mode) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                New = ecf_perms:remove_perm(perms(G), Class, Mode),
                mnesia:write(G#ecf_group{perms=New})
        end,
    mnesia:activity(transaction, F).

%% Wrapper functions

-spec id(group()) -> id().
id(Group) ->
    Group#ecf_group.id.

-spec name(group()) -> binary().
name(Group) ->
    Group#ecf_group.name.

-spec desc(group()) -> binary().
desc(Group) ->
    Group#ecf_group.desc.

-spec member(group(), ecf_user:id()) -> boolean().
member(Group, User) ->
    lists:member(User, Group#ecf_group.members).

-spec members(group()) -> [ecf_user:id()].
members(Group) ->
    Group#ecf_group.members.

-spec perms(group()) -> [ecf_perms:perm()].
perms(Group) ->
    Group#ecf_group.perms.

