-module(ecf_group).

-export_type([id/0, group/0]).

-export([create_table/1,
         new_group/2, delete_group/1,
         get_group/1,
         edit_name/2, edit_desc/2,
         add_perm/2, remove_perm/2,
         add_member/2, remove_member/2,
         id/1, name/1, desc/1, member/2, members/1, perms/1]).

%%% Wrapper for group type

-type id() :: non_neg_integer().


-record(ecf_group,
        {id   :: id(),
         name :: binary(),
         desc :: binary(),
         members = [] :: [ecf_user:id()],
         perms = [] :: [ecf_perms:perms()]}).
-type group() :: #ecf_group{}.

-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_group,
                        [{attributes, record_info(fields, ecf_group)},
                         {disc_copies, Nodes}]),
    F = fun() ->
                mnesia:write(#ecf_group{id=0,
                                        name = <<"Administrators">>,
                                        desc = <<"Administrator group">>})
        end,
    ok = mnesia:activity(transaction, F),
    1 = new_group(<<"Registered Users">>, <<"Default user group">>),
    ok.


-spec new_group(binary(), binary()) -> id().
new_group(Name, Desc) ->
    F = fun() ->
                Id = ecf_db:get_new_id(ecf_group),
                ok = mnesia:write(#ecf_group{id=Id,name=Name,desc=Desc}),
                Id
        end,
    mnesia:activity(transaction, F).

% TODO: search for group in users and permissions to delete it
-spec delete_group(id()) -> ok.
delete_group(Id) ->
    F = fun() ->
                [_] = mnesia:wread({ecf_group, Id}),
                mnesia:delete(Id)
        end,
    mnesia:activity(transaction, F).

-spec get_group(id()) -> ok | {error, group_not_found}.
get_group(Id) ->
    F = fun() ->
                case mnesia:read({ecf_group, Id}) of
                    [] -> {error, group_not_found};
                    [Group] -> Group
                end
        end,
    mnesia:activity(transaction, F).

-spec edit_name(id(), binary()) -> ok.
edit_name(Id, Name) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                mnesia:write(G#ecf_group{name=Name})
        end,
    mnesia:activity(transaction, F).

-spec edit_desc(id(), binary()) -> ok.
edit_desc(Id, Desc) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                mnesia:write(G#ecf_group{desc=Desc})
        end,
    mnesia:activity(transaction, F).

-spec add_member(id(), ecf_user:id()) -> ok.
add_member(Id, User) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                _ = ecf_user:get_user(User),
                ecf_user:add_group(User, Id),
                NewList = [User|G#ecf_group.members],
                mnesia:write(G#ecf_group{members=NewList})
        end,
    mnesia:activity(transaction, F).

-spec remove_member(id(), ecf_user:id()) -> ok.
remove_member(Id, User) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                _ = ecf_user:get_user(User),
                ecf_user:remove_group(User, Id),
                NewList = lists:delete(User, G#ecf_group.members),
                mnesia:write(G#ecf_group{members=NewList})
        end,
    mnesia:activity(transaction, F).

-spec add_perm(id(), ecf_perms:perm()) -> ok.
add_perm(Id, Perm) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                New = [Perm|perms(G)],
                mnesia:write(G#ecf_group{perms=New})
        end,
    mnesia:activity(transaction, F).

-spec remove_perm(id(), ecf_perms:perm()) -> ok.
remove_perm(Id, Perm) ->
    F = fun() ->
                [G] = mnesia:wread({ecf_group, Id}),
                New = lists:delete(Perm, perms(G)),
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

-spec perms(group()) -> [ecf_perms:perms()].
perms(Group) ->
    Group#ecf_group.perms.

