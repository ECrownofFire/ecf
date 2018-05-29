-module(ecf_group).

-export_type([id/0, group/0]).

-export([create_table/1,
         new_group/2, delete_group/1,
         get_group/1,
         edit_name/2, edit_desc/2,
         add_member/2, remove_member/2,
         id/1, name/1, desc/1, member/2, members/1]).

%%% Wrapper for group type

-type id() :: non_neg_integer().


-record(ecf_group,
        {id   :: id(),
         name :: string(),
         desc :: string(),
         members = [] :: [ecf_user:id()]}).
-type group() :: #ecf_group{}.

-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_group,
                        [{attributes, record_info(fields, ecf_group)},
                         {disc_copies, Nodes}]),
    ok.

-spec new_group(binary(), binary()) -> ok.
new_group(Name, Desc) ->
    F = fun() ->
                Id = ecf_db:get_new_id(ecf_group),
                mnesia:write(#ecf_group{id=Id,name=Name,desc=Desc})
        end,
    mnesia:activity(transaction, F).

-spec delete_group(id()) -> ok.
delete_group(Id) ->
    F = fun() ->
                [_] = mnesia:wread({ecf_group, Id}),
                mnesia:delete(Id)
        end,
    mnesia:activity(transaction, F).

-spec get_group(id()) -> ok.
get_group(Id) ->
    F = fun() ->
                [Group] = mnesia:read({ecf_group, Id}),
                Group
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

%% Wrapper functions

-spec id(group()) -> id().
id(Group) ->
    Group#ecf_group.id.

-spec name(group()) -> string().
name(Group) ->
    Group#ecf_group.name.

-spec desc(group()) -> string().
desc(Group) ->
    Group#ecf_group.desc.

-spec member(group(), ecf_user:id()) -> boolean().
member(Group, User) ->
    lists:member(User, Group#ecf_group.members).

-spec members(group()) -> [ecf_user:id()].
members(Group) ->
    Group#ecf_group.members.

