-module(ecf_perms).

-export_type([class/0, mode/0, perms/0]).

-export([create_table/1, edit_global_perm/1, remove_global_perm/1,
         check_perm_forum/3, check_perm_thread/3, check_perm_group/3,
         edit_perm/4, remove_perm/3]).


% 'others' includes any user as well as guests
-type class() :: {user, ecf_user:id()} | {group, ecf_group:id()} | others.

-type mode() :: view_forum | view_thread | view_group | view_user
              | create_forum | create_thread | create_post | create_group
              | delete_forum | delete_thread | delete_post | delete_group
              | edit_forum | edit_thread | edit_post | edit_group | edit_user
              | move_thread | lock_thread | ban_user
              | manage_group.

% class, allowed, denied
-type perms() :: {class(), [mode()], [mode()]}.

% for global perms
-record(ecf_perm,
        {dummy = 0  :: integer(),
         perms = [] :: [perms()]}).

-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_perm,
                        [{attributes, record_info(fields, ecf_perm)},
                         {disc_copies, Nodes}]),
    F = fun() ->
                mnesia:write(#ecf_perm{})
        end,
    mnesia:activity(transaction, F).

edit_global_perm(Perm = {Class, _, _}) ->
    F = fun() ->
                [Perms] = mnesia:wread({ecf_perm, 0}),
                Old = Perms#ecf_perm.perms,
                New = lists:keystore(Class, 1, Old, Perm),
                mnesia:write(Perms#ecf_perm{perms=New})
        end,
    mnesia:activity(transaction, F).

remove_global_perm(Class) ->
    F = fun() ->
                [Perms] = mnesia:wread({ecf_perm, 0}),
                Old = Perms#ecf_perm.perms,
                New = lists:keydelete(Class, 1, Old),
                mnesia:write(Perms#ecf_perm{perms=New})
        end,
    mnesia:activity(transaction, F).

get_global_perms() ->
    F = fun() ->
                [P] = mnesia:read({ecf_perm, 0}),
                P#ecf_perm.perms
        end,
    mnesia:activity(transaction, F).

-spec check_perm_global(ecf_user:user(), mode()) -> boolean().
check_perm_global(User, Mode) ->
    case check_perm(User, get_global_perms(), Mode) of
        allow ->
            true;
        _ ->
            false
    end.

-spec check_perm_forum(ecf_user:user(), ecf_forum:forum(), mode()) -> boolean().
check_perm_forum(User, Forum, Mode) ->
    case check_perm(User, ecf_forum:perms(Forum), Mode) of
        allow -> true;
        deny -> false;
        _ -> check_perm_global(User, Mode)
    end.

-spec check_perm_thread(ecf_user:user(), ecf_thread:thread(), mode()) -> boolean().
check_perm_thread(User, Thread, Mode) ->
    case check_perm(User, ecf_thread:perms(Thread), Mode) of
        allow -> true;
        deny -> false;
        _ -> check_perm_forum(User,
                              ecf_forum:get_forum(ecf_thread:forum(Thread)),
                              Mode)
    end.

-spec check_perm_group(ecf_user:user(), ecf_group:group(), mode()) -> boolean().
check_perm_group(User, Group, Mode) ->
    case check_perm(User, ecf_group:perms(Group), Mode) of
        allow -> true;
        deny -> false;
        _ -> check_perm_global(User, Mode)
    end.

-spec check_perm(ecf_user:user() | undefined, [perms()], mode()) ->
    allow | deny | none.
check_perm(undefined, Perms, Mode) ->
    check_perm_guest(Mode, Perms);
check_perm(User, Perms, Mode) ->
    case lists:member(0, ecf_user:groups(User)) of
        true -> allow;
        false -> check_perm(Perms, User, Mode, none)
    end.

-spec check_perm_guest(mode(), [perms()]) -> allow | deny | none.
check_perm_guest(Mode, Perms) ->
    case lists:keyfind(others, 1, Perms) of
        {_, A, D} ->
            case lists:member(Mode, D) of
                true -> deny;
                false ->
                    case lists:member(Mode, A) of
                        true -> allow;
                        false -> none
                    end
            end;
        false ->
            none
    end.

-spec check_perm([perms()], ecf_user:user(), mode(),
                 none | {others | group, allow | deny}) -> allow | deny | none.
check_perm([], _, _, {_, Status}) ->
    Status;
check_perm([], _, _, none) ->
    none;
check_perm([{others, A, D}|Tail], User, Mode, none) ->
    % others permission doesn't take precedence over anything else
    case lists:member(Mode, D) of
        true -> check_perm(Tail, User, Mode, {others, deny});
        false ->
            case lists:member(Mode, A) of
                true -> check_perm(Tail, User, Mode, {others, allow});
                false -> check_perm(Tail, User, Mode, none)
            end
    end;
check_perm([{others, _, _}|Tail], User, Mode, Status) ->
    check_perm(Tail, User, Mode, Status);
check_perm([{{user, Id}, A, D}|Tail], User, Mode, Status) ->
    case ecf_user:id(User) of
        Id ->
            case lists:member(Mode, D) of
                true -> deny; % user-specific deny overrides everything
                false ->
                    case lists:member(Mode, A) of
                        true -> allow; % user-specific allow also overrides all
                        false -> check_perm(Tail, User, Mode, Status)
                    end
            end;
        _ -> check_perm(Tail, User, Mode, Status)
    end;
check_perm([{{group, _}, _, _}|Tail], User, Mode, {group, deny}) ->
    % group deny always takes precedence over group allow
    check_perm(Tail, User, Mode, {group, deny});
check_perm([{{group, Id}, A, D}|Tail], User, Mode, Status) ->
    % group takes precedence over others, but not over user-specific
    Groups = ecf_user:groups(User),
    case lists:member(Id, Groups) of
        true ->
            case lists:member(Mode, D) of
                true -> check_perm(Tail, User, Mode, {group, deny});
                false ->
                    case lists:member(Mode, A) of
                        true -> check_perm(Tail, User, Mode, {group, allow});
                        false -> check_perm(Tail, User, Mode, Status)
                    end
            end;
        false ->
            check_perm(Tail, User, Mode, Status)
    end.


-spec edit_perm([perms()], class(), mode(), allow | deny) -> [perms()].
edit_perm(Perms, Class, Mode, Set) ->
    case lists:keyfind(Class, 1, Perms) of
        false ->
            case Set of
                allow -> [{Class, [Mode], []}|Perms];
                deny -> [{Class, [], [Mode]}|Perms]
            end;
        {Class, A, D} ->
            NewD = case Set of
                       allow ->
                           lists:delete(Mode, D);
                       deny ->
                           case lists:member(Mode, D) of
                               true -> D;
                               false -> [Mode|D]
                           end
                   end,
            NewA = case Set of
                       deny ->
                           lists:delete(Mode, A);
                       allow ->
                           case lists:member(Mode, A) of
                               true -> A;
                               false -> [Mode|A]
                           end
                   end,
            lists:keyreplace(Class, 1, Perms, {Class, NewA, NewD})
    end.

-spec remove_perm([perms()], class(), mode()) -> [perms()].
remove_perm(Perms, Class, Mode) ->
    case lists:keyfind(Class, 1, Perms) of
        false ->
            Perms;
        {Class, A, D} ->
            NewA = lists:delete(Mode, A),
            NewD = lists:delete(Mode, D),
            lists:keyreplace(Class, 1, Perms, {Class, NewA, NewD})
    end.

