-module(ecf_perms).

-export_type([class/0, mode/0, perm/0]).

-export([create_table/1,
         get_global_perms/0, edit_global_perm/3, remove_global_perm/2,
         check_perm_global/2, check_perm_forum/3, check_perm_thread/3,
         check_perm_group/3,
         edit_perm/4, remove_perm/3,
         mode/1, allow/1, deny/1]).


% 'others' includes any user as well as guests
-type class() :: {user, ecf_user:id()} | {group, ecf_group:id()} | others.


% TODO: avoid duplication of the mode list
% unsure if that's possible with dialyzer specs, will need to investigate
% probably requires a parse transform or something
-define(MODES, [view_forum, view_thread, view_group, view_user,
                create_forum, create_thread, create_post, create_group,
                delete_forum, delete_thread, delete_post, delete_group,
                edit_forum, edit_thread, edit_post, edit_group, edit_user,
                delete_own_thread, edit_own_thread,
                delete_own_post, edit_own_post,
                reorder_forums,
                move_thread, lock_thread, ban_user,
                edit_perms, manage_group,
                join_group, leave_group]).

-type mode() :: view_forum | view_thread | view_group | view_user
              | create_forum | create_thread | create_post | create_group
              | delete_forum | delete_thread | delete_post | delete_group
              | edit_forum | edit_thread | edit_post | edit_group | edit_user
              | delete_own_thread | edit_own_thread
              | delete_own_post | edit_own_post
              | reorder_forums
              | move_thread | lock_thread | ban_user
              | edit_perms | manage_group
              | join_group | leave_group.

% for global perms
-record(ecf_perm,
        {mode :: mode(),
         allow = [] :: [class()],
         deny  = [] :: [class()]
        }).

-type perm() :: #ecf_perm{}.

-spec mode(perm()) -> mode().
mode(Perm) ->
    Perm#ecf_perm.mode.

-spec allow(perm()) -> [class()].
allow(Perm) ->
    Perm#ecf_perm.allow.

-spec deny(perm()) -> [class()].
deny(Perm) ->
    Perm#ecf_perm.deny.

-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_perm,
                        [{attributes, record_info(fields, ecf_perm)},
                         {disc_copies, Nodes}]),
    F = fun() ->
                [mnesia:write(#ecf_perm{mode=X}) || X <- ?MODES],
                ok
        end,
    mnesia:activity(transaction, F).

-spec edit_global_perm(class(), mode(), allow | deny) -> ok.
edit_global_perm(Class, Mode, Set) ->
    F = fun() ->
                [Old] = mnesia:wread({ecf_perm, Mode}),
                New = edit_perm(Old, Class, Set),
                mnesia:write(New)
        end,
    mnesia:activity(transaction, F).

-spec remove_global_perm(class(), mode()) -> ok.
remove_global_perm(Class, Mode) ->
    F = fun() ->
                [Perm] = mnesia:wread({ecf_perm, Mode}),
                New = remove_perm(Perm, Class),
                mnesia:write(New)
        end,
    mnesia:activity(transaction, F).

-spec get_global_perms() -> [perm()].
get_global_perms() ->
    F = fun() ->
                mnesia:read_lock_table(ecf_perm),
                % done this way to make sure the ordering is maintained
                lists:flatten([mnesia:read({ecf_perm, X}) || X <- ?MODES])
        end,
    mnesia:activity(transaction, F).

-spec get_global_perm(mode()) -> perm().
get_global_perm(Mode) ->
    F = fun() ->
                [P] = mnesia:read({ecf_perm, Mode}),
                P
        end,
    mnesia:activity(transaction, F).

-spec check_perm_global(ecf_user:user() | undefined, mode()) -> boolean().
check_perm_global(User, Mode) ->
    case check_perm(User, get_global_perm(Mode)) of
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
        none -> check_perm_global(User, Mode)
    end.

-spec check_perm_thread(ecf_user:user(), ecf_thread:thread(), mode()) -> boolean().
check_perm_thread(User, Thread, Mode) ->
    case check_perm(User, ecf_thread:perms(Thread), Mode) of
        allow -> true;
        deny -> false;
        none -> check_perm_forum(User,
                                 ecf_forum:get_forum(ecf_thread:forum(Thread)),
                                 Mode)
    end.

-spec check_perm_group(ecf_user:user(), ecf_group:group(), mode()) -> boolean().
check_perm_group(User, Group, Mode) ->
    case check_perm(User, ecf_group:perms(Group), Mode) of
        allow -> true;
        deny -> false;
        none -> check_perm_global(User, Mode)
    end.

-spec check_perm(ecf_user:user() | undefined, [perm()], mode()) ->
    allow | deny | none.
check_perm(User, Perms, Mode) ->
    case lists:keyfind(Mode, #ecf_perm.mode, Perms) of
        false ->
            none;
        Perm ->
            check_perm(User, Perm)
    end.

-spec check_perm(ecf_user:user() | undefined, perm()) -> allow | deny | none.
check_perm(undefined, Perm) ->
    check_perm_class(Perm, others);
check_perm(User, Perm) ->
    case lists:member(0, ecf_user:groups(User)) of
        true ->
            allow;
        false ->
            case check_perm_class(Perm, {user, ecf_user:id(User)}) of
                none ->
                    case check_perm_groups(Perm, ecf_user:groups(User)) of
                        none ->
                            check_perm_class(Perm, others);
                        P ->
                            P
                    end;
                P ->
                    P
            end
    end.


-spec check_perm_groups(perm(), [ecf_group:id()]) -> allow | deny | none.
check_perm_groups(Perm, Groups) ->
    check_perm_groups(Perm, Groups, none).

check_perm_groups(_, [], Res) ->
    Res;
check_perm_groups(Perm, [H|T], Res) ->
    case check_perm_class(Perm, {group, H}) of
        none ->
            check_perm_groups(Perm, T, Res);
        deny -> % deny from one group overrides allow from any other groups
            deny;
        allow ->
            check_perm_groups(Perm, T, allow)
    end.


-spec check_perm_class(perm(), class()) -> allow | deny | none.
check_perm_class(Perm, Class) ->
    case lists:member(Class, Perm#ecf_perm.deny) of
        true ->
            deny;
        false ->
            case lists:member(Class, Perm#ecf_perm.allow) of
                true ->
                    allow;
                false ->
                    none
            end
    end.

-spec edit_perm([perm()], class(), mode(), allow | deny) -> [perm()].
edit_perm(Perms, Class, Mode, Set) ->
    Perm = case lists:keyfind(Mode, #ecf_perm.mode, Perms) of
               false ->
                   #ecf_perm{mode=Mode};
               P ->
                   P
           end,
    NewPerm = edit_perm(Perm, Class, Set),
    lists:keyreplace(Mode, #ecf_perm.mode, Perms, NewPerm).

-spec edit_perm(perm(), class(), allow | deny) -> perm().
edit_perm(Perm, Class, Set) ->
    case Set of
        allow ->
            case lists:member(Class, Perm#ecf_perm.allow) of
                true ->
                    Perm;
                false ->
                    NewA = [Class|Perm#ecf_perm.allow],
                    NewD = lists:delete(Class, Perm#ecf_perm.deny),
                    Perm#ecf_perm{allow=NewA, deny=NewD}
            end;
        deny ->
            case lists:member(Class, Perm#ecf_perm.deny) of
                true ->
                    Perm;
                false ->
                    NewA = lists:delete(Class, Perm#ecf_perm.allow),
                    NewD = [Class|Perm#ecf_perm.deny],
                    Perm#ecf_perm{allow=NewA, deny=NewD}
            end
    end.

-spec remove_perm([perm()], class(), mode()) -> [perm()].
remove_perm(Perms, Class, Mode) ->
    case lists:keyfind(Mode, #ecf_perm.mode, Perms) of
        false ->
            Perms;
        Perm ->
            NewPerm = remove_perm(Perm, Class),
            lists:keyreplace(Mode, #ecf_perm.mode, Perms, NewPerm)
    end.

-spec remove_perm(perm(), class()) -> perm().
remove_perm(Perm, Class) ->
    NewA = lists:delete(Class, Perm#ecf_perm.allow),
    NewD = lists:delete(Class, Perm#ecf_perm.deny),
    Perm#ecf_perm{allow=NewA, deny=NewD}.

