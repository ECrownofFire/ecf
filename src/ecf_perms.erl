-module(ecf_perms).

-export_type([perms/0, mode/0]).

-export([create_perm/2, check_perm/3]).


-type perm_type() :: {user, ecf_user:id()} | {group, ecf_group:id()} | others.

% most of these are self-explanatory
-type mode() :: view_forum | view_thread | view_post
              | create_thread | create_post | delete_thread | delete_post
              | edit_thread | edit_post
              | ban_user.

-type perms() :: {perm_type(), [mode()]}.


-spec create_perm(perm_type(), [mode()]) -> perms().
create_perm(others, Modes) ->
    {others, Modes};
create_perm({Type, Id}, Modes) when Type =:= user; Type =:= group ->
    {{Type, Id}, Modes}.


-spec check_perm(ecf_user:user() | undefined,
                 {thread, ecf_thread:thread()} | {forum, ecf_forum:forum()},
                 mode()) -> boolean().
check_perm(undefined, {thread, Thread}, Mode) ->
    check_perm_groups(Mode, ecf_thread:perms(Thread), [1]);
check_perm(undefined, {forum, Forum}, Mode) ->
    check_perm_groups(Mode, ecf_forum:perms(Forum), [1]);
check_perm(User, {thread, Thread}, Mode) ->
    check_perm2(ecf_thread:perms(Thread), User, Mode);
check_perm(User, {forum, Forum}, Mode) ->
    check_perm2(ecf_forum:perms(Forum), User, Mode).

check_perm2(Perms, User, Mode) ->
    UserPerm = case lists:keyfind({user, ecf_user:id(User)}, 1, Perms) of
                   {_, Modes} ->
                       lists:member(Mode, Modes);
                   false ->
                       false
               end,
    case UserPerm of
        true -> true;
        false ->
            Groups = ecf_user:groups(User),
            check_perm_groups(Mode, Perms, Groups)
    end.

check_perm_groups(Mode, Perms, Groups) ->
    check_perm_groups(Mode, Perms, Groups, false).

check_perm_groups(_, _, _, true) ->
    true;
check_perm_groups(_, _, [], false) ->
    false;
check_perm_groups(Mode, Perms, [Head|Tail], false) ->
    case Head of
        0 -> true; % Administrator group always has permissions
        _ ->
            case lists:keyfind({group, Head}, 1, Perms) of
                {_, Modes} ->
                    check_perm_groups(Mode, Perms, Tail, lists:member(Mode, Modes));
                _ ->
                    check_perm_groups(Mode, Perms, Tail, false)
            end
    end.

