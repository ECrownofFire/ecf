-module(ecf_perms).

-export_type([perms/0, mode/0]).

-export([create_perm/2, check_perm/3]).


% 'others' includes any user who isn't listed in other perms, as well as guests
-type perm_type() :: {user, ecf_user:id()} | {group, ecf_group:id()} | others.

% most of these are self-explanatory
-type mode() :: view_forum | view_thread | view_post
              | create_thread | create_post | delete_thread | delete_post
              | edit_thread | edit_post
              | move_thread | lock_thread | ban_user.

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
    check_perm_guest(Mode, ecf_thread:perms(Thread));
check_perm(undefined, {forum, Forum}, Mode) ->
    check_perm_guest(Mode, ecf_forum:perms(Forum));
check_perm(User, {thread, Thread}, Mode) ->
    case lists:member(0, ecf_user:groups(User)) of
        true -> true;
        false -> check_perm(ecf_thread:perms(Thread), User, Mode, false)
    end;
check_perm(User, {forum, Forum}, Mode) ->
    case lists:member(0, ecf_user:groups(User)) of
        true -> true;
        false -> check_perm(ecf_forum:perms(Forum), User, Mode, false)
    end.

check_perm_guest(Mode, Perms) ->
    case lists:keyfind(others, 1, Perms) of
        {_, List} ->
            lists:member(Mode, List);
        false ->
            false
    end.

-spec check_perm([perms()], ecf_user:user(), mode(), boolean()) -> boolean().
check_perm([], _, _, false) ->
    false;
check_perm(_, _, _, true) ->
    true;
check_perm([{{user, Id}, Modes}|Tail], User, Mode, false) ->
    case ecf_user:id(User) of
        Id ->
            check_perm(Tail, User, Mode, lists:member(Mode, Modes));
        _ ->
            check_perm(Tail, User, Mode, false)
    end;
check_perm([{{group, Id}, Modes}|Tail], User, Mode, false) ->
    Groups = ecf_user:groups(User),
    case lists:member(Id, Groups) of
        false ->
            check_perm(Tail, User, Mode, false);
        true ->
            check_perm(Tail, User, Mode, lists:member(Mode, Modes))
    end;
check_perm([{others, Modes}|Tail], User, Mode, false) ->
    check_perm(Tail, User, Mode, lists:member(Mode, Modes)).

