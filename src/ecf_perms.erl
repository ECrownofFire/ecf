-module(ecf_perms).

-export_type([perm/0, perms/0, mode/0, global_mode/0, global_perms/0]).

-export([check_perm/3, add_perm/2, remove_perm/2]).


% 'others' includes any user who isn't listed in other perms, as well as guests
-type perm_type() :: {user, ecf_user:id()} | {group, ecf_group:id()} | others.

-type mode() :: view_forum | view_thread
              | create_forum | create_thread | create_post
              | delete_forum | delete_thread | delete_post
              | edit_forum | edit_thread | edit_post
              | move_thread | lock_thread.

-type perm() :: {perm_type(), mode()}.
-type perms() :: {perm_type(), [mode()]}.


-type global_mode() :: mode() | ban_user.
-type global_perms() :: [global_mode()].


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
check_perm([], User, Mode, false) ->
    Groups = ecf_user:groups(User),
    lists:any(fun(G) ->
                      Modes = ecf_group:perms(G),
                      lists:member(Mode, Modes) end,
              Groups);
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

-spec add_perm([perms()], perm()) -> [perms()].
add_perm(Perms, P = {Type, Mode}) ->
    case lists:keyfind(Type, 1, Perms) of
        false ->
            [P|Perms];
        {Type, Modes} ->
            case lists:member(Mode, Modes) of
                true ->
                    Perms;
                false ->
                    lists:keyreplace(Type, 1, Perms, {Type, [Mode|Modes]})
            end
    end.

-spec remove_perm([perms()], perm()) -> [perms()].
remove_perm(Perms, {Type, Mode}) ->
    case lists:keyfind(Type, 1, Perms) of
        false ->
            Perms;
        {Type, Modes} ->
            case lists:member(Mode, Modes) of
                false ->
                    Perms;
                true ->
                    NewModes = lists:delete(Mode, Modes),
                    lists:keyreplace(Type, 1, Perms, {Type, NewModes})
            end
    end.

