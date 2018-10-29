-module(ecf_ban).

-export([create_table/1,
         check_perm/2,
         new_ban/5,
         get_bans/0,
         check_ban/1,
         delete_ban/1,
         user/1, by/1, reason/1, time/1, until/1]).

-record(ecf_ban,
        {user   :: ecf_user:id(),
         by     :: ecf_user:id(),
         reason :: binary(),
         time   :: erlang:timestamp(),
         until  :: erlang:timestamp()}).
-type ban() :: #ecf_ban{}.


-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_ban,
                        [{attributes, record_info(fields, ecf_ban)},
                         {disc_copies, Nodes}]),
    ok.


% user A is allowed to ban a target B only if A is allowed ban_user in a group
% that B is in (or globally), and is not denied it in any of B's other groups.
-spec check_perm(ecf_user:user(), ecf_user:user()) -> boolean().
check_perm(User, Target) ->
    Groups = ecf_user:groups(Target),
    check_perm(User, Groups, none).

check_perm(_, _, deny) ->
    false;
check_perm(_User, [], allow) ->
    true;
check_perm(User, [], none) ->
    ecf_perms:check_perm_global(User, ban_user);
check_perm(User, [H|T], none) ->
    Perms = ecf_group:perms(ecf_group:get_group(H)),
    R = ecf_perms:check_perm(User, Perms, ban_user),
    check_perm(User, T, R).


-spec new_ban(ecf_user:id(), ecf_user:id(), binary(),
              erlang:timestamp(), erlang:timestamp()) -> ok.
new_ban(User, By, Reason, Time, Until) ->
    F = fun() ->
                ecf_group:add_member(3, User),
                mnesia:write(#ecf_ban{user=User,by=By,reason=Reason,
                                      time=Time,until=Until})
        end,
    mnesia:activity(transaction, F).

-spec get_bans() -> [ban()].
get_bans() ->
    F = fun() ->
                mnesia:select(ecf_ban, [{'_',[],['$_']}])
        end,
    mnesia:activity(transaction, F).

% checks a user's ban status and unbans them if their time is up
-spec check_ban(ecf_user:id()) -> ban() | undefined.
check_ban(User) ->
    F = fun() ->
                case mnesia:read({ecf_ban, User}) of
                    [] -> undefined;
                    [B] ->
                        Time = timer:now_diff(until(B), erlang:timestamp()),
                        if Time < 0 ->
                               delete_ban(User),
                               ecf_group:remove_member(3, User),
                               undefined;
                           true ->
                               B
                        end
                end
        end,
    mnesia:activity(transaction, F).

-spec delete_ban(ecf_user:id()) -> ok.
delete_ban(User) ->
    F = fun() ->
                ecf_group:remove_member(3, User),
                mnesia:delete({ecf_ban, User})
        end,
    mnesia:activity(transaction, F).


% Wrapper functions
-spec user(ban()) -> ecf_user:id().
user(Ban) ->
    Ban#ecf_ban.user.

-spec by(ban()) -> ecf_user:id().
by(Ban) ->
    Ban#ecf_ban.by.

-spec reason(ban()) -> binary().
reason(Ban) ->
    Ban#ecf_ban.reason.

-spec time(ban()) -> erlang:timestamp().
time(Ban) ->
    Ban#ecf_ban.time.

-spec until(ban()) -> erlang:timestamp().
until(Ban) ->
    Ban#ecf_ban.until.

