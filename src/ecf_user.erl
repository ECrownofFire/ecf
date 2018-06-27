-module(ecf_user).

-include_lib("stdlib/include/qlc.hrl").

-export_type([id/0, user/0]).

-define(HMAC, sha512).
-define(HASH_ITERATIONS, 200000).
-define(HASH_LENGTH, 64).
-define(SALT_LENGTH, 32).

-define(SESSION_LENGTH, 32).

%%% Wrapper for user type

-type id() :: non_neg_integer().

-export([create_table/1,
         new_user/5,
         get_user/1, get_user_by_name/1, get_user_by_email/1,
         edit_name/2, edit_email/2, edit_pass/2, new_session/1, reset_session/2,
         enable_user/1, disable_user/1,
         add_group/2, remove_group/2,
         edit_bio/2, edit_bday/2,
         edit_title/2, edit_loc/2,
         add_post/2,
         delete_user/1,
         check_session/2, check_pass/2, fake_hash/0,
         confirm_email/1, ban_user/1, unban_user/1,
         id/1, name/1, enabled/1, email/1, email_confirmed/1, banned/1,
         joined/1, groups/1, bday/1, title/1, bio/1, loc/1, posts/1,
         last_post/1]).

-record(ecf_user,
        {id     :: id(),
         name   :: binary(),
         enabled:: boolean(),
         session:: [{binary(), erlang:timestamp()}],
         salt   :: binary(),
         pass   :: binary(),
         email  :: binary(),
         joined :: erlang:timestamp(),
         groups = [] :: [ecf_group:id()],
         bday   :: calendar:datetime() | undefined,
         title  = <<"">> :: binary(),
         bio    = <<"">> :: binary(),
         loc    = <<"">> :: binary(),
         posts = 0 :: non_neg_integer(),
         last_post :: erlang:timestamp()}).
-type user() :: #ecf_user{}.

-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_user,
                        [{attributes, record_info(fields, ecf_user)},
                         {disc_copies, Nodes}]),
    ok.

-spec new_user(binary(), binary(), binary(),
               erlang:timestamp(), calendar:datetime()) ->
    {id(), binary()} | {error, atom()}.
new_user(Name, Pass, Email0, Time, Bday) ->
    Email = string:trim(Email0),
    Salt = crypto:strong_rand_bytes(?SALT_LENGTH),
    {ok, Hash} = pbkdf2:pbkdf2(?HMAC, Pass, Salt,
                               ?HASH_ITERATIONS, ?HASH_LENGTH),
    Session = crypto:strong_rand_bytes(?SESSION_LENGTH),
    Sess = {Session, Time},
    F = fun() ->
                case get_user_by_name(Name) of
                    undefined ->
                        case get_user_by_email(Email) of
                            undefined ->
                                Id = ecf_db:get_new_id(ecf_user),
                                ok = mnesia:write(
                                       #ecf_user{
                                          id=Id, name=Name, enabled=true,
                                          salt=Salt, pass=Hash, session=[Sess],
                                          email=Email, joined=Time, bday=Bday,
                                          last_post=Time}),
                                % All users are in the basic group
                                ok = ecf_group:add_member(1, Id),
                                {Id, Session};
                            _ ->
                                {error, email_taken}
                        end;
                    _ ->
                        {error, username_taken}
                end
        end,
    case mnesia:activity(transaction, F) of
        {error, _} = Err ->
            Err;
        {Id, _} = Res ->
            ok = ecf_email:send_confirmation_email(get_user(Id)),
            Res
    end.

-spec get_user(id()) -> user() | undefined.
get_user(Id) ->
    F = fun() ->
                mnesia:read({ecf_user, Id})
        end,
    case mnesia:activity(transaction, F) of
        [User] ->
            User;
        _ ->
            undefined
    end.

% Case insensitive search
-spec get_user_by_name(binary()) -> user() | undefined.
get_user_by_name(Name) ->
    F = fun() ->
            qlc:e(qlc:q(
                [X || X = #ecf_user{name=N} <- mnesia:table(ecf_user),
                      string:equal(Name, N, true)]))
        end,
    case mnesia:activity(transaction, F) of
        [User] ->
            User;
        _ ->
            undefined
    end.

% Case insensitive search
-spec get_user_by_email(binary()) -> user() | undefined.
get_user_by_email(Email) ->
    F = fun() ->
            qlc:e(qlc:q(
                [X || X = #ecf_user{email=E} <- mnesia:table(ecf_user),
                      string:equal(Email, E, true)]))
        end,
    case mnesia:activity(transaction, F) of
        [User] ->
            User;
        _ ->
            undefined
    end.


-spec edit_name(id(), binary()) -> binary() | {error, username_taken}.
edit_name(Id, Name) ->
    Session = crypto:strong_rand_bytes(?SESSION_LENGTH),
    Sess = {Session, erlang:timestamp()},
    F = fun() ->
                case get_user_by_name(Name) of
                    undefined ->
                        [User] = mnesia:wread({ecf_user, Id}),
                        ok = mnesia:write(User#ecf_user{name=Name,session=[Sess]}),
                        Session;
                    _ ->
                        {error, username_taken}
                end
        end,
    mnesia:activity(transaction, F).

-spec edit_email(id(), binary()) -> binary() | {error, email_taken}.
edit_email(Id, Email) ->
    Session = crypto:strong_rand_bytes(?SESSION_LENGTH),
    Sess = {Session, erlang:timestamp()},
    F = fun() ->
                case get_user_by_email(Email) of
                    undefined ->
                        [User] = mnesia:wread({ecf_user, Id}),
                        ok = mnesia:write(User#ecf_user{email=Email,session=[Sess]}),
                        ok = ecf_group:remove_member(2, Id),
                        Session;
                    _ ->
                        {error, email_taken}
                end
        end,
    case mnesia:activity(transaction, F) of
        {error, _} = Err ->
            Err;
        Res ->
            ok = ecf_email:send_confirmation_email(get_user(Id)),
            Res
    end.

-spec edit_pass(id(), binary()) -> binary().
edit_pass(Id, NewPass) ->
    Salt = crypto:strong_rand_bytes(?SALT_LENGTH),
    {ok, Hash} = pbkdf2:pbkdf2(?HMAC, NewPass, Salt,
                               ?HASH_ITERATIONS, ?HASH_LENGTH),
    Session = crypto:strong_rand_bytes(?SESSION_LENGTH),
    Sess = {Session, erlang:timestamp()},
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                ok = mnesia:write(User#ecf_user{pass=Hash,salt=Salt,session=[Sess]}),
                Session
        end,
    mnesia:activity(transaction, F).


-spec new_session(id()) -> binary().
new_session(Id) ->
    Session = crypto:strong_rand_bytes(?SESSION_LENGTH),
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                New = [{Session, erlang:timestamp()}|User#ecf_user.session],
                mnesia:write(User#ecf_user{session=New})
        end,
    ok = mnesia:activity(transaction, F),
    Session.

-spec reset_session(id(), binary()) -> ok.
reset_session(Id, Session) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                New = lists:keydelete(Session, 1, User#ecf_user.session),
                mnesia:write(User#ecf_user{session=New})
        end,
    mnesia:activity(transaction, F).

-spec clean_sessions(id()) -> ok.
clean_sessions(Id) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                Old = User#ecf_user.session,
                New = lists:filter(fun clean_sess/1, Old),
                mnesia:write(User#ecf_user{session=New})
        end,
    mnesia:activity(transaction, F).

clean_sess({_, Time}) ->
    Limit = application:get_env(ecf, session_time, 604800) * 1000000,
    Diff = timer:now_diff(erlang:timestamp(), Time),
    Diff < Limit.


-spec enable_user(id()) -> ok.
enable_user(Id) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                mnesia:write(User#ecf_user{enabled=true})
        end,
    mnesia:activity(transaction, F).

-spec disable_user(id()) -> ok.
disable_user(Id) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                mnesia:write(User#ecf_user{enabled=false})
        end,
    mnesia:activity(transaction, F).


-spec add_group(id(), ecf_group:id()) -> ok.
add_group(Id, Group) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                NewList = [Group|groups(User)],
                mnesia:write(User#ecf_user{groups=NewList})
        end,
    mnesia:activity(transaction, F).

-spec remove_group(id(), ecf_group:id()) -> ok.
remove_group(Id, Group) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                NewList = lists:delete(Group, groups(User)),
                mnesia:write(User#ecf_user{groups=NewList})
        end,
    mnesia:activity(transaction, F).

-spec edit_bio(id(), binary()) -> ok.
edit_bio(Id, Bio) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                mnesia:write(User#ecf_user{bio=Bio})
        end,
    mnesia:activity(transaction, F).

-spec edit_bday(id(), calendar:datetime() | undefined) -> ok.
edit_bday(Id, Bday) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                mnesia:write(User#ecf_user{bday=Bday})
        end,
    mnesia:activity(transaction, F).

-spec edit_title(id(), binary()) -> ok.
edit_title(Id, Title) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                mnesia:write(User#ecf_user{title=Title})
        end,
    mnesia:activity(transaction, F).

-spec edit_loc(id(), binary()) -> ok.
edit_loc(Id, Loc) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                mnesia:write(User#ecf_user{loc=Loc})
        end,
    mnesia:activity(transaction, F).

-spec add_post(id(), erlang:timestamp()) -> ok.
add_post(Id, Time) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                New = User#ecf_user.posts + 1,
                mnesia:write(User#ecf_user{posts=New, last_post=Time})
        end,
    mnesia:activity(transaction, F).


-spec delete_user(id()) -> ok.
delete_user(Id) ->
    F = fun() ->
                [User] = mnesia:wread({ecf_user, Id}),
                [ecf_group:remove_member(Group, Id) || Group <- groups(User)],
                mnesia:delete({ecf_user, Id})
        end,
    mnesia:activity(transaction, F).


-spec id(user()) -> id().
id(User) ->
    User#ecf_user.id.

-spec name(user()) -> binary().
name(User) ->
    User#ecf_user.name.

-spec enabled(user()) -> boolean().
enabled(User) ->
    User#ecf_user.enabled.

-spec check_session(user(), binary()) -> boolean().
check_session(User, Session) ->
    ok = clean_sessions(id(User)),
    case lists:keyfind(Session, 1, User#ecf_user.session) of
        {_, _} ->
            enabled(User);
        false ->
            false
    end.

-spec check_pass(user(), binary()) -> boolean().
check_pass(User, Pass) ->
    Salt = User#ecf_user.salt,
    PHash = User#ecf_user.pass,
    {ok, Hash} = pbkdf2:pbkdf2(?HMAC, Pass, Salt, ?HASH_ITERATIONS, ?HASH_LENGTH),
    pbkdf2:compare_secure(PHash, Hash).

% Fakes hashing for invalid logins
-spec fake_hash() -> ok.
fake_hash() ->
    Pass = <<"password">>,
    Salt = <<"saltSALTsaltSALT">>,
    {ok, Hash} = pbkdf2:pbkdf2(?HMAC, Pass, Salt, ?HASH_ITERATIONS, ?HASH_LENGTH),
    _ = pbkdf2:compare_secure(Hash, Hash),
    ok.

-spec confirm_email(id()) -> ok.
confirm_email(Id) ->
    ecf_group:add_member(2, Id).

-spec ban_user(id()) -> ok.
ban_user(Id) ->
    ecf_group:add_member(3, Id).

-spec unban_user(id()) -> ok.
unban_user(Id) ->
    ecf_group:remove_member(3, Id).

-spec email(user()) -> binary().
email(User) ->
    User#ecf_user.email.

-spec email_confirmed(user()) -> boolean().
email_confirmed(User) ->
    lists:member(2, groups(User)).

-spec banned(user()) -> boolean().
banned(User) ->
    lists:member(3, groups(User)).

-spec joined(user()) -> erlang:timestamp().
joined(User) ->
    User#ecf_user.joined.

-spec groups(user()) -> [ecf_group:id()].
groups(User) ->
    User#ecf_user.groups.

-spec bday(user()) -> calendar:datetime() | undefined.
bday(User) ->
    User#ecf_user.bday.

-spec title(user()) -> binary().
title(User) ->
    User#ecf_user.title.

-spec bio(user()) -> binary().
bio(User) ->
    User#ecf_user.bio.

-spec loc(user()) -> binary().
loc(User) ->
    User#ecf_user.loc.

-spec posts(user()) -> non_neg_integer().
posts(User) ->
    User#ecf_user.posts.

-spec last_post(user()) -> erlang:timestamp().
last_post(User) ->
    User#ecf_user.last_post.

