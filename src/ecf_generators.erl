-module(ecf_generators).

-define(GRAVATAR_URL, <<"https://gravatar.com/avatar/">>).
-define(GRAVATAR_OPTIONS, <<"?s=100&d=identicon">>).

-export([generate/3]).

-spec generate(atom() | integer(), ecf_user:user() | undefined, term()) -> iodata().
generate(main, User, Forums) ->
    Vars = get_vars(User),
    ForumList = forum_list(Forums),
    CanCreate = ecf_perms:check_perm_global(User, create_forum),
    Vars2 = [{forum_list, ForumList}, {create_forum, CanCreate}|Vars],
    {ok, Res} = ecf_main_dtl:render(Vars2),
    Res;
generate(admin, User, _) ->
    % TODO: ecf_admin.dtl
    Vars = get_vars(User, "Admin"),
    {ok, Res} = ecf_admin_dtl:render(Vars),
    Res;
generate(login, User, {Captcha, Url, Type}) ->
    Vars = get_vars(User, "Login"),
    Message = login_message(User, Type),
    {ok, Key} = application:get_env(ecf, recaptcha_key),
    Vars2 = [{captcha, Captcha}, {message, Message},
             {recaptcha_key, Key}, {url, Url}
             | Vars],
    {ok, Res} = ecf_login_dtl:render(Vars2),
    Res;
generate(logout, User, Url) ->
    Vars = get_vars(User, "Logout"),
    Message = application:get_env(ecf, logout_message,
                                  <<"You've successfully been logged out.">>),
    Vars2 = [{message, Message}, {url, Url}|Vars],
    {ok, Res} = ecf_logout_dtl:render(Vars2),
    Res;
generate(register, _, {Type, BaseVars}) ->
    Vars = [get_vars(undefined, "Register") | BaseVars],
    Message = application:get_env(ecf, Type, <<"Please register.">>),
    {ok, Key} = application:get_env(ecf, recaptcha_key),
    Vars2 = [{message, Message}, {recaptcha_key, Key} | Vars],
    {ok, Res} = ecf_register_dtl:render(Vars2),
    Res;
generate(user, User, Profile) ->
    Vars = get_vars(User, ["Profile of ", ecf_user:name(Profile)]),
    Self = User =/= undefined andalso ecf_user:id(User) =:= ecf_user:id(Profile),
    Vars2 = [{self, Self}|Vars],
    {ok, Res} = ecf_user_dtl:render(Vars2),
    Res;
generate(groups, User, Groups) ->
    Vars = get_vars(User, "Groups"),
    GroupList = group_list(User, Groups),
    Vars2 = [{group_list, GroupList}|Vars],
    {ok, Res} = ecf_groups_dtl:render(Vars2),
    Res;
generate(group, User, Group) ->
    Vars = get_vars(User, ["Group: ", ecf_group:name(Group)]),
    MemberList = users(ecf_group:members(Group)),
    GroupV = [{member_list, MemberList}|group(Group)],
    Vars2 = [{group, GroupV}|Vars],
    {ok, Res} = ecf_group_dtl:render(Vars2),
    Res;
generate(forum, User, {Forum, Threads}) ->
    Vars = get_vars(User, ecf_forum:name(Forum)),
    ForumV = forum(Forum),
    ThreadList = thread_list(Threads),
    CanEdit = ecf_perms:check_perm_forum(User, Forum, edit_forum),
    CanDelete = ecf_perms:check_perm_forum(User, Forum, delete_forum),
    CanCreate = ecf_perms:check_perm_forum(User, Forum, create_thread),
    Vars2 = [{forum, ForumV},
             {thread_list, ThreadList},
             {can_edit, CanEdit},
             {can_delete, CanDelete},
             {can_create_thread, CanCreate}
             | Vars],
    {ok, Res} = ecf_forum_dtl:render(Vars2),
    Res;
generate(thread, User, {Forum, Thread, Posts}) ->
    Vars = get_vars(User, ecf_thread:title(Thread)),
    ForumV = forum(Forum),
    ThreadV = thread(Thread),
    PostList = post_list(Posts),
    CreatePost = ecf_perms:check_perm_thread(User, Thread, create_post),
    DeletePosts = ecf_perms:check_perm_thread(User, Thread, delete_post),
    EditThread = ecf_perms:check_perm_thread(User, Thread, edit_thread),
    DeleteThread = ecf_perms:check_perm_thread(User, Thread, delete_thread),
    Vars2 = [{forum, ForumV},
             {thread, ThreadV},
             {post_list, PostList},
             {can_create_post, CreatePost},
             {can_delete_posts, DeletePosts},
             {can_edit_thread, EditThread},
             {can_delete_thread, DeleteThread}
             | Vars],
    {ok, Res} = ecf_thread_dtl:render(Vars2),
    Res;
generate(edit_profile, User, _) ->
    Vars = get_vars(User, "Edit Profile"),
    UserV = user(User),
    Vars2 = [{user, UserV}|Vars],
    {ok, Res} = ecf_edit_profile_dtl:render(Vars2),
    Res;
generate(Status, User, Type)
  when is_integer(Status), Status >= 400, Status =< 499 ->
    Desc = status_desc(Status),
    Vars = get_vars(User, [integer_to_list(Status), " - ", Desc]),
    Message = case Type of false -> false; _ -> application:get_env(ecf, Type, Desc) end,
    Vars2 = [{code, integer_to_list(Status)},
             {desc, Desc},
             {message, Message}
             | Vars],
    {ok, Res} = ecf_error_dtl:render(Vars2),
    Res.


get_vars(User) ->
    [{base, get_base()},
     {title, get_title()},
     {user, user(User)}].

get_vars(User, Title) ->
    [{base, get_base()},
     {title, get_title(Title)},
     {user, user(User)}].

get_base() ->
    {ok, Base} = application:get_env(ecf, base_url),
    Base.

-spec get_title() -> iodata().
get_title() ->
    {ok, ForumName} = application:get_env(ecf, forum_name),
    ForumName.

-spec get_title(iodata()) -> iodata().
get_title(String) ->
    [get_title(), " - ", String].

users(Users) ->
    [user(ecf_user:get_user(X)) || X <- Users].

user(undefined) ->
    false;
user(User) ->
    Bday = case ecf_user:bday(User) of
               undefined -> false;
               B -> iso8601:format(B)
           end,
    [{id, ecf_user:id(User)},
     {name, ecf_user:name(User)},
     {email, ecf_user:email(User)},
     {joined, iso8601:format(ecf_user:joined(User))},
     {birthday, Bday},
     {bio, ecf_user:bio(User)},
     {title, ecf_user:title(User)},
     {loc, ecf_user:loc(User)},
     {posts, integer_to_list(ecf_user:posts(User))}].


login_message(undefined, Type) ->
    application:get_env(ecf, Type, <<"Please login.">>);
login_message(_User, _) ->
    application:get_env(ecf, already_logged_in,
                        <<"You're already logged in!">>).


forum_list(Forums) ->
    [forum(X) || X <- Forums].

forum(Forum) ->
    [{id, integer_to_list(ecf_forum:id(Forum))},
     {name, ecf_forum:name(Forum)},
     {desc, ecf_forum:desc(Forum)}].

group_list(User, Groups) ->
    [group(User, X) || X <- Groups].

group(User, Group) ->
    Id = ecf_group:id(Group),
    In = lists:member(Id, ecf_user:groups(User)),
    CanJoin = Id > 1
              andalso not In
              andalso ecf_perms:check_perm_group(User, Group, join_group),
    CanLeave = Id > 1
               andalso In
               andalso ecf_perms:check_perm_group(User, Group, leave_group),
     [{can_leave, CanLeave},
      {can_join, CanJoin}
      | group(Group)].

group(Group) ->
    [{id, integer_to_list(ecf_group:id(Group))},
     {name, ecf_group:name(Group)},
     {desc, ecf_group:desc(Group)},
     {members, integer_to_list(length(ecf_group:members(Group)))}].


thread_list(Threads) ->
    [thread(X) || X <- Threads].

thread(Thread) ->
    Id = ecf_thread:id(Thread),
    LastId = ecf_thread:last(Thread),
    LastPost = ecf_post:get_post(Id, LastId),
    LastPostTime = ecf_post:time(LastPost),
    LastPosterId = ecf_post:poster(LastPost),
    LastPoster = ecf_user:get_user(LastPosterId),
    LastPosterName = ecf_user:name(LastPoster),
    CreatorId = ecf_thread:creator(Thread),
    Creator = ecf_user:get_user(CreatorId),
    CreatorName = ecf_user:name(Creator),
    [{id, integer_to_list(Id)},
     {title, ecf_thread:title(Thread)},
     {replies, integer_to_list(LastId)},
     {creator_id, integer_to_list(CreatorId)},
     {creator_name, CreatorName},
     {last_time, iso8601:format(LastPostTime)},
     {last_poster_id, integer_to_list(LastPosterId)},
     {last_poster_name, LastPosterName}].

post_list(Posts) ->
    [post(X) || X <- Posts].

post(Post) ->
    Id = ecf_post:id(Post),
    PosterId = ecf_post:poster(Post),
    Poster = ecf_user:get_user(PosterId),
    Gravatar = gravatar(Poster),
    Time = iso8601:format(ecf_post:time(Post)),
    Text = ecf_post:text(Post),
    [{id, integer_to_list(Id)},
     {gravatar, Gravatar},
     {poster, user(Poster)},
     {time, Time},
     {text, Text}].

status_desc(400) ->
    "Bad Request";
status_desc(401) ->
    "Unauthorized";
status_desc(403) ->
    "Forbidden";
status_desc(404) ->
    "Not Found";
status_desc(405) ->
    "Bad Method";
status_desc(429) ->
    "Too Many Requests".


-spec gravatar(ecf_user:user()) -> iodata().
gravatar(User) ->
    Email = string:lowercase(ecf_user:email(User)),
    Hash = erlang:md5(Email),
    Hex = [[io_lib:format("~2.16.0b", [X]) || <<X:8>> <= Hash]],
    [?GRAVATAR_URL, Hex, ?GRAVATAR_OPTIONS].

