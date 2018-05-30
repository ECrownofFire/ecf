-module(ecf_generators).

-export([generate/3]).

-spec generate(atom() | integer, ecf_user:user() | undefined, term()) -> iodata().
generate(main, User, Forums) ->
    [generate_head("Home"),
     generate_header(User),
     generate_forum_list(Forums, User),
     generate_forum_end()];
generate(login, User, {Message, Url}) ->
    [generate_head("Login"),
     generate_header(User),
     generate_login(User, Message, Url),
     generate_forum_end()];
generate(logout, User, Url) ->
    [generate_head("Logout"),
     generate_header(User),
     generate_logout(Url),
     generate_forum_end()];
generate(register, _, Message) ->
    [generate_head("Register"),
     generate_header(undefined),
     generate_register(Message),
     generate_forum_end()];
generate(user, User, Profile) ->
    Self = User =/= undefined andalso ecf_user:id(User) =:= ecf_user:id(Profile),
    [generate_head(["Profile of ", ecf_user:name(Profile)]),
     generate_header(User),
     generate_user_profile(Self, Profile),
     generate_forum_end()];
generate(forum, User, {Forum, Threads}) ->
    [generate_head(ecf_forum:name(Forum)),
     generate_header(User),
     generate_thread_list(User, Forum, Threads),
     generate_forum_end()];
generate(thread, User, {Forum, Thread, Posts}) ->
    [generate_head(ecf_thread:title(Thread)),
     generate_header(User),
     generate_post_list(User, Forum, Thread, Posts),
     generate_forum_end()];
generate(edit_profile, User, _) ->
    [generate_head("Edit Profile"),
     generate_header(User),
     generate_edit_profile(User),
     generate_forum_end()];
generate(401, User, Type) ->
    [generate_head("401 - Unauthorized"),
     generate_header(User),
     generate_401_error(Type),
     generate_forum_end()];
generate(403, User, Type) ->
    [generate_head("403 - Forbidden"),
     generate_header(User),
     generate_403_error(Type),
     generate_forum_end()];
generate(404, User, _) ->
    [generate_head("404 - Not Found"),
     generate_header(User),
     generate_404_error(),
     generate_forum_end()];
generate(405, User, _Context) ->
    [generate_head("405 - Method Not Allowed"),
     generate_header(User),
     generate_405_error(),
     generate_forum_end()].



-spec generate_head(iodata()) -> iodata().
generate_head(Title) ->
    {ok, ForumName} = application:get_env(ecf, forum_name),
    Title2 = [ForumName, " - ", Title],
    String = read_priv_file("head.html"),
    replace(String, "title", Title2).


-spec generate_header(undefined | ecf_user:user()) -> iodata().
generate_header(undefined) ->
    String = read_priv_file("header_guest.html"),
    {ok, ForumName} = application:get_env(ecf, forum_name),
    replace(String, "forum_name", ForumName);
generate_header(User) ->
    String = read_priv_file("header_user.html"),
    Id = ecf_user:id(User),
    {ok, ForumName} = application:get_env(ecf, forum_name),
    replace_many(String, [{"forum_name", ForumName},
                          {"user_id", integer_to_list(Id)},
                          {"username", ecf_user:name(User)}]).


-spec generate_forum_list([ecf_forum:forum()], ecf_user:user()) -> iodata().
generate_forum_list(Forums, User) ->
    Begin = read_priv_file("forum_list.html"),
    Elem = read_priv_file("forum_list_element.html"),
    End = read_priv_file("forum_list_end.html"),
    Sorted = ecf_forum:visible_forums(Forums, User),
    [Begin, [generate_forum_element(X, Elem) || X <- Sorted], End].


-spec generate_forum_element(ecf_forum:forum(), iodata()) -> iodata().
generate_forum_element(Forum, String) ->
    replace_many(String, [{"id", integer_to_binary(ecf_forum:id(Forum))},
                          {"name", ecf_forum:name(Forum)},
                          {"desc", ecf_forum:desc(Forum)}]).


-spec generate_thread_list(ecf_user:user(), ecf_forum:forum(),
                           [ecf_thread:thread()]) -> iodata().
generate_thread_list(User, Forum, Threads) ->
    Begin = read_priv_file("thread_list.html"),
    Begin2 = replace_many(Begin, [{"forum", ecf_forum:name(Forum)},
                                  {"forum_desc", ecf_forum:desc(Forum)}]),
    Elem = read_priv_file("thread_list_element.html"),
    EndFile = case ecf_perms:check_perm(User, {forum, Forum}, create_thread) of
                  false -> "thread_list_end_guest.html";
                  true -> "thread_list_end.html"
              end,
    ForumId = integer_to_list(ecf_forum:id(Forum)),
    End = replace(read_priv_file(EndFile), "forum_id", ForumId),
    Sorted = ecf_thread:visible_threads(Threads, User),
    [Begin2, [generate_thread_element(X, Elem) || X <- Sorted], End].

-spec generate_thread_element(ecf_thread:thread(), iodata()) -> iodata().
generate_thread_element(Thread, String) ->
    Id = ecf_thread:id(Thread),
    LastId = ecf_thread:last(Thread),
    LastPost = ecf_post:get_post(Id, LastId),
    LastPostTime = ecf_post:time(LastPost),
    LastPosterId = ecf_post:poster(LastPost),
    LastPoster = ecf_user:get_user(LastPosterId),
    LastPosterName = ecf_user:name(LastPoster),
    CreatorId = ecf_thread:creator(Thread),
    Creator = ecf_user:get_user(CreatorId),
    Name = ecf_user:name(Creator),
    replace_many(String, [{"id", integer_to_binary(Id)},
                          {"title", ecf_thread:title(Thread)},
                          {"creator_id", integer_to_binary(CreatorId)},
                          {"creator_name", Name},
                          {"replies", integer_to_list(LastId)},
                          {"last_post_time", iso8601:format(LastPostTime)},
                          {"last_poster_id", integer_to_binary(LastPosterId)},
                          {"last_poster_name", LastPosterName}]).

generate_post_list(User, Forum, Thread, Posts) ->
    Begin = read_priv_file("post_list.html"),
    ForumId = ecf_forum:id(Forum),
    ForumName = ecf_forum:name(Forum),
    Begin2 = replace_many(Begin,
                          [{"title", ecf_thread:title(Thread)},
                           {"forum_id", integer_to_list(ForumId)},
                           {"forum_name", ForumName}]),
    Elem = read_priv_file("post_list_element.html"),
    EndFile = case ecf_perms:check_perm(User, {thread, Thread}, create_post) of
                  false -> "post_list_end_guest.html";
                  true -> "post_list_end.html"
              end,
    End = replace(read_priv_file(EndFile),
                  "thread", integer_to_list(ecf_thread:id(Thread))),
    [Begin2, [generate_post_element(X, Elem) || X <- Posts], End].

generate_post_element(Post, String) ->
    Id = ecf_post:id(Post),
    PosterId = ecf_post:poster(Post),
    Poster = ecf_user:get_user(PosterId),
    PosterName = ecf_user:name(Poster),
    PosterTitle = ecf_user:title(Poster),
    Time = iso8601:format(ecf_post:time(Post)),
    Text = ecf_post:text(Post),
    % TODO: edited by/time
    replace_many(String,
                 [{"id", integer_to_list(Id)},
                  {"user_id", integer_to_list(PosterId)},
                  {"username", PosterName},
                  {"user_title", PosterTitle},
                  {"time", Time},
                  {"text", Text}]).


generate_user_profile(Self, Profile) ->
    File = case Self of
               true -> "user_profile_self.html";
               false -> "user_profile.html" end,
    String = read_priv_file(File),
    Joined = iso8601:format(calendar:now_to_datetime(ecf_user:joined(Profile))),
    Bday = case ecf_user:bday(Profile) of
               undefined -> <<"">>;
               B -> iso8601:format(B)
           end,
    replace_many(String,
                 [{"username", ecf_user:name(Profile)},
                  {"email", ecf_user:email(Profile)},
                  {"joined", Joined},
                  {"birthday", Bday},
                  {"bio", ecf_user:bio(Profile)},
                  {"title", ecf_user:title(Profile)},
                  {"loc", ecf_user:loc(Profile)},
                  {"posts", integer_to_list(ecf_user:posts(Profile))}]).

generate_login(undefined, Type, Url) ->
    Message = application:get_env(ecf, Type, <<"Please login.">>),
    String = read_priv_file("login.html"),
    replace_many(String, [{"url", Url},
                          {"message", Message}]);
generate_login(_User, _Message, Url) ->
    Message = application:get_env(ecf, already_logged_in,
                                  <<"You're already logged in!">>),
    replace_many(read_priv_file("already_logged_in.html"),
                 [{"url", Url},
                  {"message", Message}]).

generate_logout(Url) ->
    Message = application:get_env(ecf, logout_message,
                                  <<"You've successfully been logged out.">>),
    replace_many(read_priv_file("logout.html"),
                 [{"url", Url},
                  {"message", Message}]).

generate_register(Type) ->
    Message = application:get_env(ecf, Type, <<"Please register.">>),
    {ok, Key} = application:get_env(ecf, recaptcha_key),
    String = read_priv_file("register.html"),
    replace_many(String, [{"message", Message},
                          {"recaptcha_key", Key}]).

generate_edit_profile(User) ->
    Bday = case ecf_user:bday(User) of
               {{Y, M, D}, _} ->
                   FmtStr = "~4.10.0B-~2.10.0B-~2.10.0B",
                   io_lib:format(FmtStr, [Y, M, D]);
               _ -> <<"">>
           end,
    String = read_priv_file("edit_profile.html"),
    replace_many(String, [{"username", ecf_user:name(User)},
                          {"bday", Bday},
                          {"bio", ecf_user:bio(User)},
                          {"title", ecf_user:title(User)},
                          {"loc", ecf_user:loc(User)}]).

generate_401_error(Type) ->
    Message = application:get_env(ecf, Type, <<"You need to log in first.">>),
    replace(read_priv_file("401.html"), "message", Message).

generate_403_error(Type) ->
    Message = application:get_env(ecf, Type,
                                  <<"You don't have permission to do that.">>),
    replace(read_priv_file("403.html"), "message", Message).

generate_404_error() ->
    read_priv_file("404.html").

generate_405_error() ->
    read_priv_file("405.html").

generate_forum_end() ->
    read_priv_file("forum_end.html").



%% Utility functions

-spec read_priv_file(string()) -> binary().
read_priv_file(Filename) ->
    {ok, String} = file:read_file(filename:join(code:priv_dir(ecf), Filename)),
    String.

-spec replace_many(iodata(), [{iodata(),iodata()}]) -> iodata().
replace_many(String, List) ->
    R = fun({Search, Rep}, Str) ->
                replace(Str, Search, Rep)
        end,
    lists:foldl(R, String, List).

-spec replace(iodata(), iodata(), iodata()) -> iodata().
replace(String, Search, Replace) ->
    string:replace(String, ["{{",Search,"}}"], Replace, all).

