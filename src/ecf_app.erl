-module(ecf_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(TYPES, [<<"user">>]).

-define(P_TYPES, [<<"global">>, <<"forum">>, <<"thread">>, <<"group">>]).

-define(G_ACTIONS, [<<"create">>, <<"delete">>, <<"edit">>,
                    <<"join">>, <<"leave">>,
                    <<"add">>, <<"remove">>]).

-define(U_ACTIONS, [<<"edit">>]).

start(_Type, _Args) ->
    GActions = make_fun(?G_ACTIONS),
    UActions = make_fun(?U_ACTIONS),
    TypeFun = make_fun(?TYPES),
    PTypes = make_fun(?P_TYPES),
    GroupActions = [{action, GActions}],
    UConstraints = [{id, int}, {action, UActions}],
    HConstraints = [{type, TypeFun}, {id, int}],
    PConstraints = [{type, PTypes}, {id, int}],
    IdC = [{id, int}],
    TConstraints = [{id, int}, {post, int}],
    Host = application:get_env(ecf, host, '_'),
    Base = application:get_env(ecf, base_url, ""),
    Port = application:get_env(ecf, port, 8080),
    Dispatch = cowboy_router:compile([
        {Host, [{[Base, "/favicon.ico"], cowboy_static,
                 {priv_file, ecf, "favicon.ico"}},
               {[Base, "/static/[...]"], cowboy_static,
                {priv_dir, ecf, "static"}},
               {[Base, "/login"], ecf_login_handler, {}},
               {[Base, "/register"], ecf_register_handler, {}},
               {[Base, "/edit_profile"], ecf_edit_profile_handler, {}},
               {[Base, "/logout"], ecf_logout_handler, {}},
               {[Base, "/group[/:id]"], IdC, ecf_group_handler, {}},
               {[Base, "/group[/:action]"], GroupActions, ecf_group_handler, {}},
               {[Base, "/user[/:id][/:action]"], UConstraints, ecf_user_handler, {}},
               {[Base, "/:type/[:id/]perms"], PConstraints, ecf_perms_handler, {}},
               {[Base, "/[:type/:id]"], HConstraints, ecf_handler, {}},
               {[Base, "/forum/[:id]"], IdC, ecf_forum_handler, {}},
               {[Base, "/thread/[:id[/:post]]"], TConstraints, ecf_thread_handler, {}},
               {[Base, "/post"], ecf_post_handler, {}},
               {[Base, "/[...]"], ecf_404_handler, {}}]}
    ]),
    {ok, _Pid} = cowboy:start_clear(ecf_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch},
          stream_handlers => [ecf_stream, cowboy_stream_h]
         }
    ),
    ecf_sup:start_link().

stop(_State) ->
    ok.

make_fun(List) ->
    fun(_, Name) ->
            case lists:member(Name, List) of
                true -> {ok, Name};
                false -> {error, invalid_type}
            end
    end.

