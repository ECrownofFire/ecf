-module(ecf_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(G_ACTIONS, [<<"create">>, <<"delete">>, <<"edit">>,
                    <<"join">>, <<"leave">>,
                    <<"add">>, <<"remove">>]).

-define(U_ACTIONS, [<<"edit">>]).

-define(T_ACTIONS, [<<"create">>, <<"delete">>, <<"edit">>,
                    <<"pin">>, <<"unpin">>]).

-define(PO_ACTIONS, [<<"create">>, <<"delete">>, <<"edit">>]).

-define(F_ACTIONS, [<<"create">>, <<"delete">>, <<"edit">>, <<"reorder">>]).

-define(PE_ACTIONS, [<<"add">>, <<"remove">>]).

-define(MSG_ACTIONS, [<<"create">>, <<"add">>, <<"remove">>]).

-define(BAN_ACTIONS, [<<"ban">>, <<"unban">>]).

start(_Type, _Args) ->
    IdC = [{id, int}],
    TPCon = [{id, int}, {post, int}],
    GCon = make_con(?G_ACTIONS),
    UCon = make_con(?U_ACTIONS),
    TCon = make_con(?T_ACTIONS),
    PoCon = make_con(?PO_ACTIONS),
    FCon = make_con(?F_ACTIONS),
    PeCon = make_con(?PE_ACTIONS),
    MsgCon = make_con(?MSG_ACTIONS),
    BanCon = make_con(?BAN_ACTIONS),
    {ok, Host} = application:get_env(ecf, host),
    Base = application:get_env(ecf, base_url, ""),
    Dispatch = cowboy_router:compile([
        {Host, [{[Base, "/favicon.ico"], cowboy_static,
                 {priv_file, ecf, "favicon.ico"}},
               {[Base, "/static/[...]"], cowboy_static,
                {priv_dir, ecf, "static"}},
               {[Base, "/admin"], ecf_admin_handler, {}},
               {[Base, "/login"], ecf_login_handler, {}},
               {[Base, "/forgot_password"], ecf_forgot_pw_handler, {}},
               {[Base, "/reset_password"], ecf_reset_pw_handler, {}},
               {[Base, "/change_password"], ecf_change_pw_handler, {}},
               {[Base, "/register"], ecf_register_handler, {}},
               {[Base, "/edit_profile"], ecf_edit_profile_handler, {}},
               {[Base, "/logout"], ecf_logout_handler, {}},
               {[Base, "/confirm"], ecf_confirm_handler, {}},
               {[Base, "/group[/:id]"], IdC, ecf_group_handler, {}},
               {[Base, "/group/:action"], GCon, ecf_group_handler, {}},
               {[Base, "/user[/:id]"], IdC, ecf_user_handler, {}},
               {[Base, "/user/:action"], UCon, ecf_user_handler, {}},
               {[Base, "/thread/:id[/post/:post]"], TPCon, ecf_thread_handler, {}},
               {[Base, "/thread/:action"], TCon, ecf_thread_handler, {}},
               {[Base, "/post/:action"], PoCon, ecf_post_handler, {}},
               {[Base, "/forum/:id"], IdC, ecf_forum_handler, {}},
               {[Base, "/forum/:action"], FCon, ecf_forum_handler, {}},
               {[Base, "/perms/:action"], PeCon, ecf_perms_handler, {}},
               {[Base, "/msg/:action"], MsgCon, ecf_msg_handler, {}},
               {[Base, "/:action"], BanCon, ecf_ban_handler, {}},
               {[Base, "/bans"], ecf_bans_handler, {}},
               {[Base, "/"], ecf_handler, {}},
               {[Base, "/[...]"], ecf_404_handler, {}}]}
    ]),
    {ok, Http} = application:get_env(ecf, http),
    {ok, Https} = application:get_env(ecf, https),
    Middlewares = [ecf_csrf, ecf_refresh, cowboy_router, cowboy_handler],
    start_clear(Http, Dispatch, Middlewares),
    start_tls(Https, Dispatch, Middlewares),
    ecf_sup:start_link().

stop(_State) ->
    ok.


start_clear(false, _, _) ->
    ok;
start_clear(Port, Dispatch, Middlewares) ->
    {ok, _Pid} = cowboy:start_clear(ecf_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch},
          middlewares => Middlewares}),
    ok.

start_tls(false, _, _) ->
    ok;
start_tls({Port, CertFile, KeyFile}, Dispatch, Middlewares) ->
    {ok, _Pid} = cowboy:start_tls(ecf_https_listener,
        [{port, Port}, {certfile, CertFile}, {keyfile, KeyFile}],
        #{env => #{dispatch => Dispatch},
          middlewares => Middlewares}),
    ok.

make_con(List) ->
    [{action, make_fun(List)}].

make_fun(List) ->
    fun(forward, Name) ->
            case lists:member(Name, List) of
                true -> {ok, Name};
                false -> {error, invalid_type}
            end;
       (reverse, Name) ->
            {ok, Name}
    end.

