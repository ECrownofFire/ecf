-module(ecf_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(TYPES, [<<"user">>, <<"group">>, <<"thread">>]).

-define(P_TYPES, [<<"global">>, <<"forum">>, <<"thread">>, <<"group">>]).

start(_Type, _Args) ->
    TypeFun = fun (_, Name) ->
                      case lists:member(Name, ?TYPES) of
                          true -> {ok, Name};
                          false -> {error, invalid_type}
                      end
              end,
    PTypes = fun (_, Name) ->
                     case lists:member(Name, ?P_TYPES) of
                         true -> {ok, Name};
                         false -> {error, invalid_type}
                     end
            end,
    HConstraints = [{type, TypeFun}, {id, int}],
    PConstraints = [{type, PTypes}, {id, int, -1}],
    IdConstraint = [{id, int, -1}],
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
               {[Base, "/groups"], ecf_groups_handler, {}},
               {[Base, "/:type/[:id/]perms"], PConstraints, ecf_perms_handler, {}},
               {[Base, "/[:type/:id]"], HConstraints, ecf_handler, {}},
               {[Base, "/forum/[:id]"], IdConstraint, ecf_forum_handler, {}},
               {[Base, "/post"], ecf_post_handler, {}},
               {[Base, "/thread"], ecf_thread_handler, {}},
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

