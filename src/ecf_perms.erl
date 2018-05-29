-module(ecf_perms).

-export_type([perms/0, mode/0]).

-export([create_perm/2]).

%%% Permissions

%% permissions follow a unix-ish scheme

-type mode() :: read | edit | delete.

-record(perms,
        {type  :: {user, ecf_user:id()} | {group, ecf_group:id()} | others,
         modes  :: [mode()]}).
-type perms() :: #perms{}.

create_perm(others, Modes) ->
    #perms{type=others, modes=Modes};
create_perm({Type, Id}, Modes) when Type =:= user; Type =:= group ->
    #perms{type = {Type, Id}, modes=Modes}.

