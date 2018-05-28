-module(ecf_perms).

-export_type([perms/0]).

-export([validate/1]).

%%% Permissions

%% permissions follow a unix-ish scheme


-type class() :: user | group | others.

-type mode() :: read | edit | delete.

-record(perms,
        {class :: class(),
         id    :: ecf_user:id() | ecf_group:id() | undefined,
         mode :: mode()}).
-type perms() :: #perms{}.

-spec validate(perms()) -> boolean().
validate(#perms{class=C,id=I,mode=M}) ->
    T1 = (C =:= user) or (C =:= group) or (C =:= others),
    T2 = (is_integer(I) andalso I >= 0) orelse (I =:= undefined),
    T3 = (M =:= read) or (M =:= edit) or (M =:= delete),
    T1 and T2 and T3.

