-module(ecf_db).

%%% Contains helper functions for connecting to the database

% Record definitions to be used in the database, private to the database
% modules and only accessed through the helper functions.

-record(ecf_id,
        {type :: atom(),
         id   :: non_neg_integer()}).

-export([install/0,
         get_new_id/1]).

-spec get_new_id(atom()) -> non_neg_integer().
get_new_id(Type) ->
    mnesia:dirty_update_counter(ecf_id, Type, 1).

install() ->
    Nodes = [node()],
    ok = application:stop(mnesia),
    ok = mnesia:create_schema(Nodes),
    ok = application:start(mnesia),
    ok = create_id_table(Nodes),
    ecf_forum:create_table(Nodes),
    ecf_thread:create_table(Nodes),
    ecf_post:create_table(Nodes),
    ecf_user:create_table(Nodes),
    ecf_group:create_table(Nodes),
    ecf_log:create_table(Nodes),
    ecf_perms:create_table(Nodes).

-spec create_id_table([node()]) -> ok.
create_id_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_id,
                        [{attributes, record_info(fields, ecf_id)},
                         {disc_copies, Nodes}]),
    ok.

