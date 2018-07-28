-module(ecf_log).

-export_type([log/0]).

-export([create_table/1,
         log/2,
         clear_log/2,
         check_log/2]).

-record(ecf_log,
        {user_or_ip  :: binary() | inet:ip_address(),
         fails :: non_neg_integer()
         }).
-type log() :: #ecf_log{}.

create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_log,
                        [{attributes, record_info(fields, ecf_log)},
                         {ram_copies, Nodes}]),
    ok.

log(User, Ip) ->
    mnesia:dirty_update_counter(ecf_log, User, 1),
    mnesia:dirty_update_counter(ecf_log, Ip, 1).

clear_log(User, Ip) ->
    F = fun() ->
                mnesia:delete({ecf_log, User}),
                mnesia:delete({ecf_log, Ip})
        end,
    mnesia:activity(transaction, F).

-spec check_log(binary(), inet:ip_address()) -> boolean().
check_log(User, Ip) ->
    F = fun() ->
                max(count(mnesia:read(ecf_log, User)),
                    count(mnesia:read(ecf_log, Ip)))
        end,
    Count = mnesia:activity(transaction, F),
    Max = application:get_env(ecf, max_login_attempts, 5),
    Count >= Max.

count(C) ->
    case C of
        [] -> 0;
        [R] -> R#ecf_log.fails
    end.

