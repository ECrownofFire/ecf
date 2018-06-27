-module(ecf_email).

-export([create_table/1, get_code/1, clear_code/1, send_confirmation_email/1]).


-record(ecf_email,
        {user :: ecf_user:id(),
         type :: confirm, % TODO: password_reset
         code :: binary()}).

create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_email,
                       [{attributes, record_info(fields, ecf_email)},
                        {disc_copies, Nodes}]),
    ok.


-spec get_code(ecf_user:id()) -> undefined | {confirm, binary()}.
get_code(Id) ->
    F = fun() ->
                case mnesia:read({ecf_email, Id}) of
                    [C] ->
                        {C#ecf_email.type, C#ecf_email.code};
                    _ ->
                        undefined
                end
        end,
    mnesia:activity(transaction, F).

-spec clear_code(ecf_user:id()) -> ok.
clear_code(Id) ->
    F = fun() ->
                mnesia:delete({ecf_email, Id})
        end,
    mnesia:activity(transaction, F).


-spec send_confirmation_email(ecf_user:user()) -> ok.
send_confirmation_email(User) ->
    Username = ecf_user:name(User),
    Email0 = ecf_user:email(User),
    Email = <<"<",Email0/binary,">">>,
    {ok, Host} = application:get_env(ecf, host),
    Base = application:get_env(ecf, base_url, <<"">>),
    {ok, ForumName} = application:get_env(ecf, forum_name),
    {ok, Addr0} = application:get_env(ecf, email_addr),
    Addr = <<"<",Addr0/binary,">">>,
    {ok, Relay} = application:get_env(ecf, email_relay),
    {ok, EHost} = application:get_env(ecf, email_host),
    Code = make_code(),
    Body = <<"https://",Host/binary,Base/binary,"/confirm?code=",Code/binary>>,
    Mail = mimemail:encode({<<"text">>, <<"plain">>,
                            [{<<"Subject">>,
                              <<"Confirm your email for ",ForumName/binary>>},
                             {<<"From">>, <<ForumName/binary," ",Addr/binary>>},
                             {<<"To">>, <<Username/binary," ",Email/binary>>}],
                            [],
                            iolist_to_binary(Body)}),
    {ok, _} = gen_smtp_client:send({Addr, [Email], Mail},
                                   [{relay, Relay}, {hostname, EHost}]),
    F = fun() ->
                mnesia:write(#ecf_email{user=ecf_user:id(User),
                                        type=confirm,
                                        code=Code})
        end,
    mnesia:activity(transaction, F).

make_code() ->
    Possible = "1234567890" ++ lists:seq($A, $Z) ++ lists:seq($a, $z),
    crypto:rand_seed(),
    list_to_binary([lists:nth(rand:uniform(length(Possible)), Possible)
                    || _ <- lists:seq(1,64)]).

