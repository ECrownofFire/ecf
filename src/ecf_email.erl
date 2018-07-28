-module(ecf_email).

-export([create_table/1,
         get_confirm_code/1, get_password_reset_code/1,
         clear_confirm_code/1, clear_password_reset_code/1,
         send_confirmation_email/1, send_password_reset_email/1]).


-define(SECS_PER_DAY, 86400).

-type type() :: confirm | reset_password.

-record(ecf_email,
        {key :: {ecf_user:id(), type()},
         code :: binary(),
         time :: integer()}).


-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_email,
                       [{attributes, record_info(fields, ecf_email)},
                        {disc_copies, Nodes}]),
    ok.


-spec get_confirm_code(ecf_user:id()) -> undefined | binary().
get_confirm_code(Id) ->
    get_code(Id, confirm).

-spec get_password_reset_code(ecf_user:id()) -> undefined | binary().
get_password_reset_code(Id) ->
    get_code(Id, reset_password).

-spec get_code(ecf_user:id(), type()) -> undefined | binary().
get_code(Id, Type) ->
    F = fun() ->
                case mnesia:read({ecf_email, {Id, Type}}) of
                    [] ->
                        undefined;
                    [C] ->
                        Limit = get_limit(Type),
                        Diff = erlang:system_time(second) - C#ecf_email.time,
                        case Diff > Limit of
                            true ->
                                mnesia:delete_object(C),
                                undefined;
                            false ->
                                C#ecf_email.code
                        end
                end
        end,
    mnesia:activity(transaction, F).


-spec clear_confirm_code(ecf_user:id()) -> ok.
clear_confirm_code(Id) ->
    clear_code(Id, confirm).

-spec clear_password_reset_code(ecf_user:id()) -> ok.
clear_password_reset_code(Id) ->
    clear_code(Id, reset_password).

-spec clear_code(ecf_user:id(), type()) -> ok.
clear_code(Id, Type) ->
    F = fun() ->
                mnesia:delete({ecf_email, {Id, Type}})
        end,
    mnesia:activity(transaction, F).


-spec send_confirmation_email(ecf_user:user()) -> ok.
send_confirmation_email(User) ->
    send_email(User, confirm).

-spec send_password_reset_email(ecf_user:user()) -> ok.
send_password_reset_email(User) ->
    send_email(User, reset_password).

-spec send_email(ecf_user:user(), type()) -> ok.
send_email(User, Type) ->
    {ok, ForumName} = application:get_env(ecf, forum_name),
    Subject = make_subject(Type, ForumName),
    Code = make_code(),
    Body = make_body(Type, ForumName, Code),
    Username = ecf_user:name(User),
    Email0 = ecf_user:email(User),
    Email = <<"<",Email0/binary,">">>,
    {ok, Addr0} = application:get_env(ecf, email_addr),
    Addr = <<"<",Addr0/binary,">">>,
    {ok, Relay} = application:get_env(ecf, email_relay),
    {ok, EHost} = application:get_env(ecf, email_host),
    Mail = mimemail:encode({<<"text">>, <<"plain">>,
                            [{<<"Subject">>, Subject},
                             {<<"From">>, <<ForumName/binary," ",Addr/binary>>},
                             {<<"To">>, <<Username/binary," ",Email/binary>>}],
                            [],
                            Body}),
    {ok, _} = gen_smtp_client:send({Addr, [Email], Mail},
                                   [{relay, Relay}, {hostname, EHost}]),
    F = fun() ->
                mnesia:write(#ecf_email{key={ecf_user:id(User), Type},
                                        code=Code,
                                        time=erlang:system_time(second)})
        end,
    mnesia:activity(transaction, F).


-spec make_subject(type(), binary()) -> binary().
make_subject(confirm, ForumName) ->
    <<"Confirm your email for ", ForumName/binary>>;

make_subject(reset_password, ForumName) ->
    <<"Reset your password for ", ForumName/binary>>.


-spec make_body(type(), binary(), binary()) -> binary().
make_body(Type, ForumName, Code) ->
    {ok, Host} = application:get_env(ecf, host),
    Base = application:get_env(ecf, base_url, <<"">>),
    BaseUrl = <<"https://", Host/binary, Base/binary>>,
    make_body(Type, BaseUrl,  ForumName, Code).


make_body(confirm, BaseUrl, ForumName, Code) ->
    <<"Thanks for signing up at ", ForumName/binary, ".\n\n",
     "Use the following link to confirm your email:\n\n",
     BaseUrl/binary,"/confirm?code=",Code/binary>>;

make_body(reset_password, BaseUrl, ForumName, Code) ->
    <<"A password reset was requested for your account at ",
      ForumName/binary, ".\n\n",
      "If you did not make this request, you can safely ignore this email.\n\n",
      "If you did make this request, use this link to reset your password:\n\n",
      BaseUrl/binary,"/reset_password?code=",Code/binary>>.


make_code() ->
    Possible = "1234567890" ++ lists:seq($A, $Z) ++ lists:seq($a, $z),
    crypto:rand_seed(),
    list_to_binary([lists:nth(rand:uniform(length(Possible)), Possible)
                    || _ <- lists:seq(1,64)]).


get_limit(confirm) ->
    application:get_env(ecf, days_confirm_email, 7) * ?SECS_PER_DAY;
get_limit(reset_password) ->
    application:get_env(ecf, days_reset_password, 1) * ?SECS_PER_DAY.

