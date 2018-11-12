-module(ecf_confirm_handler).

-behaviour(cowboy_handler).

-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
    #{code := Code} = cowboy_req:match_qs([{code, [], <<>>}], Req),
    case check_code(Code, Req) of
        {ok, User} ->
            Html = ecf_generators:generate(confirm_email, User, Code),
            Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    Html, Req),
            {ok, Req2, State};
        R ->
            {ok, R, State}
    end;
init(Req0 = #{method := <<"POST">>}, State) ->
    L = [{code, [], <<>>}],
    {ok, M, Req} = cowboy_req:read_and_match_urlencoded_body(L, Req0),
    #{code := Code} = M,
    Req2 = case check_code(Code, Req) of
               {ok, User} ->
                   Id = ecf_user:id(User),
                   ok = ecf_group:add_member(2, Id),
                   ok = ecf_email:clear_confirm_code(Id),
                   Html = ecf_generators:generate(confirmed_email, User, ignored),
                   cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>},
                                    Html, Req);
               R ->
                   R
           end,
    {ok, Req2, State}.

check_code(Code, Req) ->
    case ecf_utils:check_user_session(Req) of
        undefined ->
            ecf_utils:reply_status(401, undefined, confirm_401, Req);
        User ->
            case ecf_email:get_confirm_code(ecf_user:id(User)) of
                undefined ->
                    ecf_utils:reply_status(400, User, confirm_400, Req);
                Code ->
                    {ok, User}
            end
    end.

