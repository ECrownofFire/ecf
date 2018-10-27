-module(ecf_perm_pt).

-export([parse_transform/2]).

parse_transform(Forms, _Opt) ->
    List = get_cons(Forms),
    DoTrans = fun ({call, _L0, {atom, _L1, get_modes}, _Args}) ->
                      List;
                  (_) ->
                      continue
              end,
    parse_trans:plain_transform(DoTrans, Forms).

get_cons([{attribute, _L0, type, {mode, Union, []}}|_T]) ->
    Types = erl_syntax:type_union_types(Union),
    erl_syntax:revert(erl_syntax:list(Types));
get_cons([_|T]) ->
    get_cons(T).

