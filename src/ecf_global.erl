-module(ecf_global).

% mnesia-backed mochiglobal-based globals
% from https://github.com/mochi/mochiweb/blob/master/src/mochiglobal.erl
% Compared to mochiglobal:
% 1. Retrieving nonexistent keys is slower.
% 2. Writes are slower.
% 3. Retrieving keys for the first time is also quite slow.
% TODO: benchmark this?


-export([create_table/1,
         get/1, get/2,
         put/2,
         delete/1]).

-record(ecf_global,
        {key :: atom(),
         val :: any()}).

-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_global,
                        [{attributes, record_info(fields, ecf_global)},
                         {disc_copies, Nodes}]),
    ok.

-spec get(atom()) -> any().
get(Key) ->
    get(Key, undefined).

-spec get(atom(), any()) -> any().
get(Key, Default) ->
    Mod = key_to_module(Key),
    try Mod:value()
    catch error:undef ->
            db_get(Key, Default)
    end.

-spec put(atom(), any()) -> ok.
put(Key, Val) ->
    F = fun() ->
                mnesia:write(#ecf_global{key=Key,val=Val})
        end,
    mnesia:activity(transaction, F),
    global_put(Key, Val).

-spec delete(atom()) -> ok.
delete(Key) ->
    F = fun() ->
                mnesia:delete({ecf_global, Key})
        end,
    mnesia:activity(transaction, F),
    Mod = key_to_module(Key),
    code:purge(Mod),
    code:delete(Mod),
    ok.

%% internal functions

-spec db_get(atom(), any()) -> any().
db_get(Key, Default) ->
    F = fun() ->
                mnesia:read({ecf_global, Key})
        end,
    case mnesia:activity(transaction, F) of
        [Val] ->
            global_put(Key, Val),
            Val;
        _ ->
            Default
    end.

-spec global_put(atom(), any()) -> ok.
global_put(Key, Val) ->
    Mod = key_to_module(Key),
    Bin = compile(Mod, Val),
    code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
    ok.

-spec key_to_module(atom()) -> atom().
key_to_module(Key) ->
    list_to_atom("ecf_global:" ++ atom_to_list(Key)).

-spec compile(atom(), any()) -> binary().
compile(Module, T) ->
    {ok, Module, Bin} = compile:forms(forms(Module, T),
                                      [verbose, report_errors]),
    Bin.

-spec forms(atom(), any()) -> [erl_syntax:syntaxTree()].
forms(Module, T) ->
    [erl_syntax:revert(X) || X <- term_to_abstract(Module, term, T)].

-spec term_to_abstract(atom(), atom(), any()) -> [erl_syntax:syntaxTree()].
term_to_abstract(Module, Getter, T) ->
    [%% -module(Module).
     erl_syntax:attribute(
       erl_syntax:atom(module),
       [erl_syntax:atom(Module)]),
     %% -export([Getter/0]).
     erl_syntax:attribute(
       erl_syntax:atom(export),
       [erl_syntax:list(
         [erl_syntax:arity_qualifier(
            erl_syntax:atom(Getter),
            erl_syntax:integer(0))])]),
     %% Getter() -> T.
     erl_syntax:function(
       erl_syntax:atom(Getter),
[erl_syntax:clause([], none, [erl_syntax:abstract(T)])])].

