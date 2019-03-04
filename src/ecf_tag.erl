-module(ecf_tag).

-export([create_table/1,
         new_tag/1,
         get_tag/1,
         delete_tag/1,
         tag_thread/2, untag_thread/2,
         get_tags/1, get_threads/1,
         delete_thread/1,
         name/1]).

-export_type([tag/0]).

% future-proofing, in case anybody wants tag categories or whatever
-record(ecf_tag,
        {name :: binary(),
         dummy = undefined}).
-opaque tag() :: #ecf_tag{}.

-record(ecf_tag_thread,
        {tag :: binary(),
         thread :: ecf_thread:id()}).


-spec create_table([node()]) -> ok.
create_table(Nodes) ->
    {atomic, ok} = mnesia:create_table(ecf_tag,
                        [{attributes, record_info(fields, ecf_tag)},
                         {disc_copies, Nodes}]),
    {atomic, ok} = mnesia:create_table(ecf_tag_thread,
                        [{attributes, record_info(fields, ecf_tag_thread)},
                         {type, bag},
                         {index, [#ecf_tag_thread.thread]},
                         {disc_copies, Nodes}]),
    ok.


-spec new_tag(binary()) -> ok | {already_exists, tag()}.
new_tag(Name) ->
    F = fun() ->
            case get_tag(Name) of
                undefined ->
                    mnesia:write(#ecf_tag{name = Name});
                Tag ->
                    {already_exists, Tag}
            end
        end,
    mnesia:activity(transaction, F).

-spec get_tag(binary()) -> tag() | undefined.
get_tag(Name) ->
    F = fun() ->
            case mnesia:read({ecf_tag, Name}) of
                [] -> undefined;
                [T] -> T
            end
        end,
    mnesia:activity(transaction, F).

-spec delete_tag(binary()) -> ok.
delete_tag(Name) ->
    F = fun() ->
            case mnesia:wread({ecf_tag, Name}) of
                [] -> ok;
                [_T] ->
                    mnesia:delete({ecf_tag_thread, Name}),
                    mnesia:delete({ecf_tag, Name})
            end
        end,
    mnesia:activity(transaction, F).

-spec tag_thread(binary(), ecf_thread:id()) -> ok | {error, tag_not_found}.
tag_thread(Name, Thread) ->
    F = fun() ->
            case mnesia:read({ecf_tag, Name}) of
                [] -> {error, tag_not_found};
                [_T] ->
                    mnesia:write(#ecf_tag_thread{tag=Name, thread=Thread})
            end
        end,
    mnesia:activity(transaction, F).

-spec untag_thread(binary(), ecf_thread:id()) -> ok.
untag_thread(Name, Thread) ->
    F = fun() ->
            mnesia:delete_object(#ecf_tag_thread{tag=Name,thread=Thread})
        end,
    mnesia:activity(transaction, F).

-spec get_tags(ecf_thread:id()) -> [tag()].
get_tags(Thread) ->
    F = fun() ->
            TagThreads = mnesia:index_read(ecf_tag_thread, Thread,
                                              #ecf_tag_thread.thread),
            lists:flatten([mnesia:read({ecf_tag, X#ecf_tag_thread.tag}) || X <- TagThreads])
        end,
    mnesia:activity(transaction, F).

-spec get_threads(binary()) -> [ecf_thread:id()].
get_threads(Name) ->
    F = fun() ->
            List = mnesia:read({ecf_tag_thread, Name}),
            [X#ecf_tag_thread.thread || X <- List]
        end,
    mnesia:activity(transaction, F).

-spec delete_thread(ecf_thread:id()) -> ok.
delete_thread(Thread) ->
    F = fun() ->
            Tags = mnesia:index_read(ecf_tag_thread, Thread, #ecf_tag_thread.thread),
            [mnesia:delete_object(X) || X <- Tags],
            ok
        end,
    mnesia:activity(transaction, F).

%% wrappers
-spec name(tag()) -> binary().
name(Tag) ->
    Tag#ecf_tag.name.

