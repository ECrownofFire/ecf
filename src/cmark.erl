-module(cmark).

%%% The goal here is implement CommonMark (http://commonmark.org).

-export([to_html/1]).

-export([strip_ws/1]).

-export([normalize_tabs/2]).

% Default options for REs
-define(OPTIONS, [unicode,
                  multiline,
                  {newline, anycrlf} % Match CR, LR, or CRLF
                 ]).

-define(TAB_VALUE, 4).

% List of things to try
% This ordering might be able to be changed around a bit, but blank lines must
%   come first, and paragraphs must come last.
-define(BLOCKS, [fun is_blank_line/2,
                 fun is_block_quote/2,
                 fun is_atx_header/2,
                 fun is_code_fence/2,
                 %fun is_html_block/2, % TODO: HTML block?
                 fun is_setext_header/2,
                 fun is_thematic_break/2,
                 fun is_a_list/2,
                 fun is_code_block/2,
                 fun is_paragraph/2
                ]).

-type block_tree() :: [block_tree_node()] | ok.

-type block_list() :: block_unordered_list() | block_ordered_list().

-type block_unordered_list() :: {unordered_list,
                                 dash | star | plus,
                                 non_neg_integer(),
                                 iodata()}.

-type block_ordered_list() :: {ordered_list,
                               dot | parens,
                               non_neg_integer(),
                               integer(),
                               iodata()}.

-type block_tree_node() :: thematic_break
                         | blank_line
                         | {atx_header, 1..6}
                         | {code_block, iodata()}
                         | {setext_header, dash | equals}
                         | {code_fence, tilde | back_tick,
                            non_neg_integer(), pos_integer()}
                         | {paragraph, iodata()}
                         | block_list().


%% DO IT!
-spec to_html(iodata()) -> iodata() | ok.
to_html(Document) ->
    Safe = sanitize(Document),
    Blocks = block_phase(Safe),
    Tree = inline_phase(Blocks),
    tree_to_html(Tree).


%% PHASE ONE: Process document into blocks
%% block quotes, paragraphs, lists, etc
-spec block_phase(iodata()) -> block_tree().
block_phase(Document) ->
    Ls = split_lines(Document),
    Lines = [normalize_tabs(L, ?TAB_VALUE) || L <- Ls],
    to_block_tree(Lines).


inline_phase(_BlockTree) ->
    ok.


tree_to_html(_Tree) ->
    ok.


%%%% BLOCK PROCESSING
%%% This gets rather complicated because some block types can affect the
%%%   interpretation of lines after it. For example, indented code blocks cannot
%%%   interrupt a paragraph, lists can be indented, etc.
-spec to_block_tree([iodata()]) -> block_tree().
to_block_tree(Lines) ->
    to_block_tree(Lines, [], ignored).

to_block_tree([], List, _) ->
    List;

% Whitespace at the beginning of a line after a paragraph or block quote is
%   ignored, so just strip it out
to_block_tree([L | Rest], List, Type) when Type =:= paragraph;
                                           Type =:= block_quote ->
    {Line, C} = indent_line(L),
    NewLine = if C > 3 ->
                     block_continuation(Line, 0, ?BLOCKS);
                 C =< 3 ->
                     case block_continuation(Line, C, [fun is_a_list/2,
                                                       fun is_code_fence/2]) of
                         T when is_tuple(T) ->
                             T;
                         L ->
                             block_continuation(Line, 0, ?BLOCKS)
                     end
              end,
    to_block_tree(Rest, [NewLine|List], element(1, NewLine));

% Logic here is a bit complicated...
to_block_tree([L | Rest], List, Type) when Type =:= code_fence ->
    [H|_] = List,
    tbt_in_code_fence([L | Rest], List, H);

to_block_tree([L | Rest], List, _) ->
    {Line, Count} = indent_line(L),
    NewLine = block_continuation(Line, Count, ?BLOCKS),
    to_block_tree(Rest, [NewLine|List], element(1, NewLine)).


tbt_in_code_fence([L | Rest], List, H) ->
    % First, we need to know stuff about the previous code fence
    {_, T, C, Count} = H,
    {Line, _} = indent_line(L),
    % If the next line has >3 spaces, it's automatically inside the fence
    case is_code_fence(Line, 0) of
        % In order to end the fence, it must be of the same type T
        %     and have at least as many symbols as the last fence
        % Note: it's perfectly valid to have an empty fenced block
        NL = {code_fence, T, _, Count2} when Count2 >= Count ->
            % anything can directly follow a fenced code block
            to_block_tree(Rest, [NL, List], ignored);
        false ->
            % Finally, if inside a block, leading spaces equal to
            % the number the opening fence had are stripped out
            NL = {fenced_code, replace(L, binary:copy(<<" ">>,C), <<"">>)},
            tbt_in_code_fence(Rest, [NL | List], H)
    end.

% Sort of a pseudo CPS thing here, makes it a bit less ugly than it would be
% otherwise
block_continuation(Line, _, []) ->
    Line;
block_continuation(Line, Count, [Func|Rest]) ->
    case Func(Line, Count) of
        false ->
            block_continuation(Line, Count, Rest);
        Res ->
            Res
    end.

%%% LEAF BLOCKS
%%% these blocks cannot have any children
%%% some will have content, others don't
%%% signifiers are stripped by these functions
%%% NOTE: For the most part, these merely decide whether a line COULD be
%%%       interpreted as a particular block. For example, the setext headers
%%%       would need to follow a plain line to actually be a header.

% Called last, default choice
is_paragraph(Line, _) ->
    {paragraph, Line}.


% A blank line contains nothing but whitespace
is_blank_line(Line, _) ->
    case run(Line, <<"[^ \\t]">>) of
        nomatch -> blank_line;
        _ -> false
    end.


% A thematic break has 3 or more *, -, or = and nothing else but whitespace
% a single line must only have one of those types of characters
is_thematic_break(Line, _) ->
    case run(Line,
<<"^(?:(?:\\*[ \\t]*){3,}|(?:_[ \\t]*){3,}|(?:-[ \\t]*){3,})[ \\t]*$">>)
    of
        nomatch -> false;
        _ -> thematic_break
    end.


% An ATX header is defined with 1-6 # at the beginning of the line, with no
%   separating whitespace
% The number of # determines the header level (h1-h6)
% There must be at least one space of whitespace before the content
% That whitespace is stripped, as is any whitespace at the end of the content
% An ATX header may have any number of # at the end of the line, which is not
%   part of the content
is_atx_header(Line, _) ->
    case run(Line, <<"^#{1,6}(?:[ \\t]+|$)">>) of
        nomatch -> false;
        _ -> {match, [{0, Count}]} = run(Line, <<"^#{1,6}">>),
             StrippedLeft  = replace(Line, <<"^#{1,6}[ \\t]*">>, ""),
             StrippedRight = replace(StrippedLeft, <<" *#+ *$">>, ""),
             {atx_header,Count,StrippedRight}
    end.


%% Those are the simple ones.
%% these ones are (possibly) multi-lined
%% will need like a state machine or something like that

% A code block begins with 4 spaces (more spaces are part of the content)
% however, code blocks cannot interrupt paragraphs (in that case, the leading
%   whitspace is stripped and the paragraph continues as normal)
% Code blocks cannot interrupt paragraphs, unlike most blocks
is_code_block(Line, Count) when Count >= 4 ->
    {code_block, Line};
is_code_block(Line, _) ->
    case run(Line, <<"^ {4}">>) of
        nomatch -> false;
        _ -> {code_block, replace(Line, <<"^ {4}">>, "")}
    end.


% A setext header is at least 1 - or = directly underneath a paragraph that
%   also cannot be interpreted as anything else
is_setext_header(Line, _) ->
    case run(Line, <<"^=+[ \\t]*">>) of
        nomatch ->
            case run(Line, <<"^-+[ \\t]*">>) of
                nomatch -> false;
                _ -> {setext_header, dash}
            end;
        _ -> {setext_header, equals}
    end.


% TODO: info_string for code fence
is_code_fence(Line, C) ->
    case run(Line, <<"^`{3,}">>) of
        nomatch ->
            case run(Line, <<"^~{3,}">>) of
                nomatch ->
                    false;
                {match, [{0, Count}]} ->
                    {code_fence, tilde, C, Count}
            end;
        {match, [{0, Count}]} ->
            {code_fence, back_tick, C, Count}
    end.


%%% CONTAINER BLOCKS
%%% these will need extra processing to figure out if they have nested blocks

-spec is_block_quote(iodata(), non_neg_integer()) ->
    {block_quote, iodata()} | false.
is_block_quote(Line, _) ->
    case run(Line, <<"^>">>) of
        nomatch -> false;
        _ -> {block_quote, replace(Line, <<"^> ?">>, <<"">>)}
    end.


% extra "a" in the name to avoid conflict with guard is_list
-spec is_a_list(iodata(), non_neg_integer()) -> block_list() | false.
is_a_list(Line, Count) ->
    case replace(Line, <<"^\\*">>, <<"">>) of
        Line ->
            case replace(Line, <<"^\\+">>, <<"">>) of
                Line ->
                    case replace(Line, <<"^-">>, <<"">>) of
                        Line ->
                            is_ordered_list(Line, Count);
                        NewLine ->
                            {unordered_list, dash, Count, NewLine}
                    end;
                NewLine ->
                    {unordered_list, plus, Count, NewLine}
            end;
        NewLine ->
            {unordered_list, star, Count, NewLine}
    end.


-spec is_ordered_list(iodata(), non_neg_integer()) ->
    block_ordered_list() | false.
is_ordered_list(Line, Count) ->
    case run(Line, <<"^\\d{1,9}\\.">>) of
        {match, [{0,Digits}]} ->
            Number = binary_to_integer(binary_part(iolist_to_binary(Line),
                                                   {0, Digits-1})),
            {ordered_list, dot, Count, Number,
             replace(Line, <<"^\\d{1,9}\\.">>, <<"">>)};
        nomatch ->
            case run(Line, <<"^\\d{1,9}\\)">>) of
                {match, [{0,Digits}]} ->
                    Number =
                    binary_to_integer(binary_part(iolist_to_binary(Line),
                                                  {0, Digits-1})),
                    {ordered_list, parens, Count, Number,
                     replace(Line, <<"^\\d{1,9}\\)">>, <<"">>)};
                nomatch ->
                    false
            end
    end.


%%% UTILITY FUNCTIONS
%%% various useful things for processing

% 0-3 spaces followed by a tab is replaced with four spaces
% if less than 4 spaces, they're stripped
-spec indent_line(iodata()) -> {iodata(), non_neg_integer()}.
indent_line(Line) ->
    Tabbed = replace(Line, <<"^ {0,3}\\t">>, <<"    ">>),
    {match, [{0, Count}]} = run(Line, <<"^ +">>),
    {replace(Tabbed, <<"^ {0,3}">>, <<"">>), Count}.

% Strip all whitespace from beginning of line
-spec strip_ws(iodata()) -> {iodata(), non_neg_integer()}.
strip_ws(Line) ->
    {match, [{0, Count}]} = run(Line, <<"^ +">>),
    {replace(Line, <<"^ +">>, <<"">>), Count}.



% Intelligently replace tabs in a line with N spaces
-spec normalize_tabs(iodata(), non_neg_integer()) -> binary().
normalize_tabs(Line, N) ->
    normalize_tabs(iolist_to_binary(Line), N, <<"">>).

% Acc collects the spaces, then is pasted back
-spec normalize_tabs(binary(), non_neg_integer(), binary()) ->
    binary().
normalize_tabs(Line, N, Acc) when binary_part(Line, 0, 1) =:= <<" ">> ->
    <<" ",  Rest/binary>> = Line,
    normalize_tabs(Rest, N, <<" ", Acc/binary>>);

normalize_tabs(Line, N, Acc) when binary_part(Line, 0, 1) =:= <<"\t">> ->
    <<"\t", Rest/binary>> = Line,
    Col = byte_size(Acc) rem N,
    Bin = binary:copy(<<" ">>, N-Col),
    normalize_tabs(Rest, N, <<Bin/binary,Acc/binary>>);

normalize_tabs(Line, _, Acc) ->
    <<Acc/binary, Line/binary>>.


% Split lines and return a list, excluding all newlines
-spec split_lines(iodata()) -> [iodata()].
split_lines(Document) ->
    re:split(Document, <<"\\n|\\r|\\r\\n|\\n\\r">>).

%% (Hopefully) sanitizes this to safe HTML
%% Note: this is ONLY okay to put inside a body tag
-spec sanitize(iodata()) -> iodata().
sanitize(Document) ->
    % Here's where I really wish Erlang had a "pipe" operator...
    Doc  = replace(Document, <<"\\x00">>, <<"\\xFFFD">>),
    % Replace ampersand first, obviously.
    Doc2 = replace(Doc, <<"\\&">>, <<"\\&amp;">>),
    Doc3 = replace(Doc2, <<"<">>, <<"\\&lt;">>),
    Doc4 = replace(Doc3, <<">">>, <<"\\&gt;">>),
    Doc5 = replace(Doc4, <<"\"">>, <<"\\&quot;">>),
    Doc6 = replace(Doc5, <<"'">>, <<"\\&#x27;">>),
    replace(Doc6, <<"/">>, <<"\\&#x2F;">>).

%% Couple of helper functions with default options
run(A, B) ->
    re:run(A, B, ?OPTIONS).

replace(A, B, C) ->
    re:replace(A, B, C, ?OPTIONS).

