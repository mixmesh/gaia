%%%-------------------------------------------------------------------
%%% Copyright (C) 2015, Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @author Robby Raschke <robby.raschke@erlang-solutions.com>
%%% @doc
%%% Fuzzy Name Matching Library.
%%% Inspired by http://www.sportshacker.net/posts/fuzzy_string_matching.html
%%% @end
%%% Created: 9 Feb 2015 by Robby Raschke <erlang-solutions.com>
%%%-------------------------------------------------------------------

-module(gaia_fuzzy).
-export([match/2, match/3]).

-type matchers() :: [exact | abbreviation | levenshtein |  tokens].

%%
%% Export: match
%%

-spec match(binary(), [binary()]) -> nomatch | {ok, binary()}.

match(S, Canonicals) ->
    do_match(
      S, Canonicals,
      [fun exact/2, fun abbreviation/2, fun levenshtein/2, fun tokens/2]).


-spec match(binary(), [binary()], matchers() | all) -> nomatch | {ok, binary()}.

match(S, Canonicals, all) ->
    match(S, Canonicals);
match(S, Canonicals, Matchers) ->
    do_match(S, Canonicals, get_matcher_callbacks(Matchers)).

get_matcher_callbacks([]) ->
    [];
get_matcher_callbacks([exact|Rest]) ->
    [fun exact/2|get_matcher_callbacks(Rest)];
get_matcher_callbacks([abbreviation|Rest]) ->
    [fun abbreviation/2|get_matcher_callbacks(Rest)];
get_matcher_callbacks([levenshtein|Rest]) ->
    [fun levenshtein/2|get_matcher_callbacks(Rest)];
get_matcher_callbacks([tokens|Rest]) ->
    [fun tokens/2|get_matcher_callbacks(Rest)].

do_match(_S, _Canonicals, []) ->
    nomatch;
do_match(S, Canonicals, [Matcher|Rest]) ->
    case Matcher(S, Canonicals) of
        {ok, T} ->
            {ok, T};
        undefined ->
            do_match(S, Canonicals, Rest)
    end.

%%
%% Exact match
%%

exact(S, Canon) ->
    case lists:member(S, Canon) of
        true ->
            {ok, S};
        false ->
            undefined
    end.

%%
%% Abbrevation match
%%

%% * The first letter of the abbreviation must match the first letter of the text
%% * The rest of the abbreviation (the abbrev minus the first letter) must be an abbreviation for:
%%   a) the remaining words, or
%%   b) the remaining text starting from any position in the first word.
%%
%% http://stackoverflow.com/questions/7331462/check-if-a-string-is-a-possible-abbrevation-for-a-name

abbreviation(_S, []) ->
    undefined;
abbreviation(S, [Name | Rest]) ->
    case is_abbreviation(S, Name) orelse is_abbreviation(Name, S) of
        true ->
            {ok, Name};
        false ->
            abbreviation(S, Rest)
    end.

is_abbreviation(<<>>, _Text) ->
    true;
is_abbreviation(<<C:8, Abv/binary>> = _Abbrev, <<C:8, Txt/binary>> = Text) ->
    [<<_:8, Fst/binary>> | Rest] = split(Text),
    is_abbreviation(Abv, join(Rest, <<" ">>)) orelse
        lists:any(fun (S) ->
                          is_abbreviation(Abv, S)
                  end,
                  postfixes(Fst, Txt, []));
is_abbreviation(_Abbrev, _Text) ->
    false.

join([], _) ->
    <<>>;
join([S], _) when is_binary(S) ->
    S;
join([H | T], Sep) ->
    B = << <<Sep/binary, X/binary>> || X <- T >>,
    <<H/binary, B/binary>>.

postfixes(<<>>, Text, Acc) ->
    lists:reverse([Text | Acc]);
postfixes(<<_:8, Fst/binary>>, <<_:8, Txt/binary>> = Text, Acc) ->
    postfixes(Fst, Txt, [Text | Acc]).

%%
%% Levenshtein distance match
%%

levenshtein(_S, []) ->
    undefined;
levenshtein(S, [Name | Rest]) ->
    case levenshtein_distance(S, Name) =< 2 of
        true ->
            {ok, Name};
        false ->
            levenshtein(S, Rest)
    end.

%% http://rosettacode.org/wiki/Levenshtein_distance#Erlang

levenshtein_distance(S, T) ->
    {L,_} = ld(S, T, dict:new()),
    L.

ld(<<>> = S, T, Cache) ->
    {byte_size(T), dict:store({S,T}, byte_size(T), Cache)};
ld(S, <<>> = T, Cache) ->
    {byte_size(S), dict:store({S,T}, byte_size(S), Cache)};
ld(<<X:8, S/binary>>, <<X:8, T/binary>>, Cache) ->
    ld(S, T, Cache);
ld(<<_SH:8, ST/binary>> = S, <<_TH:8, TT/binary>> = T, Cache) ->
    case dict:is_key({S,T}, Cache) of
        true -> {dict:fetch({S,T}, Cache), Cache};
        false ->
            {L1, C1} = ld(S, TT, Cache),
            {L2, C2} = ld(ST, T, C1),
            {L3, C3} = ld(ST, TT, C2),
            L = 1 + lists:min([L1, L2, L3]),
            {L, dict:store({S,T}, L, C3)}
    end.

%%
%% Token match
%%

tokens(_String, []) ->
    undefined;
tokens(S, [Name|Rest]) ->
    case lists:any(
           fun({A, B}) ->
                   A == B orelse levenshtein_distance(A, B) =< 2
           end,
           [{A, B} || A <- split(S), B <- split(Name)]) of
        true ->
            {ok, Name};
        false ->
            tokens(S, Rest)
    end.

ws() -> [<<" ">>, <<"\t">>, <<"\r\n">>, <<"\n">>, <<"\r">>, <<"\f">>].

split(B) ->
    lists:filter(
      fun (<<>>) ->
              false;
          (_) -> true
      end,
      binary:split(B, ws(), [global])).
