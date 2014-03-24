#! /usr/bin/env escript
-mode(compile).

main([FileName]) ->
  {ok, Bin} = file:read_file(FileName),
  Map = lists:reverse(group_vals(parse_map(Bin))),
  io:format("~p~n", [optimal_path(Map)]),
  erlang:halt().

parse_map(B) when is_binary(B) ->
  parse_map(binary_to_list(B));
parse_map(Str) when is_list(Str) ->
  [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ") ].

group_vals(L) -> group_vals(L, []).
group_vals([], Acc) -> Acc;
group_vals([A,B,X | L], Acc) -> group_vals(L, [{A,B,X} | Acc]).

optimal_path(Map) ->
  {A, B} = lists:foldl(fun shortest_path/2, {{0, []}, {0, []}}, Map),
  {_, Path} = if hd(element(2, A)) =/= {x, 0} -> A;
                 hd(element(2, B)) =/= {x, 0} -> B
              end,
  lists:reverse(Path).

shortest_path({A, B, X}, {{DistA, PathA}, {DistB, PathB}}) ->
  OptA1 = { DistA + A, [ {a, A}|PathA] },
  OptA2 = { DistB + B + X, [ {x, X}, {b, B} | PathB ]},
  OptB1 = { DistB + B, [ {b, B} | PathB ] },
  OptB2 = { DistA + A + X, [ {x, X}, {a, A} | PathA ]},
  { erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2) }.