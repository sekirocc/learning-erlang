-module (calc).
-compile (export_all).

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H, Start), T).

rpn(L) when is_list(L) ->
  [Res] = fold(fun rpn/2, [], string:tokens(L, " ")),
  Res.

rpn("+", [N1, N2|S]) -> [N1+N2|S];
rpn("-", [N1, N2|S]) -> [N2-N1|S];
rpn("*", [N1, N2|S]) -> [N1*N2|S];
rpn("/", [N1, N2|S]) -> [N2/N1|S];
rpn("^", [N1, N2|S]) -> [math:pow(N2,N1)|S];
rpn("ln", [N1|S]) -> [math:log(N1)|S];
rpn("log10", [N1|S]) -> [math:log10(N1)|S];
rpn("sum", S) -> [lists:sum(S)];
rpn("prod", S) -> [fold(fun erlang:'*'/2, 1, S)];
rpn(X, Stack) -> [read(X)|Stack].

rpn_sum([]) -> 0;
rpn_sum([N1|S]) -> N1 + rpn_sum(S).

read(N) ->
  case string:to_float(N) of
    {error, no_float} -> list_to_integer(N);
    {F, _} -> F
  end.