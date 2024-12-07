day(7). testResult(11387).

:- use_module(lib/solve).

equals(Result, Result, []).
equals(Result, IntermediateResult, [H|T]) :- NextResult is IntermediateResult + H, equals(Result, NextResult, T).
equals(Result, IntermediateResult, [H|T]) :- NextResult is IntermediateResult * H, equals(Result, NextResult, T).
equals(Result, IntermediateResult, [H|T]) :-
  maplist(number_string, [IntermediateResult,H], [S1,S2]),
  string_concat(S1, S2, NextResultString),
  number_string(NextResult, NextResultString),
  equals(Result, NextResult, T).

addPossible([Result,FirstNumber|T], IntermediateSum, Sum) :- equals(Result, FirstNumber, T) -> Sum is IntermediateSum + Result ; Sum = IntermediateSum.

result(Equations, CalibrationResult) :- foldl(addPossible, Equations, 0, CalibrationResult).
  
/* required for loadData */
data_line(Numbers, Line) :-
  split_string(Line, " ", ":", NumberStrings),
  maplist(number_string, Numbers, NumberStrings).
