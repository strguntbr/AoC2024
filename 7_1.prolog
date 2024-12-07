day(7). testResult(3749).

:- use_module(lib/solve).

equals(Result, Result, []).
equals(Result, IntermediateResult, [H|T]) :- NextResult is IntermediateResult + H, equals(Result, NextResult, T).
equals(Result, IntermediateResult, [H|T]) :- NextResult is IntermediateResult * H, equals(Result, NextResult, T).

addPossible([Result,FirstNumber|T], IntermediateSum, Sum) :- equals(Result, FirstNumber, T) -> Sum is IntermediateSum + Result ; Sum = IntermediateSum.

result(Equations, CalibrationResult) :- foldl(addPossible, Equations, 0, CalibrationResult).

/* required for loadData */
data_line(Numbers, Line) :-
  split_string(Line, " ", ":", NumberStrings),
  maplist(number_string, Numbers, NumberStrings).