day(7). testResult(part1, 3749). testResult(part2, 11387).

:- use_module(lib/solve).

equals(Result, _, Result, []).
equals(Result, ConcatEnabled, IntermediateResult, [H|T]) :- NextResult is IntermediateResult + H, equals(Result, ConcatEnabled, NextResult, T).
equals(Result, ConcatEnabled, IntermediateResult, [H|T]) :- NextResult is IntermediateResult * H, equals(Result, ConcatEnabled, NextResult, T).
equals(Result, true, IntermediateResult, [H|T]) :-
  maplist(number_string, [IntermediateResult,H], [S1,S2]),
  string_concat(S1, S2, NextResultString),
  number_string(NextResult, NextResultString),
  equals(Result, true, NextResult, T).

addPossible([Result,FirstNumber|T], ConcatEnabled, IntermediateSum, Sum) :- equals(Result, ConcatEnabled, FirstNumber, T) -> Sum is IntermediateSum + Result ; Sum = IntermediateSum.

resultPart1(Equations, CalibrationResult) :- foldl([E,I,R]>>addPossible(E,false,I,R), Equations, 0, CalibrationResult).
resultPart2(Equations, CalibrationResult) :- foldl([E,I,R]>>addPossible(E,true,I,R), Equations, 0, CalibrationResult).
  
/* required for loadData */
data_line(Numbers, Line) :-
  split_string(Line, " ", ":", NumberStrings),
  maplist(number_string, Numbers, NumberStrings).
