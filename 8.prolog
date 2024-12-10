day(8). testResult(part1, 14). testResult(part2, 34).

:- use_module(lib/solve).

antinode([Xn,Yn], V) :-
  point([Xa1,Ya1], A), point([Xa2,Ya2], A),
  A \= '.', [Xa1,Ya1] \= [Xa2,Ya2],
  point([Xn,Yn], _),
  V is (Xn - Xa1) / (Xa2 - Xa1),
  V > 0, V =:= (Yn - Ya1) / (Ya2 - Ya1).
  
resultPart1(_, Count) :- aggregate_all(count, Antinode, antinode(Antinode,2), Count).
resultPart2(_, Count) :- aggregate_all(count, Antinode, antinode(Antinode,_), Count).

/* required for loadData */
data_line(Index, Data, Line) :- string_chars(Line, Data), assertPoints(Index, 1, Data).

assertPoints(_, _, []).
assertPoints(X, Y, [H|T]) :- assert(point([X,Y],H)), Yn is Y+1, assertPoints(X, Yn, T).
