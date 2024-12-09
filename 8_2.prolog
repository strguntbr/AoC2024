day(8). testResult(34).

:- use_module(lib/solve).

antinode([Xn,Yn]) :-
  point([Xa1,Ya1], A), point([Xa2,Ya2], A),
  A \= '.', [Xa1,Ya1] \= [Xa2,Ya2],
  point([Xn,Yn], _),
  V is (Xn - Xa1) / (Xa2 - Xa1),
  V > 0, V =:= (Yn - Ya1) / (Ya2 - Ya1).
  
result(_, Count) :- setof(Antinode, antinode(Antinode), Antinodes), length(Antinodes, Count).

/* required for loadData */
data_line(Index, Data, Line) :- string_chars(Line, Data), assertPoints(Index, 1, Data).

assertPoints(_, _, []).
assertPoints(X, Y, [H|T]) :- assert(point([X,Y],H)), Yn is Y+1, assertPoints(X, Yn, T).
