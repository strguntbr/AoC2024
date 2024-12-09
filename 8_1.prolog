day(8). testResult(14).

:- use_module(lib/solve).

antinode([Xn,Yn]) :-
  point([Xa1,Ya1], A), point([Xa2,Ya2], A),
  A \= '.', [Xa1,Ya1] \= [Xa2,Ya2],
  Xn is 2 * Xa1 - Xa2, Yn is 2 * Ya1 - Ya2,
  point([Xn,Yn], _).

result(_, Count) :- setof(A, antinode(A), Antinodes), length(Antinodes, Count).

/* required for loadData */
data_line(Index, Data, Line) :- string_chars(Line, Data), assertPoints(Index, 1, Data).

assertPoints(_, _, []).
assertPoints(X, Y, [H|T]) :- assert(point([X,Y],H)), Yn is Y+1, assertPoints(X, Yn, T).
