day(8). testResult(14).

:- use_module(lib/solve).

antinode([Xn,Yn]) :-
  point([Xa1,Ya1], A),
  point([Xa2,Ya2], A),
  A \= '.',
  [Xa1,Ya1] \= [Xa2,Ya2],
  Xn is 2 * Xa1 - Xa2,
  Yn is 2 * Ya1 - Ya2,
  point([Xn,Yn], _).

result(_, Count) :- setof(A, antinode(A), Antinodes), length(Antinodes, Count).
/*  
  findall(Antinode, antinode(Antinode), Antinodes),
  sort(Antinodes, Uniqueantinodes),
  length(Uniqueantinodes, Count).*/

/* required for loadData */
resetData :- retractall(point(_,_)).

data_line(Index, Data, Line) :- string_chars(Line, Data), assertPoint(Index, 1, Data).

assertPoint(_, _, []).
assertPoint(X, Y, [H|T]) :- assert(point([X,Y],H)), Yn is Y+1, assertPoint(X, Yn, T).
