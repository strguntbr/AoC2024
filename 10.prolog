day(10). testResult(part1, 36). testResult(part2, 81).

:- use_module(lib/solve).

neighbor([X1,Y1], [X2,Y1]) :- X2 is X1-1 ; X2 is X1+1.
neighbor([X1,Y1], [X1,Y2]) :- Y2 is Y1-1 ; Y2 is Y1+1.

step([X1,Y1], [X2,Y2], H1, H2) :- height([X1,Y1], H1), neighbor([X1,Y1], [X2,Y2]), H2 is H1+1, height([X2,Y2], H2).

path(P, P, H, H) :- !.
path(Start, End, StartH, EndH) :- step(Start, Next, StartH, NextH), path(Next, End, NextH, EndH).

resultPart1(_, Count) :- aggregate_all(count, [E,S], path(E,S,0,9), Count).
resultPart2(_, Count) :- aggregate_all(count, path(_,_,0,9), Count).

/* required for loadData */
data_line(Index, Data, Line) :-
  string_chars(Line, Data),
  maplist(atom_number, Data, Numbers),
  assertMap(Index, 1, Numbers).

assertMap(_, _, []).
assertMap(X, Y, [H|T]) :- assert(height([X,Y],H)), Yn is Y+1, assertMap(X, Yn, T).
