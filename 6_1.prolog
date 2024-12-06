day(6). testResult(41).

:- use_module(lib/solve).

turn([1,0], [0,-1]).
turn([0,-1], [-1,0]).
turn([-1,0], [0,1]).
turn([0,1], [1,0]).

next([X, Y], [Xd, Yd], [Xn, Yn], Place) :- Xn is X+Xd, Yn is Y+Yd, lab([Xn,Yn], Place).

go(Pos, Direction, [Pos|Path]) :-
  next(Pos, Direction, [X, Y], '.'), !,
  go([X, Y], Direction, Path).
go(Pos, Direction, Path) :-
  next(Pos, Direction, _, '#'), !,
  turn(Direction, NextDirection),
  go(Pos, NextDirection, Path).
go(Pos, _, [Pos]).

result(_, Count) :-
  start(Start, Direction),
  go(Start, Direction, Path),
  sort(Path, SortedPath), length(SortedPath, Count).

/* required for loadData */
resetData :- retractall(lab(_,_)), retractall(start(_,_)).

data_line(Index, Data, Line) :- string_chars(Line, Data), assertLab(Index, 1, Data).

assertLab(_, _, []).
assertLab(X, Y, ['^'|T]) :- !, assert(start([X, Y], [-1,0])), assertLab(X, Y, ['.'|T]).
assertLab(X, Y, [H|T]) :- assert(lab([X,Y],H)), Yn is Y+1, assertLab(X, Yn, T).
