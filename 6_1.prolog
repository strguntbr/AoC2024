day(6). testResult(41).

:- use_module(lib/solve).

turn([1,0], [0,-1]).
turn([0,-1], [-1,0]).
turn([-1,0], [0,1]).
turn([0,1], [1,0]).

next([X, Y], [Xd, Yd], [Xn, Yn], Place) :- Xn is X+Xd, Yn is Y+Yd, lab([Xn,Yn], Place).

move([Pos, Direction], [NextPos, Direction]) :- next(Pos, Direction, NextPos, '.'), !.
move([Pos, Direction], [NextPos, NextDirection]) :- next(Pos, Direction, _, '#'), turn(Direction, IntemateDirection), move([Pos, IntemateDirection], [NextPos, NextDirection]).

go(Pos, Direction, [Pos|Path]) :-
  move([Pos, Direction], [NextPos, NextDirection]), !,
  go(NextPos, NextDirection, Path).
go(Pos, _, [Pos]).

result(Data, Count) :-
  append(_, [[Start,Direction]|_], Data),
  go(Start, Direction, Path),
  sort(Path, SortedPath), length(SortedPath, Count).

/* required for loadData */
resetData :- retractall(lab(_,_)).

data_line(Index, Start, Line) :- string_chars(Line, Data), assertLab(Index, 1, Data, Start).

assertLab(_, _, [], []).
assertLab(X, Y, ['^'|T], [[X,Y],[-1,0]]) :- !, assertLab(X, Y, ['.'|T], _).
assertLab(X, Y, [H|T], Start) :- assert(lab([X,Y],H)), Yn is Y+1, assertLab(X, Yn, T, Start).
