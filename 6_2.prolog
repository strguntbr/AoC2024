day(6). testResult(6).

:- use_module(lib/solve).

turn([1,0], [0,-1]).
turn([0,-1], [-1,0]).
turn([-1,0], [0,1]).
turn([0,1], [1,0]).

lab_(Pos, Place) :- obstacle(Pos) -> Place = '#' ; lab(Pos, Place).

next([X, Y], [Xd, Yd], [Xn, Yn], Place) :- Xn is X+Xd, Yn is Y+Yd, lab_([Xn,Yn], Place).

move([Pos, Direction], [NextPos, Direction]) :- next(Pos, Direction, NextPos, '.'), !.
move([Pos, Direction], [NextPos, NextDirection]) :- next(Pos, Direction, _, '#'), turn(Direction, IntemateDirection), move([Pos, IntemateDirection], [NextPos, NextDirection]).

isLoop(Pos, Direction) :- visited(Pos, Direction).
isLoop(Pos, Direction) :- tempVisited(Pos, Direction).
isLoop(Pos, Direction) :-
  assert(tempVisited(Pos, Direction)),
  move([Pos, Direction], [NextPos, NextDirection]),
  isLoop(NextPos, NextDirection).

searchLoop(Pos, Direction, ObstaclePos) :-
  not(visited(ObstaclePos, _)),
  retractall(obstacle(_)),
  retractall(tempVisited(_, _)),
  assert(obstacle(ObstaclePos)),
  turn(Direction, NextDirection),
  isLoop(Pos, NextDirection).

go(Pos, Direction, Loops) :-
  assert(visited(Pos, Direction)),
  next(Pos, Direction, NextPos, '.'), !,
  (searchLoop(Pos, Direction, NextPos) -> (go(NextPos, Direction, NextLoops), Loops = [NextPos|NextLoops]) ; go(NextPos, Direction, Loops)).
go(Pos, Direction, Loops) :-
  next(Pos, Direction, _, '#'), !,
  turn(Direction, NextDirection),
  go(Pos, NextDirection, Loops).
go(_, _, []).

result(Data, Count) :-
  append(_, [[Start,Direction]|_], Data),
  go(Start, Direction, Loops),
  length(Loops, Count).

/* required for loadData */
resetData :- retractall(lab(_,_)), retractall(visited(_,_)), retractall(obstacle(_)), retractall(tempVisited(_,_)).

data_line(Index, Start, Line) :- string_chars(Line, Data), assertLab(Index, 1, Data, Start).

assertLab(_, _, [], []).
assertLab(X, Y, ['^'|T], [[X,Y],[-1,0]]) :- !, assertLab(X, Y, ['.'|T], _).
assertLab(X, Y, [H|T], Start) :- assert(lab([X,Y],H)), Yn is Y+1, assertLab(X, Yn, T, Start).
