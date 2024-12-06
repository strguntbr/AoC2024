day(6). testResult(6).

:- use_module(lib/solve).

turn([1,0], [0,-1]).
turn([0,-1], [-1,0]).
turn([-1,0], [0,1]).
turn([0,1], [1,0]).

lab_(Pos, Place) :- obstacle(Pos) -> Place = '#' ; lab(Pos, Place).

next([X, Y], [Xd, Yd], [Xn, Yn], Place) :- Xn is X+Xd, Yn is Y+Yd, lab_([Xn,Yn], Place).

isLoop(Pos, Direction) :- visited(Pos, Direction).
isLoop(Pos, Direction) :- tempVisited(Pos, Direction).
isLoop(Pos, Direction) :-
  assert(tempVisited(Pos, Direction)),
  next(Pos, Direction, NextPos, '.'), !,
  isLoop(NextPos, Direction).
isLoop(Pos, Direction) :-
  next(Pos, Direction, _, '#'),
  turn(Direction, NextDirection),
  isLoop(Pos, NextDirection).

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
  (searchLoop(Pos, Direction, NextPos) -> (
    go(NextPos, Direction, NextLoops), Loops = [NextPos|NextLoops]
  ) ; (
    go(NextPos, Direction, Loops)
  )).
go(Pos, Direction, Loops) :-
  next(Pos, Direction, _, '#'), !,
  turn(Direction, NextDirection),
  go(Pos, NextDirection, Loops).
go(_, _, []).

result(_, Count) :-
  start(Start, Direction),
  go(Start, Direction, Loops),
  length(Loops, Count).

/* required for loadData */
resetData :- retractall(lab(_,_)), retractall(visited(_,_)), retractall(start(_,_)), retractall(obstacle(_)).

data_line(Index, Data, Line) :- string_chars(Line, Data), assertLab(Index, 1, Data).

assertLab(_, _, []).
assertLab(X, Y, ['^'|T]) :- !, assert(start([X, Y], [-1,0])), assertLab(X, Y, ['.'|T]).
assertLab(X, Y, [H|T]) :- assert(lab([X,Y],H)), Yn is Y+1, assertLab(X, Yn, T).
