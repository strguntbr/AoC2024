day(16). testResult(part1, "test", 7036). testResult(part1, "test-1", 11048). testResult(part2, "test", 45). testResult(part2, "test-1", 64).

:- use_module(lib/solve).

neighbor([[X,Y], north], 1, [[Xn,Y], north]) :- Xn is X-1, tile([[Xn,Y], north]).
neighbor([[X,Y], south], 1, [[Xn,Y], south]) :- Xn is X+1, tile([[Xn,Y], south]).
neighbor([[X,Y], west] , 1, [[X,Yn], west]) :- Yn is Y-1, tile([[X,Yn], west]).
neighbor([[X,Y], east] , 1, [[X,Yn], east]) :- Yn is Y+1, tile([[X,Yn], east]).
neighbor([Pos, Dir], 1001, [NextPos, NewDir]) :-
  member(Dir, [north, south]), member(NewDir, [east, west]),
  neighbor([Pos, NewDir], 1, [NextPos, NewDir]).
neighbor([Pos, Dir], 1001, [NextPos, NewDir]) :-
  member(Dir, [east, west]), member(NewDir, [north, south]),
  neighbor([Pos, NewDir], 1, [NextPos, NewDir]).

next(Pos, OldWeight, Weight, NextPos) :-
  neighbor(Pos, NextWeight, NextPos),
  tile(NextPos),
  \+ visited(NextPos),
  Weight is OldWeight + NextWeight.

insertSorted(Elem, [], [Elem]).
insertSorted([WeightA,PosA,PathA], [[WeightB,PosB,PathB]|Rest], [[WeightA,PosA,PathA]|RestN]) :-
  WeightA < WeightB, !, 
  (select([WeightA,_,_], [[WeightB,PosB,PathB]|Rest], RestN) -> true ; RestN=[[WeightB,PosB,PathB]|Rest]).
insertSorted([Weight,Pos,PathA], [[Weight,Pos,PathB]|Rest], [[Weight,Pos,AllPaths]|Rest]) :- !, append(PathA, PathB, Paths), sort(Paths, AllPaths).
insertSorted([WeightA,Pos,_], [[WeightB,Pos,PathB]|Rest], [[WeightB,Pos,PathB]|Rest]) :- WeightA > WeightB, !.
insertSorted(A, [B|Rest], [B|RestWithA]) :- insertSorted(A, Rest, RestWithA).

dijkstra([[Weight,End,Path]|_], End, Weight, [End|Path]) :- !, assert(visited(End)).
dijkstra([[Weight,[Pos,Dir],Path]|Queue], End, FinalWeight, FinalPath) :-
  assert(visited([Pos, Dir])),
  findall([W,[Pn,Dn],[Pos|Path]], next([Pos,Dir],Weight,W,[Pn,Dn]), NextTiles),
  foldl(insertSorted, NextTiles, Queue, NewQueue),
  dijkstra(NewQueue, End, FinalWeight, FinalPath).

resultPart1([Start, End], Weight) :- dijkstra([[0,[Start,east],[]]], [End,_], Weight, _).

resultPart2([Start, End], Result) :-
  dijkstra([[0,[Start,east],[]]], [End,_], _, Path),
  length(Path, Result).

/* required for loadData */
resetData :- retractall(tile(_)), retractall(visited(_)).
postProcessData(StartEnd, [StartPos, EndPos]) :- 
  member(Start, StartEnd), member(start(StartPos), Start),
  member(End, StartEnd), member(end(EndPos), End).

data_line(Index, StartEnd, Line) :- string_chars(Line, Chars), assertMaze(Index, 1, Chars, StartEnd).

assertMaze(_, _, [], []).
assertMaze(X, Y, [H|T], StartEnd) :- assertTile(X, Y, H, SE1), Yn is Y+1, assertMaze(X, Yn, T, SE2), append(SE1, SE2, StartEnd).
assertTile(X, Y, '.', []) :- assert(tile([[X,Y], south])), assert(tile([[X,Y], north])), assert(tile([[X,Y], west])), assert(tile([[X,Y], east])).
assertTile(X, Y, 'S', [start([X,Y])]) :- assertTile(X, Y, '.', _).
assertTile(X, Y, 'E', [end([X,Y])]) :- assertTile(X, Y, '.', _).
assertTile(_, _, '#', []).