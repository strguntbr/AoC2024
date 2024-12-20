day(16). testResult(part1, "test", 7036). testResult(part1, "test-1", 11048). testResult(part2, "test", 45). testResult(part2, "test-1", 64).

:- use_module(lib/dijkstra), use_module(lib/solve).

edge([[X,Y], north], 1, [[Xn,Y], north]) :- Xn is X-1, tile([[Xn,Y], north]).
edge([[X,Y], south], 1, [[Xn,Y], south]) :- Xn is X+1, tile([[Xn,Y], south]).
edge([[X,Y], west] , 1, [[X,Yn], west]) :- Yn is Y-1, tile([[X,Yn], west]).
edge([[X,Y], east] , 1, [[X,Yn], east]) :- Yn is Y+1, tile([[X,Yn], east]).
edge([Pos, Dir], 1001, [NextPos, NewDir]) :-
  member(Dir, [north, south]), member(NewDir, [east, west]),
  edge([Pos, NewDir], 1, [NextPos, NewDir]).
edge([Pos, Dir], 1001, [NextPos, NewDir]) :-
  member(Dir, [east, west]), member(NewDir, [north, south]),
  edge([Pos, NewDir], 1, [NextPos, NewDir]).

resultPart1([Start, End], Weight) :- dijkstra(edge, [Start,east], [End,_], Weight).

resultPart2([Start, End], Result) :-
  dijkstra(edge, allVerticesOnShortestPaths, [Start,east], [End,_], _, Path),
  setof(Tile, Dir^member([Tile,Dir],Path), Tiles), length(Tiles, Result).

/* required for loadData */
postProcessData(StartEnd, [StartPos, EndPos]) :- append(StartEnd, StartEndAll), member(start(StartPos), StartEndAll), member(end(EndPos), StartEndAll).

data_line(Index, StartEnd, Line) :- string_chars(Line, Chars), assertMaze(Index, 1, Chars, StartEnd).

assertMaze(_, _, [], []).
assertMaze(X, Y, [H|T], StartEnd) :- assertTile(X, Y, H, SE1), Yn is Y+1, assertMaze(X, Yn, T, SE2), append(SE1, SE2, StartEnd).
assertTile(X, Y, '.', []) :- assert(tile([[X,Y], south])), assert(tile([[X,Y], north])), assert(tile([[X,Y], west])), assert(tile([[X,Y], east])).
assertTile(X, Y, 'S', [start([X,Y])]) :- assertTile(X, Y, '.', _).
assertTile(X, Y, 'E', [end([X,Y])]) :- assertTile(X, Y, '.', _).
assertTile(_, _, '#', []).
