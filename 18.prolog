day(18). auxData([70,1024]). testResult(part1, auxData([6,12]), 22). testResult(part2, auxData([6,12]), "6,1").

:- use_module(lib/dijkstra), use_module(lib/solve).

neighbor([X,Y], [Xn, Y]) :- (Xn is X-1 ; Xn is X+1), Xn >= 0, size(Max), Xn =< Max.
neighbor([X,Y], [X, Yn]) :- (Yn is Y-1 ; Yn is Y+1), Yn >= 0, size(Max), Yn =< Max.

edge(Pos, 1, NextPos) :- neighbor(Pos, NextPos), duration(MaxDuration), \+ (corruptMemory(Duration, NextPos), Duration =< MaxDuration).

resultPart1(_, [Size,Duration], Weight) :-
  assert(size(Size)),
  assert(duration(Duration)),
  dijkstra(edge, [0,0], [Size,Size], Weight).

noPathAfter(MaxDuration, Size, Duration) :-
  between(0, MaxDuration, Duration),
  retractall(duration(_)), assert(duration(Duration)),
  \+ dijkstra(edge, [0,0], [Size,Size], _).

resultPart2(Data, [Size,__], Result) :-
  assert(size(Size)),
  length(Data, MaxDuration),
  noPathAfter(MaxDuration, Size, Duration),
  corruptMemory(Duration, Memory),
  maplist(number_string, Memory, [X,Y]),
  string_concat(X, ",", XSep), string_concat(XSep, Y, Result).

/* required for loadData */
resetData :- retractall(size(_)), retractall(duration(_)), retractall(corruptMemory(_,_)).

data_line(Time, [X,Y], Line) :-
  split_string(Line, ",", "", NumberStrings),
  maplist(number_string, [X,Y], NumberStrings),
  assert(corruptMemory(Time, [X,Y])).
