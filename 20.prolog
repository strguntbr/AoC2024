day(20). auxData(100). testResult(part1, auxData(1), 44). testResult(part2, auxData(50), 285).

:- use_module(lib/solve).

/* Part 1 */
cheat([Xs,Y], [Xe,Y], TimeSaved) :-
  trackPosition([Xs,Y], D1), (Xe is Xs+2 ; Xe is Xs-2),
  trackPosition([Xe,Y], D2), D1 < D2,
  (Xw is (Xs+Xe) div 2), wall([Xw,Y]),
  TimeSaved is D2 - D1 - 1.
cheat([X,Ys], [X,Ye], TimeSaved) :-
  trackPosition([X,Ys], D1), (Ye is Ys+2 ; Ye is Ys-2),
  trackPosition([X,Ye], D2), D1 < D2,
  (Yw is (Ys+Ye) div 2), wall([X,Yw]),
  TimeSaved is D2 - D1 - 1.

resultPart1(_, Shortcut, Result) :- aggregate_all(count, [S,E], (cheat(S,E,TimeSaved), TimeSaved>=Shortcut), Result).

/* Part 2 */
/* Of course part 1 would also work with this generic cheat predicate, but the specialized on is much faster */
cheat([Xs,Ys], MaxCheatLength, [Xe,Ye], TimeSaved) :-
  MaxCheatLength > 2,
  trackPosition([Xs,Ys], D1),
  trackPosition([Xe,Ye], D2), D1 < D2,
  L is abs(Xs-Xe) + abs(Ys-Ye), L =< MaxCheatLength,
  TimeSaved is D2 - D1 - L.

resultPart2(_, Shortcut, Result) :- aggregate_all(count, [S,E], (cheat(S,20,E,TimeSaved), TimeSaved>=Shortcut), Result).

/* required for loadData */
resetData :- retractall(wall(_)), retractall(track(_)), retractall(track(_,_)).
postProcessData(Data, [Start,End]) :- 
  append(Data, StartEnd), member(start(Start), StartEnd), member(end(End), StartEnd),
  calculateTrackPositions(Start, 0, End).

data_line(Index, StartEnd, Line) :- string_chars(Line, Chars), assertTrack(Index, 1, Chars, StartEnd).
assertTrack(_, _, [], []).
assertTrack(X, Y, [H|T], StartEnd) :- assertPos(X, Y, H, CurStartEnd), Yn is Y+1, assertTrack(X, Yn, T, NextStartEnd), append(CurStartEnd, NextStartEnd, StartEnd).
assertPos(X, Y, '#', []) :- assert(wall([X, Y])).
assertPos(X, Y, '.', []) :- assert(track([X, Y])).
assertPos(X, Y, 'S', [start([X,Y])]) :- assertPos(X, Y, '.', _).
assertPos(X, Y, 'E', [end([X,Y])]) :- assertPos(X, Y, '.', _).

neighbor([X,Y], 1, [Xn, Y]) :- (Xn is X-1 ; Xn is X+1), track([Xn,Y]).
neighbor([X,Y], 1, [X, Yn]) :- (Yn is Y-1 ; Yn is Y+1), track([X,Yn]).
calculateTrackPositions(Cur, Position, End) :-
  retractall(track(Cur)),
  assert(trackPosition(Cur, Position)),
  NextPosition is Position+1,
  (Cur = End -> true ; (neighbor(Cur, 1, Next), calculateTrackPositions(Next, NextPosition, End))).
