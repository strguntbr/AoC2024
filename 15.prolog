day(15). groupData. testResult(part1, 10092). testResult(part2, 9021).

:- use_module(lib/solve).

/* Common methods */
next([X,Y], '<', [X,Yn]) :- Yn is Y-1.
next([X,Y], '>', [X,Yn]) :- Yn is Y+1.
next([X,Y], '^', [Xn,Y]) :- Xn is X-1.
next([X,Y], 'v', [Xn,Y]) :- Xn is X+1.

/* Part 1 */
finalBoxCoordinate1(Coordinate) :- box([X, Y]), Coordinate is X*100+Y.

moveBox(Pos, Dir) :- next(Pos, Dir, NextPos), wall(NextPos), !, fail.
moveBox(Pos, Dir) :- next(Pos, Dir, NextPos), box(NextPos), !,  (moveBox(NextPos, Dir) -> (retractall(box(Pos)), assert(box(NextPos)))).
moveBox(Pos, Dir) :- next(Pos, Dir, NextPos), (retractall(box(Pos)), assert(box(NextPos))).
  
moveRobot1(Pos, Dir, Pos) :- next(Pos, Dir, NextPos), wall(NextPos), !.
moveRobot1(Pos, Dir, NextRobot) :- next(Pos, Dir, NextPos), box(NextPos), !, (moveBox(NextPos, Dir) -> NextRobot=NextPos ; NextRobot=Pos).
moveRobot1(Pos, Dir, NextPos) :- next(Pos, Dir, NextPos).

processInstructions1(Pos, [], Pos).
processInstructions1(Pos, [Dir|OtherInstructions], FinalPos) :- moveRobot1(Pos, Dir, NextPos), processInstructions1(NextPos, OtherInstructions, FinalPos).

resultPart1([Robot, _, Instructions], CoordinateSum) :- 
  processInstructions1(Robot, Instructions, _),
  aggregate_all(sum(C), finalBoxCoordinate1(C), CoordinateSum).
  
/* Part 2 */
wall2([X, Y]) :- Y1 is div(Y,2), wall([X, Y1]).

finalBoxCoordinate2(Coordinate) :- boxPart(BoxId, [Bx,By]), boxPart(BoxId, [_,Oy]), By<Oy, Coordinate is Bx*100+By.

fullBox(BoxId, [Part1, Part2]) :- boxPart(BoxId, Part1), boxPart(BoxId, Part2), Part2 \= Part1.

moveParts(BoxParts, Dir, NextBoxParts) :- maplist([Part,Next]>>next(Part,Dir,Next), BoxParts, NextBoxParts).
fullBoxes(BoxParts, FullBoxParts) :- setof(P, [BoxPart,BoxId]^(member(BoxPart, BoxParts), boxPart(BoxId, BoxPart), boxPart(BoxId, P)), FullBoxParts), ! ; FullBoxParts=[].
findPartsToMove(BoxParts, Dir, AllPartsToMove) :-
  moveParts(BoxParts, Dir, MovedParts),
  not((member(W, MovedParts), wall2(W))),
  fullBoxes(MovedParts, MovedPartsBlocked),
  exclude([P]>>member(P, BoxParts), MovedPartsBlocked, MovedPartsBlockedByOtherBoxes),
  (MovedPartsBlockedByOtherBoxes = [] -> AllPartsToMove = BoxParts ; (findPartsToMove(MovedPartsBlockedByOtherBoxes, Dir, NewNextParts), append(BoxParts, NewNextParts, AllPartsToMove))).

moveFullBox(BoxPart, Dir) :-
  fullBox(_, [BoxPart, OtherBoxPart]),
  findPartsToMove([BoxPart, OtherBoxPart], Dir, PartsToMove),
  setof(BoxId, P^(member(P, PartsToMove), boxPart(BoxId, P)), AllBoxesToMove),
  moveBoxAssertions(AllBoxesToMove, Dir).

moveRobot2(Pos, Dir, Pos) :- next(Pos, Dir, NextPos), wall2(NextPos), !.
moveRobot2(Pos, Dir, NextRobot) :- next(Pos, Dir, NextPos), boxPart(_, NextPos), !, (moveFullBox(NextPos, Dir) -> NextRobot=NextPos ; NextRobot=Pos).
moveRobot2(Pos, Dir, NextPos) :- next(Pos, Dir, NextPos).

processInstructions2(Pos, [], Pos).
processInstructions2(Pos, [Dir|OtherInstructions], FinalPos) :- moveRobot2(Pos, Dir, NextPos), processInstructions2(NextPos, OtherInstructions, FinalPos).

moveBoxAssertions(Boxes, Dir) :-
  findall([B,Ps], (member(B, Boxes), fullBox(B, Ps)), ToMove),
  foreach(member([B,[P1,P2]], ToMove), moveBoxAssertion(B, P1, P2, Dir)).
moveBoxAssertion(BoxId, Part1, Part2, Dir) :-
  next(Part1, Dir, N1), next(Part2, Dir, N2),
  retractall(boxPart(BoxId, _)),
  assert(boxPart(BoxId, N1)), assert(boxPart(BoxId, N2)).  
  
resultPart2([_, Robot, Instructions], CoordinateSum) :- 
  processInstructions2(Robot, Instructions, _),
  aggregate_all(sum(C), finalBoxCoordinate2(C), CoordinateSum).

/* required for loadData */
resetData :- retractall(box(_)), retractall(boxPart(_,_)), retractall(wall(_)).

data_line(Chars, Line) :- string_chars(Line, Chars).
postProcessData([Map, InstructionList], [[RobotX, Robot1Y], [RobotX, Robot2Y], Instructions]) :- 
  assertMap(0, Map, [RobotX, Robot1Y]),
  Robot2Y is Robot1Y*2,
  append(InstructionList, Instructions).

assertPos(X, Y, '#', []) :- assert(wall([X, Y])), !.
assertPos(X, Y, 'O', []) :- Y1 is 2*Y, Y2 is Y1+1, assert(box([X,Y])), assert(boxPart([X, Y], [X, Y1])), assert(boxPart([X, Y], [X, Y2])), !.
assertPos(X, Y, '@', [X,Y]) :- !.
assertPos(_, _, _, []).
assertLine(_, _, [], []).
assertLine(X, Y, [H|T], Robot) :- assertPos(X, Y, H, ThisRobot), Yn is Y+1, assertLine(X, Yn, T, NextRobot), append(ThisRobot, NextRobot, Robot).
assertMap(_, [], []).
assertMap(X, [H|T], Robot) :- assertLine(X, 0, H, ThisRobot), Xn is X+1, assertMap(Xn, T, NextRobot), append(ThisRobot, NextRobot, Robot).
