day(14). auxData([101,103]). testResult(part1, auxData([11,7]), 12).

:- use_module(lib/solve).

move([[Px, Py], [Vx, Vy]], Seconds, [Sx, Sy], [Rx, Ry]) :-
  Rx is mod(Px + Vx * Seconds, Sx),
  Ry is mod(Py + Vy * Seconds, Sy),
  assert(pos(Rx, Ry)).
moveAll(Robots, Seconds, Size, Positions) :- retractall(pos(_,_)), maplist([R,N]>>move(R,Seconds,Size,N), Robots, Positions).

/* Part 1 */
lowerUpper(A, B, lower) :- A < B.
lowerUpper(A, B, upper) :- A > B.
quadrant([PosX, PosY], [SizeX, SizeY], [Qx, Qy]) :-
  MiddleX is (SizeX-1) / 2, MiddleY is (SizeY-1) / 2,
  lowerUpper(MiddleX, PosX, Qx), lowerUpper(MiddleY, PosY, Qy).
robotsInQuadrant(Quadrant, Size, Count) :-
  member(Quadrant, [[lower,lower],[lower,upper],[upper,lower],[upper,upper]]),
  findall([X,Y], (pos(X,Y), quadrant([X,Y], Size, Quadrant)), RobotsInQuadrant),
  length(RobotsInQuadrant, Count).

resultPart1(Robots, BathroomSize, SafetyFactor) :- 
  moveAll(Robots, 100, BathroomSize, _),
  findall(C, robotsInQuadrant(_, BathroomSize, C), QuadrantCounts),
  productlist(QuadrantCounts, SafetyFactor).
  
/* Part 2 */
border([Ax,Ay], [Bx,By]) :-
  foreach(between(Ax, Bx, X), (pos(X,Ay), pos(X,By))),
  foreach(between(Ay, By, Y), (pos(Ax,Y), pos(Bx,Y))).
containsImage :-
  /* Assume the image is at least 25x25 pixels and has a solid border */
  pos(Ax, Ay), pos(Bx, By),
  Bx2 is Bx-20, By2 is By-20, Ax =< Bx2, Ay =< By2,
  pos(Ax,By), pos(Bx,Ay),
  border([Ax,Ay], [Bx,By]).
hasImageAfter(Robots, BathroomSize, Seconds) :- moveAll(Robots, Seconds, BathroomSize, _), containsImage.

resultPart2(Robots, BathroomSize, Seconds) :- between(0, infinite, Seconds), hasImageAfter(Robots, BathroomSize, Seconds).

/* required for loadData */
data_line([[Px, Py], [Vx, Vy]], Line) :- split_string(Line, ", ", "pv=", Parts), maplist(number_string, [Px, Py, Vx, Vy], Parts), !.
