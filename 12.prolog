day(12).

testResult(part1, "test-1", 140).
testResult(part1, "test-2", 772).
testResult(part1, "test", 1930).

testResult(part2, "test-1", 80).
testResult(part2, "test-2", 436).
testResult(part2, "test-3", 236).
testResult(part2, "test-4", 368).
testResult(part2, "test", 1206).

:- use_module(lib/solve).

neighbor([X1,Y1], [X2,Y1], top) :- X2 is X1-1.
neighbor([X1,Y1], [X2,Y1], bottom) :- X2 is X1+1.
neighbor([X1,Y1], [X1,Y2], left) :- Y2 is Y1-1.
neighbor([X1,Y1], [X1,Y2], right) :- Y2 is Y1+1.

visitPlant(Pos, Type) :- plant(Pos, Type), \+ visited(Pos), assert(visited(Pos)).

typedNeighbor(Pos, Type, Neighbor) :- neighbor(Pos, Neighbor, _), visitPlant(Neighbor, Type).
typedNeighbors(Pos, Type, Neighbors) :-
  findall(N, typedNeighbor(Pos, Type, N), DirectNeighbors),
  maplist([DN,TN]>>typedNeighbors(DN,Type,TN), DirectNeighbors, TransitiveNeighbors),
  append(TransitiveNeighbors, AllTransitiveNeighbors),
  append(AllTransitiveNeighbors, DirectNeighbors, Neighbors).

region(Plant, [Pos|Neighbors]) :- visitPlant(Pos, Plant), typedNeighbors(Pos, Plant, Neighbors).

fence(Pos, Neighbor, Direction) :- plant(Pos, Type), neighbor(Pos, Neighbor, Direction), \+ plant(Neighbor, Type). 

perimeter(Region, Discount, Perimeter) :-
  maplist([R,F]>>findall([N,D],fence(R,N,D),F), Region, Fences),
  append(Fences, AllFences),
  call(Discount, AllFences, DiscountedFences),
  length(DiscountedFences, Perimeter).

sideDiscount([], []) :- !.
sideDiscount([FirstFence|OtherFences], [FirstFence|OtherSides]) :-
  removeSameSide(FirstFence, OtherFences, IntermediateFences),
  removeSameSide(FirstFence, IntermediateFences, RemainingFences),
  sideDiscount(RemainingFences, OtherSides).
removeSameSide([Pos, Dir], OtherFences, RemainingFences) :-
  select([Neighbor, Dir], OtherFences, NextFences),
  neighboringFence(Pos, Dir, Neighbor), !,
  removeSameSide([Neighbor, Dir], NextFences, RemainingFences).
removeSameSide(_, Fences, Fences).
turn(Dir1, Dir2) :- Map=[[left,top],[left,bottom],[right,top],[right,bottom]], (member([Dir1, Dir2], Map) ; member([Dir2, Dir1], Map)).
neighboringFence(Pos, Dir, Neighbor) :- turn(Dir, TurnedDir), neighbor(Pos, Neighbor, TurnedDir).


price(Region, Price) :- perimeter(Region, [Fence,Fence]>>true, Perimeter), length(Region, Area), Price is Perimeter * Area.
discountedPrice(Region, Price) :- perimeter(Region, sideDiscount, Perimeter), length(Region, Area), Price is Perimeter * Area.

resultPart1(_, Perimeters) :- findall(Region, region(_, Region), Regions), mapsum(Regions, price, Perimeters).
resultPart2(_, Perimeters) :- findall(Region, region(_, Region), Regions), mapsum(Regions, discountedPrice, Perimeters).

/* required for loadData */
initPredicates :- dynamic(plant/2), dynamic(visited/1).

data_line(Index, Plants, Line) :- string_chars(Line, Plants), assertGarden(Index, 1, Plants).

assertGarden(_, _, []).
assertGarden(X, Y, [Type|T]) :- assert(plant([X,Y],Type)), Yn is Y+1, assertGarden(X, Yn, T).
