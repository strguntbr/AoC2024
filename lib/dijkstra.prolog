:- module(dijkstra, [
  dijkstra/4,
  dijkstra/6,
  allVerticesOnShortestPaths/3,
  firstPath/3,
  lastPath/3
]).

dijkstra(EdgeGenerator, MergePathGoal, Start, End, Weight, Path) :-
  retractall(visited(_)),
  dijkstra_(EdgeGenerator, MergePathGoal, [[0,Start,[]]], End, Weight, Path).
dijkstra(EdgeGenerator, Start, End, Weight) :- dijkstra(EdgeGenerator, [_,_,_]>>true, Start, End, Weight, _).

dijkstra_(_, _, [[Weight,End,Path]|_], End, Weight, [End|Path]) :- !.
dijkstra_(EdgeGenerator, MergePathGoal, [[Weight,Vertex,Path]|Queue], End, FinalWeight, FinalPath) :-
  assert(visited(Vertex)),
  findall([W,[Pn,Dn],[Vertex|Path]], next(Vertex,Weight,EdgeGenerator,W,[Pn,Dn]), NextVertices),
  foldl([V,Q,N]>>insertSorted(V,Q,MergePathGoal,N), NextVertices, Queue, NewQueue),
  dijkstra_(EdgeGenerator, MergePathGoal, NewQueue, End, FinalWeight, FinalPath).

insertSorted(Elem, [], _, [Elem]).
insertSorted([WeightA,VertexA,PathA], [[WeightB,VertexB,PathB]|Rest], _, [[WeightA,VertexA,PathA]|RestN]) :-
  WeightA < WeightB, !, 
  (select([WeightA,_,_], [[WeightB,VertexB,PathB]|Rest], RestN) -> true ; RestN=[[WeightB,VertexB,PathB]|Rest]).
insertSorted([Weight,Vertex,PathA], [[Weight,Vertex,PathB]|Rest], MergeGoal, [[Weight,Vertex,MergedPath]|Rest]) :- !, call(MergeGoal, PathA, PathB, MergedPath).
insertSorted([WeightA,Vertex,_], [[WeightB,Vertex,PathB]|Rest], _, [[WeightB,Vertex,PathB]|Rest]) :- WeightA > WeightB, !.
insertSorted(A, [B|Rest], MergeGoal, [B|RestWithA]) :- insertSorted(A, Rest, MergeGoal, RestWithA).

next(Vertex, OldWeight, EdgeGenerator, Weight, NextVertex) :-
  call(EdgeGenerator, Vertex, NextWeight, NextVertex),
  \+ visited(NextVertex),
  Weight is OldWeight + NextWeight.

allVerticesOnShortestPaths(PathA, PathB, MergedPath) :- append(PathA, PathB, D), sort(D, MergedPath).
firstPath(_ , PathB, PathB).
lastPath(PathA , _, PathA).
