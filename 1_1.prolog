day(1). testResult(11).

:- use_module(lib/solve).

distance([First, Second], Distance) :- Distance is abs(First - Second).

combine([], [], []).
combine([[First, Second]|R], [First|FirstR], [Second|SecondR]) :- combine(R, FirstR, SecondR).

result(Ids, DistanceSum) :-
  combine(Ids, Firsts, Seconds),
  maplist(msort, [Firsts, Seconds], [FirstsSorted, SecondsSorted]),
  combine(SortedPairs, FirstsSorted, SecondsSorted),
  maplist(distance, SortedPairs, Distances),
  sumlist(Distances, DistanceSum).

/* required for loadData */
data_line([First, Second], Line) :- split_string(Line, " ", " ", Data), maplist(number_string, [First, Second], Data).
