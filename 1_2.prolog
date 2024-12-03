day(1). testResult(31).

:- use_module(lib/solve).

combine([], [], []).
combine([[First, Second]|R], [First|FirstR], [Second|SecondR]) :- combine(R, FirstR, SecondR).

sumX(_, [], 0) :- !.
sumX(X, [X|R], Sum) :- !, sumX(X, R, SumN), Sum is SumN + X.
sumX(X, [_|R], Sum) :- !, sumX(X, R, Sum).

result(Ids, Sum) :-
  combine(Ids, Firsts, Seconds),
  maplist([X, Sum]>>sumX(X, Seconds, Sum), Firsts, Sums),
  sumlist(Sums, Sum).

/* required for loadData */
data_line([First, Second], Line) :- split_string(Line, " ", " ", Data), maplist(number_string, [First, Second], Data).
