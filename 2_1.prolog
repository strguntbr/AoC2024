day(2). testResult(2).

:- use_module(lib/solve).

isSafeInc([_]).
isSafeInc([E1|[E2|R]]) :- E1 < E2, E2 - E1 < 4, isSafeInc([E2|R]).
isSafeDec([_]).
isSafeDec([E1|[E2|R]]) :- E1 > E2, E1 - E2 < 4, isSafeDec([E2|R]).

isSafe(Levels) :- isSafeInc(Levels) ; isSafeDec(Levels).

countSafe([], 0).
countSafe([H|T], C) :- isSafe(H), !, countSafe(T, Cn), C is Cn + 1.
countSafe([_|T], C) :- !, countSafe(T, C).

result(Levels, Safe) :- countSafe(Levels, Safe).

/* required for loadData */
data_line(Levels, Line) :- split_string(Line, " ", " ", Data), maplist(number_string, Levels, Data).
