day(2). testResult(part1, 2). testResult(part2, 4).

:- use_module(lib/solve).

isSafeInc([_]).
isSafeInc([E1|[E2|R]]) :- E1 < E2, E2 - E1 < 4, isSafeInc([E2|R]).
isSafeDec([_]).
isSafeDec([E1|[E2|R]]) :- E1 > E2, E1 - E2 < 4, isSafeDec([E2|R]).

isSafe(Levels, RemoveMax) :- removeMax(Levels, RemoveMax, Levels2), (isSafeInc(Levels2) ; isSafeDec(Levels2)).

removeMax([], _, []).
removeMax([_|T], C, Tn) :- C > 0, Cn is C-1, removeMax(T, Cn, Tn).
removeMax([H|T], C, [H|Tn]) :- removeMax(T, C, Tn).

resultPart1(LevelsList, Count) :- aggregate_all(count, L, (member(L,LevelsList), isSafe(L,0)), Count).
resultPart2(LevelsList, Count) :- aggregate_all(count, L, (member(L,LevelsList), isSafe(L,1)), Count).

/* required for loadData */
data_line(Levels, Line) :- split_string(Line, " ", " ", Data), maplist(number_string, Levels, Data).
