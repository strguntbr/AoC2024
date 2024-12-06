day(5). testResult(143). groupData.

:- use_module(lib/solve).

center(L, M) :- length(L, Len), I is div(Len, 2), nth0(I, L, M).

unordered([H|T]) :- member(First, T), before(First, H), !.
unordered([_|T]) :- unordered(T).

result([_, Pages], Sum) :-
  findall(P, (member(P, Pages), not(unordered(P))), OrderedPages),
  maplist(center, OrderedPages, CenterPages),
  sumlist(CenterPages, Sum).

/* required for loadData */
resetData :- retractall(before(_,_)).

data_line([], Line) :-
  split_string(Line, "|", "", [First,Second]), !,
  maplist(number_string, [FirstPage, SecondPage], [First,Second]),
  assert(before(FirstPage, SecondPage)).
data_line(PageNumbers, Line) :-
  split_string(Line, ",", "", Pages),
  maplist(number_string, PageNumbers, Pages).
