day(5). testResult(part1, 143). testResult(part2, 123). groupData.

:- use_module(lib/solve).

center(L, M) :- length(L, Len), I is div(Len, 2), nth0(I, L, M).

unordered([H|T]) :- member(First, T), before(First, H), !.
unordered([_|T]) :- unordered(T).

ordered(L) :- \+ unordered(L).

first([H|T], FirstT, [H|OtherT]) :- first(T, FirstT, OtherT), not(before(H, FirstT)), !.
first([H|T], H, T).

order([Page], [Page]) :- !.
order(UnorderedPages, [FirstPage|OrderedPages]) :- first(UnorderedPages, FirstPage, OtherPages), order(OtherPages, OrderedPages).

orderedCenter(Pages, Center) :- order(Pages, OrderedPages), center(OrderedPages, Center).

resultPart1([_, Pages], Sum) :- include(ordered, Pages, UnorderedPages), mapsum(UnorderedPages, center, Sum).
resultPart2([_, Pages], Sum) :- include(unordered, Pages, UnorderedPages), mapsum(UnorderedPages, orderedCenter, Sum).

/* required for loadData */
resetData :- retractall(before(_,_)).

data_line([], Line) :-
  split_string(Line, "|", "", [First,Second]), !,
  maplist(number_string, [FirstPage, SecondPage], [First,Second]),
  assert(before(FirstPage, SecondPage)).
data_line(PageNumbers, Line) :-
  split_string(Line, ",", "", Pages),
  maplist(number_string, PageNumbers, Pages).
