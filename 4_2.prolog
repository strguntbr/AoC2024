day(4). testResult(9).

:- use_module(lib/solve).

xmasV([M,A,S|_]) :-
  M = ['M', _ ,'S'|_],
  A = [ _ ,'A'    |_],
  S = ['M', _ ,'S'|_].
xmasV([M,A,S|_]) :-
  M = ['S', _ ,'S'|_],
  A = [ _ ,'A'    |_],
  S = ['M', _ ,'M'|_].
xmasV([M,A,S|_]) :-
  M = ['S', _ ,'M'|_],
  A = [ _ ,'A'    |_],
  S = ['S', _ ,'M'|_].
xmasV([M,A,S|_]) :-
  M = ['M', _ ,'M'|_],
  A = [ _ ,'A'    |_],
  S = ['S', _ ,'S'|_].

xmasV([H|T]) :- 
    maplist([[_|Tv],Tv]>>true, [H|T], Puzzle),
    xmasV(Puzzle).

xmas(Puzzle) :- xmasV(Puzzle).
xmas([_|T]) :- xmas(T).

result(Puzzle, XMas) :-  aggregate_all(count, xmas(Puzzle), XMas).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
