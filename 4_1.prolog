day(4). testResult(18).

:- use_module(lib/solve).

xmasV([XMAS|_]) :- XMAS = ['X','M','A','S'|_].
xmasV([SAMX|_]) :- SAMX = ['S','A','M','X'|_].
xmasV([X,M,A,S|_]) :- X = ['X'|_], M = ['M'|_], A = ['A'|_], S = ['S'|_].
xmasV([S,A,M,X|_]) :- X = ['X'|_], M = ['M'|_], A = ['A'|_], S = ['S'|_].
xmasV([X,M,A,S|_]) :- X = ['X'|_], M = [_,'M'|_], A = [_,_,'A'|_], S = [_,_,_,'S'|_].
xmasV([S,A,M,X|_]) :- X = ['X'|_], M = [_,'M'|_], A = [_,_,'A'|_], S = [_,_,_,'S'|_].
xmasV([X,M,A,S|_]) :- X = [_,_,_,'X'|_], M = [_,_,'M'|_], A = [_,'A'|_], S = ['S'|_].
xmasV([S,A,M,X|_]) :- X = [_,_,_,'X'|_], M = [_,_,'M'|_], A = [_,'A'|_], S = ['S'|_].

xmasV([H|T]) :- 
  maplist([[_|Tv],Tv]>>true, [H|T], Puzzle),
  xmasV(Puzzle).

xmas(Puzzle) :- xmasV(Puzzle).
xmas([_|T]) :- xmas(T).

result(Puzzle, XMas) :-  aggregate_all(count, xmas(Puzzle), XMas).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
