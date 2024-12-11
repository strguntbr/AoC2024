day(11). testResult(part1, 55312).

:- use_module(lib/solve).

splitNumber(Number, SplitNumbers) :-
  number_string(Number, String), string_length(String, Length), 0 is mod(Length, 2),
  L is Length / 2, sub_string(String, 0, L, _, String1), sub_string(String, _, L, 0, String2),
  maplist(number_string, SplitNumbers, [String1, String2]).

blink([], []).
blink([stone{number: 0, count: C}|T], [stone{number: 1, count: C}|Tn]) :- !, blink(T, Tn).
blink([stone{number: Stone, count: C}|T], [stone{number: Stone1, count: C},stone{number: Stone2, count: C}|Tn]) :-
  splitNumber(Stone, [Stone1, Stone2]),
  blink(T, Tn).
blink([stone{number: Stone, count: C}|T], [stone{number: NextStone, count: C}|Tn]) :- NextStone is Stone * 2024, blink(T, Tn).

blinkNTimes(0, Stones, StoneCount) :- !, mapsum(Stones, [stone{number:_, count: C},C]>>true, StoneCount).
blinkNTimes(Count, Stones, StoneCount) :-
  NextCount is Count - 1,
  blink(Stones, NextStones), compact(NextStones, RecompactedStones),
  blinkNTimes(NextCount, RecompactedStones, StoneCount).

compact([], []).
compact([stone{number: Stone, count: Count}|OtherStones], [stone{number: Stone, count: Sum}|OtherCompactedStones]) :-
  partition([stone{number:Stone, count: _}]>>true, OtherStones, Hs, RemainingStones),
  foldl([stone{number: _, count: Tsc},Tc,R]>>(R is Tc+Tsc), Hs, Count, Sum),
  compact(RemainingStones, OtherCompactedStones).

resultPart1([Stones], Count) :- blinkNTimes(25, Stones, Count).
resultPart2([Stones], Count) :- blinkNTimes(75, Stones, Count).

/* required for loadData */
data_line(Stones, Line) :- split_string(Line, " ", " ", Data), maplist(string_stone, Data, Stones).
string_stone(String, stone{number: Number, count: 1}) :- number_string(Number, String).
