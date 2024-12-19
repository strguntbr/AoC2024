day(19). groupData. testResult(part1, 6). testResult(part2, 16).

:- use_module(lib/solve).

prefix(Towels, Prefix, Logo, Rest) :- member(Prefix, Towels), append(Prefix, Rest, Logo).

/* Part 1 */
logo(_, []) :- !.
logo(Towels, Logo) :- \+ impossible(Logo), prefix(Towels, _, Logo, Rest), logo(Towels, Rest), !.
logo(_, Logo) :- assert(impossible(Logo)), fail.

resultPart1([[Towels], Logos], Result) :- aggregate_all(count, Logo, (member(Logo, Logos), logo(Towels, Logo)), Result).

/* Part 2 */
countArrangements(_, [], 1) :- !.
countArrangements(_, Logo, Arrangements) :- arrangements(Logo, Arrangements), !.
countArrangements(Towels, Logo, Arrangements) :-
  aggregate_all(sum(A), (prefix(Towels, _, Logo, Rest), countArrangements(Towels, Rest, A)), Arrangements),
  assert(arrangements(Logo, Arrangements)).

resultPart2([[Towels], Logos], Result) :- mapsum(Logos, [L,A]>>countArrangements(Towels, L, A), Result).

/* Alternativ solution to part1, that does not require dedicated predicates for part1, but is way slower */
resultPart1_alternative([[Towels], Logos], Result) :- aggregate_all(count, (member(Logo, Logos), \+ countArrangements(Towels, Logo, 0)), Result).

/* required for loadData */
resetData :- retractall(impossible(_)), retractall(arrangements(_,_)).

data_line(Logo, Line) :- split_string(Line, ",", " ", [LogoString]), !, string_chars(LogoString, Logo).
data_line(Towels, Line) :- split_string(Line, ",", " ", TowelStrings), maplist(string_chars, TowelStrings, Towels).