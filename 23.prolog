day(23). testResult(part1, 7). testResult(part2, "co,de,ka,ta").

:- use_module(lib/solve).

connected(C1, C2) :- connection(C1, C2), !.
connected(C1, C2) :- connection(C2, C1).

interConnectedSet_(_, SetSoFar, Set) :- \+ append(SetSoFar, _, Set), !, fail. /* fail fast for part 1 */
interConnectedSet_(Computers, SetSoFar, Set) :-
  append(_, [C|OtherComputers], Computers),
  foreach(member(O, SetSoFar), connected(C, O)),
  interConnectedSet_(OtherComputers, [C|SetSoFar], Set).
interConnectedSet_(_, Set, Set).
interConnectedSet(Computers, Set) :- append(_, [C|OtherComputers], Computers), interConnectedSet_(OtherComputers, [C], Set).

password_([], "").
password_([H|T], Password) :- password_(T, Pn), string_concat(",", H, Hn), string_concat(Hn, Pn, Password).
password(Group, Password) :- sort(Group, [H|T]), password_(T, Pn), string_concat(H, Pn, Password).

mightContainChiefHistorian(Computers) :- member(C, Computers), string_concat("t", _, C).
  
resultPart1(Computers, NumberOfSetsThatMightContainChiefHistorian) :-
  setof([C1,C2,C3], (interConnectedSet(Computers, [C1,C2,C3]), mightContainChiefHistorian([C1,C2,C3])), Sets),
  length(Sets, NumberOfSetsThatMightContainChiefHistorian).

resultPart2(Computers, Password) :-
  aggregate_all(max(L,G), (interConnectedSet(Computers,G), length(G, L)), max(_, LanParty)),
  password(LanParty, Password).
  
/* required for loadData */
postProcessData(Data, Computers) :- append(Data, AllData), sort(AllData, Computers).

data_line([C1,C2], Line) :- split_string(Line, "-", "", [C1,C2]), assert(connection(C1, C2)).
