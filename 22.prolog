day(22). testResult(part1, 37327623). testResult(part2, "test-2", 23).

:- use_module(lib/solve).

next(Number, NextNumber) :-
  N2 is ((Number << 6) xor Number) /\ 16777215,
  N3 is ((N2 >> 5) xor N2) /\ 16777215,
  NextNumber is ((N3 << 11) xor N3) /\ 16777215.

/* Part 1 */
resultPart1(Numbers, Result) :-
  length(L, 2000), 
  mapsum(Numbers, [Number,FinalNumber]>>(foldl([_,N,NN]>>(next(N,NN)), L, Number, FinalNumber)), Result).

/* Part 2 */
popPriceSum(Sequence, Price) :- once(retract(priceSum(Sequence, Price)) ; Price = 0).
addPriceToSum(Sequence, _) :- member(i, Sequence), !.
addPriceToSum(_, 0) :- !.
addPriceToSum(Sequence, Price) :- popPriceSum(Sequence, OldSum), Sum is OldSum + Price, assert(priceSum(Sequence, Sum)).

calc(0, _, Number, _, _, Number) :- !.
calc(Cycles, StartingNumber, Number, Sequence, Sequences, FinalNumber) :-
  next(Number, NextNumber),
  NextPrice is NextNumber mod 10, Diff is NextPrice - (Number mod 10),
  append(NextSequence, [_], [Diff|Sequence]),
  (member(NextSequence, Sequences) -> NextSequences=Sequences ; (addPriceToSum(NextSequence, NextPrice), NextSequences=[NextSequence|Sequences])),
  NextCycles is Cycles - 1,
  calc(NextCycles, StartingNumber, NextNumber, NextSequence, NextSequences, FinalNumber).
calc(Number, FinalNumber) :- calc(2000, Number, Number, [i,i,i,i], [], FinalNumber).

resultPart2(Numbers, Result) :-
  maplist(calc, Numbers, _),
  aggregate_all(max(P), priceSum(_, P), Result).

/* required for loadData */
initPredicates :- set_prolog_flag(stack_limit, 8_589_934_592), dynamic(priceSum/2).

data_line(Number, Line) :- number_string(Number, Line).
