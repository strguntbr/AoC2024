day(24). groupData. testResult(part1, 2024).

:- use_module(lib/solve).

/* Part 1 */
op(In1, In2, "AND", Result) :- Result is In1 /\ In2.
op(In1, In2, "OR", Result) :- Result is In1 \/ In2.
op(In1, In2, "XOR", Result) :- Result is In1 xor In2.

process([], []) :- !.
process(Gates, Z) :-
  select(Gate, Gates, OtherGates),
  value(Gate.in1, In1), value(Gate.in2, In2), !,
  op(In1, In2, Gate.op, GateResult),
  assert(value(Gate.gate, GateResult)),
  process(OtherGates, Zn),
  (string_concat("z", Zid, Gate.gate) -> Z = [[Zid, GateResult]|Zn] ; Z = Zn).

resultPart1([_, Gates], Result) :-
  process(Gates, Z),
  sort(Z, Zsorted),
  reverse(Zsorted, A),
  foldl([[_,C],I,R]>>(R is I*2+C), A, 0, Result).

/* Part 2 */
xor(Gate, A, B) :- gate(Gate, "XOR", A, B) ; gate(Gate, "XOR", B, A).
and(Gate, A, B) :- gate(Gate, "AND", A, B) ; gate(Gate, "AND", B, A).
or(Gate, A, B) :- gate(Gate, "OR", A, B) ; gate(Gate, "OR", B, A).

confirmAll([]).
confirmAll([H|T]) :- (confirmed(H) -> true ; assert(confirmed(H))), confirmAll(T).

swap(Gate, OtherGate) :-
  gate(Gate, Op, In1, In2), gate(OtherGate, OtherOp, OtherIn1, OtherIn2),
  Gate \= OtherGate,
  \+ confirmed(Gate), \+ confirmed(OtherGate),
  retractall(gate(Gate, Op, In1, In2)), retractall(gate(OtherGate, OtherOp, OtherIn1, OtherIn2)),
  assert(gate(Gate, OtherOp, OtherIn1, OtherIn2)), assert(gate(OtherGate, Op, In1, In2)).

% Assume that one swap is enough to fix each Z. That's at least true for my input and simplifies fixing vastly.
fixZ(true, N, PossibleErrors, Gates, Gate, [Error, Replacement]) :-
  member(Error, PossibleErrors),
  swap(Error, Replacement),
  (z(N, false, Gates, Gate, []) -> ! ; (swap(Error, Replacement), fail)). % revert and fail if this swap did not fix it

autoFixZ(N, Fixes) :- z(N, true, _, _, Fixes), !.
z(0, _, [Gate], Gate, []) :- % special handling for first Z (z00)
  x(0, XGate),
  y(0, YGate),
  xor(Gate, XGate, YGate),
  z(0, Gate), !,
  confirmAll([Gate]).
z(Last, _, Gates, Gate, []) :- % special handling for last Z (z45)
  NNext is Last+1, \+ z(NNext, _), NPrev is Last-1,
  c(NPrev, Gates, Gate),
  confirmAll([Gate]).
z(N, _, Gates, Gate, Fixes) :-
  NPrev is N-1,
  c(NPrev, CGates, CGate), t(N, TGates, TGate),
  xor(MaybeGate, CGate, TGate),
  z(N, Gate),
  (MaybeGate=Gate -> (!, Fixes=[]) ; (swap(Gate, MaybeGate), Fixes=[Gate, MaybeGate])),
  append([CGates, TGates, [Gate]], Gates),
  confirmAll(Gates).
z(N, true, Gates, Gate, Fixes) :-
  findall(G, (gate(G,_,_,_), \+ confirmed(G)), UnconfirmedGates),
  fixZ(true, N, UnconfirmedGates, Gates, Gate, Fixes).
c(0, [Gate], Gate) :-
  x(0, XGate), y(0, YGate),
  and(Gate, XGate, YGate).
c(N, Gates, Gate) :-
  s(N, SGates, SGate), u(N, UGates, NGate),
  or(Gate, SGate, NGate),
  append([SGates, UGates, [Gate]], Gates).
t(N, [Gate], Gate) :-
  x(N, XGate), y(N, YGate),
  xor(Gate, XGate, YGate).
u(N, Gates, Gate) :-
  NPrev is N-1,
  c(NPrev, CGates, CGate), t(N, TGates, TGate),
  and(Gate, CGate, TGate),
  append([CGates, TGates, [Gate]], Gates).
s(N, [Gate], Gate) :-
  x(N, XGate), y(N, YGate),
  and(Gate, XGate, YGate).
x(N, Gate) :- format(string(Gate), 'x~|~`0t~d~2+', [N]). % prefix number with 0s if its shorter than 2 characters
y(N, Gate) :- format(string(Gate), 'y~|~`0t~d~2+', [N]).
z(N, Gate) :- gate(Gate, _, _, _), string_concat("z", Number, Gate), number_string(N, Number).

mergeFixes(Z, FixesSoFar, Fixes) :- z(N, Z), autoFixZ(N, NewFixes), append(FixesSoFar, NewFixes, Fixes).

gate(Gate) :- gate(Gate, _, _, _).

resultPart2(_, Fixes) :-
  setof(Z, N^(gate(Z), string_concat("z",N,Z)), Zs),
  foldl(mergeFixes, Zs, [], Wires),
  sort(Wires, Fixes).
  
/* required for loadData */
initPredicates :- dynamic(confirmed/1).

data_line(Gate, Line) :-
  split_string(Line, ":", " ", [Gate,Number]),
  number_string(Value, Number),
  assert(value(Gate, Value)).
data_line(gate{gate: Gate, op:Op, in1:In1, in2:In2}, Line) :-
  split_string(Line, " ", " ->", [In1, Op, In2, Gate]),
  assert(gate(Gate, Op, In1, In2)).
