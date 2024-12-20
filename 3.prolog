day(3). testResult(part1, 161). testResult(part2, 48).

:- use_module(lib/solve).

enable(L, Products) :- string_chars("on\x27t()", DONT), append(DONT, T, L), instr(T, 0, Products).
enable(L, Products) :- string_chars("o()", DO), append(DO, T, L), instr(T, 1, Products).

finishMul(T, P1, P2, E, Products) :- instr(T, E, NextProducts), !, Products is NextProducts + P1 * P2 * E.
finishMul(_, P1, P2, E, Products) :- Products is P1 * P2 * E.

p2(['\x29'|T], P1, P2, E, Products) :- finishMul(T, P1, P2, E, Products).
p2([N|T], P1, LN, E, Products) :- atom_number(N, NR), NN is LN * 10 + NR, p2(T, P1, NN, E, Products).

p1([','|T], P1, E, Products) :- p2(T, P1, 0, E, Products).
p1([N|T], LN, E, Products) :- atom_number(N, NR), NN is LN * 10 + NR, p1(T, NN, E, Products).

mul(Memory, E, Products) :- string_chars("ul(", M), append(M, T, Memory), p1(T, 0, E, Products).

instr([d|T], _, Products) :- extendedMode, enable(T, Products).
instr([m|T], E, Products) :- mul(T, E, Products).
instr([_|T], E, Products) :- instr(T, E, Products).

resultPart1(Memory, Products) :- flatten(Memory, FlatMemory), instr(FlatMemory, 1, Products).
resultPart2(Memory, Products) :- assert(extendedMode), flatten(Memory, FlatMemory), instr(FlatMemory, 1, Products).

/* required for loadData */
initPredicates :- dynamic(extendedMode/0).

data_line(Data, Line) :- string_chars(Line, Data).
