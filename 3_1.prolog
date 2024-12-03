day(3). testResult(161).

:- use_module(lib/solve).

finishMul(T, P1, P2, Products) :- instr(T, NextProducts), !, Products is NextProducts + P1 * P2.
finishMul(_, P1, P2, Products) :- Products is P1 * P2.

p2(['\x29'|T], P1, P2, Products) :- finishMul(T, P1, P2, Products).
p2([N|T], P1, LN, Products) :- atom_number(N, NR), NN is LN * 10 + NR, p2(T, P1, NN, Products).

p1([','|T], P1, Products) :- p2(T, P1, 0, Products).
p1([N|T], LN, Products) :- atom_number(N, NR), NN is LN * 10 + NR, p1(T, NN, Products).

mul(Memory, Products) :- string_chars("ul(", M), append(M, T, Memory), p1(T, 0, Products).

instr([m|T], Products) :- mul(T, Products).
instr([_|T], Products) :- instr(T, Products).

result(Memory, Products) :- 
    flatten(Memory, FlatMemory),
    instr(FlatMemory, Products).

/* required for loadData */
data_line(Data, Line) :- string_chars(Line, Data).
