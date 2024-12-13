day(13). groupData. testResult(part1, 480).

:- use_module(library(clpq)), use_module(lib/solve).

solve([button{label: "A", x: Xa, y: Ya}, button{label: "B", x: Xb, y: Yb}, [X, Y]], AcceptanceCriteria, Solution) :-
  {
    X = A * Xa + B * Xb,
    Y = A * Ya + B * Yb
  },
  call(AcceptanceCriteria, A, B) -> Solution is 3*A+B ; Solution=0.

integerSolutionSmaller100(A, B) :- integerSolution(A, B), A < 100, B < 100.
integerSolution(A, B) :- is_of_type(integer, A), is_of_type(integer, B).

solve100(Machine, Solution) :- solve(Machine, integerSolutionSmaller100, Solution).
solveUnlimited(Machine, Solution) :- solve(Machine, integerSolution, Solution).

fixMeasurement([ButtonA, ButtonB, [Xe,Ye]], [ButtonA, ButtonB, [X,Y]]) :- X is Xe + 10000000000000, Y is Ye + 10000000000000.

resultPart1(Machines, Result) :- mapsum(Machines, solve100, Result).
resultPart2(Machines, Result) :- maplist(fixMeasurement, Machines, FixedMachines), mapsum(FixedMachines, solveUnlimited, Result).

/* required for loadData */
data_line(button{label: Button, x: X, y: Y}, Line) :-
  split_string(Line, ",:", " ", [ButtonS, XS, YS]),
  button(ButtonS, Button), move(XS, "X", X), move(YS, "Y", Y).
data_line([X, Y], Line) :-
  split_string(Line, ",:", " ", ["Prize", XS, YS]),
  pos(XS, "X", X),  pos(YS, "Y", Y).

button(ButtonString, Button) :- split_string(ButtonString, " ", "", ["Button", Button]).
move(Instruction, Axis, Diff) :- split_string(Instruction, "+", "", [Axis, DiffStr]), number_string(Diff, DiffStr).
pos(PosString, Axis, Pos) :- split_string(PosString, "=", "", [Axis, P]), number_string(Pos, P).
