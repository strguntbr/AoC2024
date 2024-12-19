day(17). testResult(part1, "4,6,3,5,6,3,5,2,1,0"). testResult(part2, "test-2", 117440).

:- use_module(lib/solve).

comboOperand([A,B,C], Operand, Value) :- nth0(Operand, [0,1,2,3,A,B,C], Value).

adv([A,B,C], Operand, [An,B,C]) :- comboOperand([A,B,C], Operand, Value), An is A >> Value.
bxl([A,B,C], Operand, [A,Bn,C]) :- Bn is B xor Operand.
bst([A,B,C], Operand, [A,Bn,C]) :- comboOperand([A,B,C], Operand, Value), Bn is Value mod 8.
jnz(IP, [A,_,_], Operand, IPn) :- A = 0 -> IPn = IP ; IPn = Operand.
bxc([A,B,C], _, [A,Bn,C]) :- Bn is B xor C.
out([A,B,C], Operand, Output) :- comboOperand([A,B,C], Operand, Value), Output is Value mod 8.
bdv([A,B,C], Operand, [A,Bn,C]) :- comboOperand([A,B,C], Operand, Value), Bn is A >> Value.
cdv([A,B,C], Operand, [A,B,Cn]) :- comboOperand([A,B,C], Operand, Value), Cn is A >> Value.

instruction(0, Operand, Registers, NextRegisters, IP, IP,  "") :- adv(Registers, Operand, NextRegisters).
instruction(1, Operand, Registers, NextRegisters, IP, IP,  "") :- bxl(Registers, Operand, NextRegisters).
instruction(2, Operand, Registers, NextRegisters, IP, IP,  "") :- bst(Registers, Operand, NextRegisters).
instruction(3, Operand, Registers, Registers,     IP, IPn, "") :- jnz(IP, Registers, Operand, IPn).
instruction(4, Operand, Registers, NextRegisters, IP, IP,  "") :- bxc(Registers, Operand, NextRegisters).
instruction(5, Operand, Registers, Registers,     IP, IP,  Output) :- out(Registers, Operand, Number), number_string(Number, Output).
instruction(6, Operand, Registers, NextRegisters, IP, IP,  "") :- bdv(Registers, Operand, NextRegisters).
instruction(7, Operand, Registers, NextRegisters, IP, IP,  "") :- cdv(Registers, Operand, NextRegisters).

appendNumber("", Number, Number) :- !.
appendNumber(String, "", String) :- !.
appendNumber(String, Number, AppendedNumberString) :- string_concat(String, ",", StringSep), string_concat(StringSep, Number, AppendedNumberString).

/* Part 1 */
process(Program, _, IP, "") :- length(Program, PL), IP >= PL, !.
process(Program, Registers, IP, Output) :-
  IPt is IP+2,
  nth0(IP, Program, Opcode), nth1(IPt, Program, Operand),
  instruction(Opcode, Operand, Registers, NextRegisters, IPt, IPn, InstructionOutput),
  process(Program, NextRegisters, IPn, NextOutput),
  appendNumber(InstructionOutput, NextOutput, Output).

resultPart1([["A",A],["B",B],["C",C],_,Program], Output) :- process(Program, [A,B,C], 0, Output).

processSingleOutput(Program, Registers, IP, Output) :-
  IPt is IP+2,
  nth0(IP, Program, Opcode), nth1(IPt, Program, Operand),
  (Opcode = 5 -> out(Registers, Operand, Output) ; (
    instruction(Opcode, Operand, Registers, NextRegisters, IPt, IPn, _),
    processSingleOutput(Program, NextRegisters, IPn, Output)
  )).

iterativeSolve(_, [], A, A).
iterativeSolve(Program, [First|Rest], CurA, Result) :-
  between(0, 7, Last3Bits), NextA is Last3Bits \/ (CurA << 3),
  processSingleOutput(Program, [NextA,0,0], 0, First),
  iterativeSolve(Program, Rest, NextA, Result).

/* Part 2 */
resultPart2([["A",_],["B",_],["C",_],_,Program], Result) :-
  checkAssumptions(Program),
  between(0, 7, Starting7Bits),
  reverse(Program, Output),
  iterativeSolve(Program, Output, Starting7Bits, Result).

checkAssumptions(Program) :-
  (
    append(P, [3,0], Program),     /* the program ends with a 'jnz' 0      */
    programContains(P, [3,_], 0),  /* and does not contain any other 'jnz' */
    programContains(P, [0,3], 1),  /* and contains one 'adv' 3             */
    programContains(P, [0,_], 1),  /* but no other 'adv'                   */
    programContains(P, [5,_], 1),  /* and exactly one 'out'                */
    /* if the program outputs A no further conditions are required         */
    /* if the program outputs C at least on 'cdv' needs to be present (or  */
    /* else C will never change). At least on 'cdv' needs to be before the */
    /* 'out', as we start each cycle with C reset to 0.                    */
    (appendProgram(PC, [5,6|_], P) -> appendProgram(_, [7,0|_], PC) ; true),
    /* if the program outputs B at least on 'bdv' or 'bxc' needs to be     */
    /* present before the 'out' (as we start each cycle with B reset to 0) */
    /* if a 'bxc' exists before the 'out', at least one 'cdv' needs to be  */
    /* present before that 'bxc'.                                          */
    (appendProgram(PB, [5,5|_], P) ->
      (
        (
          appendProgram(_, [6,_|_], PB)
        )
        ;
        (
          appendProgram(PPB, [4,_|_], PB), appendProgram(_, [7,_|_], PPB)
        )
      ) ; true
    )
  )
  ;
  (
    writeln("Program does not match some conditions"), fail
  ).

appendProgram(FirstPart, SecondPart, Program) :- append(FirstPart, SecondPart, Program), length(FirstPart, L), 0 is L mod 2.

programContains([], [_, _], 0).
programContains([Opcode,Operand|T], [Opcode,Operand], Count) :- !, programContains(T, [Opcode,Operand], CountN), Count is CountN+1.
programContains([_,_|T], [Opcode,Operand], Count) :- programContains(T, [Opcode,Operand], Count).

/* required for loadData */
data_line(Data, Line) :- split_string(Line, " ,", ":", Split), (register(Split, Data) ; program(Split, Data)).
data_line([], "").
register(["Register",Register,ValueString], [Register,Value]) :- number_string(Value, ValueString).
program(["Program"|InstructionStrings], Instructions) :- maplist(number_string, Instructions, InstructionStrings).