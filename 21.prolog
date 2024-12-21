day(21). testResult(part1, 126384). testResult(part2_, 0).

:- use_module(lib/solve).

numericPad(Button, Pos) :- member([Button, Pos], [
    ['7',[0,0]], ['8',[0,1]], ['9',[0,2]],
    ['4',[1,0]], ['5',[1,1]], ['6',[1,2]],
    ['1',[2,0]], ['2',[2,1]], ['3',[2,2]],
                 ['0',[3,1]], ['A',[3,2]]
  ]).
directionalPad(B, Pos) :- member([B, Pos], [
                  ['^', [0,1]], ['A', [0,2]],
    ['<', [1,0]], ['v', [1,1]], ['>', [1,2]]
  ]).

pad(numeric, Button, Pos) :- numericPad(Button, Pos).
pad(directional, Button, Pos) :- directionalPad(Button, Pos).

generate(0, _, _, []).
generate(D, E, _, [E|Sn]) :- D < 0, Dn is D+1, generate(Dn, E, _, Sn).
generate(D, _, E, [E|Sn]) :- D > 0, Dn is D-1, generate(Dn, _, E, Sn).

moveToNextButton(Type, CurButton, NextButton, Sequence) :-
  pad(Type, CurButton, [Sx, Sy]),
  pad(Type, NextButton, [Ex, Ey]),
  Dx is Ex - Sx, Dy is Ey - Sy,
  generate(Dx, '^', 'v', XSeq), generate(Dy, '<', '>', YSeq),
  (
    (pad(Type, _, [Ex, Sy]), XSeq \= [], append(XSeq, YSeq, Sequence)) ; (pad(Type, _, [Sx, Ey]), YSeq \= [], append(YSeq, XSeq, Sequence)) ; (XSeq = [], YSeq = [], Sequence = [])
  ).

pressNextButton(Type, CurButton, Indirections, NextButton, Count) :-
  moveToNextButton(Type, CurButton, NextButton, Seq),
  append(Seq, ['A'], PressNextSequence),
  pressButtons(directional, PressNextSequence, Indirections, Count).

shortestSequenceForNextButton(Type, CurButton, Indirections, NextButton, MinCount) :-
  aggregate_all(min(Count), pressNextButton(Type, CurButton, Indirections, NextButton, Count), MinCount).

pressButtons(Type, Buttons, Indirections, Count) :- pressButtonsCache(Type, Buttons, Indirections, Count), !.
pressButtons(Type, Buttons, -1, Count) :- !, length(Buttons, Count), assert(pressButtonsCache(Type, Buttons, -1, Count)).
pressButtons(Type, Buttons, Indirections, Count) :-
  RemainingIndirections is Indirections - 1,
  foldl([NextButton,[CurButton,CurCount],[NextButton,NextCount]]>>(
      shortestSequenceForNextButton(Type, CurButton, RemainingIndirections, NextButton, MinNextCount),
      NextCount is CurCount + MinNextCount
    ), Buttons, ['A',0], [_,Count]),
  assert(pressButtonsCache(Type, Buttons, Indirections, Count)).

resultPart1(Codes, Result) :- mapsum(Codes, [[NumericPart,Code],Complexity]>>(pressButtons(numeric, Code, 2, SeqLength), Complexity is SeqLength*NumericPart), Result).
    
resultPart2(Codes, Result) :- mapsum(Codes, [[NumericPart,Code],Complexity]>>(pressButtons(numeric, Code, 25, SeqLength), Complexity is SeqLength*NumericPart), Result).

/* required for loadData */
initPredicates :- dynamic(pressButtonsCache/4).

data_line([Number,Code], Line) :- string_concat(NumberString, "A", Line), number_string(Number, NumberString), string_chars(Line, Code).
