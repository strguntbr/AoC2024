/* the user has to define data_line(RAW_DATA, PARSED_DATA) which converts a string into parsed data */
:- module(util, [
              parts/0,
              solve/0,
              solve/1,
              getData/1,
              getTestData/1,
              getTestData/2,
              verifyTests/1,
              printResultWithoutTest/1,
              mapsum/3,
              productlist/2,
              mapAndAggregate/4,
              mapAndAggregate/5,
              list/3
          ]).
:- use_module(library(pio)).
:- use_module(ansi).

byteLines([])                     --> call(eos), !.
byteLines([FirstLine|OtherLines]) --> byteLine(FirstLine), byteLines(OtherLines).
eos([], []).
byteLine([])                      --> ( "\n" ; call(eos) ), !.
byteLine([FirstByte|OtherBytes])  --> [FirstByte], byteLine(OtherBytes).
readByteLines(File, ByteLists) :- phrase_from_file(byteLines(ByteLists), File).
readLines(File, Lines) :- readByteLines(File, ByteLists), maplist(string_codes, Lines, ByteLists).

delayedHalt(ExitCode) :- halt(ExitCode). /* delayed halt not yet implemented, just halt for now */

loadData_(GroupedData, File) :-
  current_predicate(groupData/0), !,
  p_initPredicates,
  readLines(File, Lines),
  groupLines(Lines, GroupedLines), groupedData_groupedLines(GroupedData, GroupedLines).
loadData_(Data, File) :- p_initPredicates, readLines(File, Lines), data_lines(Data, Lines).

loadData(Data, File, []) :- exists_file(File), !, loadData_(Data, File).
loadData([], File, Error) :- format(string(Error), 'File ~w does not exist', [File]).

fileForDay(Day, Extension, File) :- format(atom(File), 'input/~w.~w', [Day, Extension]).

notIgnored(X) :- X \== ignore.
data_lines(Data, Lines) :- length(Lines, LineCount), numlist(1, LineCount, Indices), maplist(p_data_line, Indices, RawData, Lines), include(notIgnored, RawData, Data).

groupLines([""], [[]]) :- !.
groupLines([Line1], [[Line1]]) :- !.
groupLines([""|OtherLines], [[]|GroupedOtherLines]) :- !, groupLines(OtherLines, GroupedOtherLines).
groupLines([Line1|OtherLines], [[Line1|GroupedOtherLinesHead]|GroupedOtherLinesTail]) :- groupLines(OtherLines, [GroupedOtherLinesHead|GroupedOtherLinesTail]).

groupedData_groupedLines([HeaderData|GroupedData], [HeaderLines|GroupedLines]) :- 
  current_predicate(data_headers/2), !, data_headers(HeaderData, HeaderLines), groupedData_groupedLines_(GroupedData, GroupedLines).
groupedData_groupedLines([HeaderData|GroupedData], [[HeaderLine]|GroupedLines]) :- 
  current_predicate(data_header/2), !, data_header(HeaderData, HeaderLine), groupedData_groupedLines_(GroupedData, GroupedLines).
groupedData_groupedLines(GroupedData, GroupedLines) :- groupedData_groupedLines_(GroupedData, GroupedLines).
groupedData_groupedLines_(GroupedData, GroupedLines) :- maplist(data_lines, GroupedData, GroupedLines).

parts :- findall(P, p_part(P), Parts), printParts(Parts).
printParts([Part]) :- !, write(Part).
printParts([Part|T]) :- writeln(Part), printParts(T).

solve :- printResult(single).
solve(Part) :- printResult(Part).

printResult(Part) :- verifyTests(Part, false), flush_output, write(" "), printResultWithoutTest(Part).
printResultWithoutTest(Part) :- getData(Data), executePuzzle(Data, Part).

getTestData(Data) :- p_day(Day), fileForDay(Day, 'test', File), loadData(Data, File, Error), checkLoadError(Error, fail).
getData(Data) :- p_day(Day), fileForDay(Day, 'input', File), loadData(Data, File, Error), !, checkLoadError(Error, []>>delayedHalt(6)).
getData(_) :- write('Error: Could not load puzzle data'), delayedHalt(5).
checkLoadError([], _) :- !.
checkLoadError(Error, ErrorHandler) :- format('Error: ~w', [Error]), call(ErrorHandler).
executePuzzle(Data, Part) :- p_postProcessData(Data, PostprocessedData), p_auxData(AuxData), call_time(p_result(Part, PostprocessedData, AuxData, Result), Time), !, checkError(Result, puzzle), p_formatResult(Result, FormattedResult), writeResult('Result is ', FormattedResult, Time), p_finalize(Result).
executePuzzle(_, _) :- write('Error: could not find result for puzzle data'), delayedHalt(7).

writeFirstResultLine(ResultLine, 0) :- p_notInlineResult, !, writeln(""), white(ResultLine, WhiteResultLine), write(WhiteResultLine).
writeFirstResultLine(ResultLine, StartPos) :- cursorPosition(StartPos), white(ResultLine, WhiteResultLine), write(WhiteResultLine).
writeOtherResultLine(StartPos, ResultLine) :- writeln(""), Move is StartPos - 1, moveCursor(Move, right), white(ResultLine, WhiteResultLine), write(WhiteResultLine).
writeMultilineResult([SingleLine]) :- !, white(SingleLine, WhiteResult), write(WhiteResult).
writeMultilineResult([FirstLine|OtherLines]) :- !, writeFirstResultLine(FirstLine, StartPos), foreach(member(Line, OtherLines), writeOtherResultLine(StartPos, Line)).
writeResult(_, _, _) :- p_hideResult, !.
writeResult(Header, Result, Time) :- string(Result), !, write(Header), split_string(Result, "\n", "", Lines), writeMultilineResult(Lines), formatTime(Time.cpu, FormattedTime), format(' (~w)', FormattedTime). /* split multiline result to list and print as aligned list */
writeResult(Header, Result, Time) :- white(Result, WhiteResult), formatTime(Time.cpu, FormattedTime), format('~w~w (~w)', [Header, WhiteResult, FormattedTime]).

formatTime(Time, FormattedTime) :- Time < 1, !, Ms is round(Time * 1000 * 1000) / 1000, format(atom(FormattedTime), '~3fms', Ms).
formatTime(Time, FormattedTime) :- Time < 60, !, S is round(Time * 1000) / 1000, format(atom(FormattedTime), '~3fs', S).
formatTime(Time, FormattedTime) :- Time < 600, !, M is round(Time) div 60, S is round(Time) mod 60, format(atom(FormattedTime), '~dm ~ds', [M,S]).
formatTime(Time, FormattedTime) :- Time < 3600, !, M is round(Time / 60), format(atom(FormattedTime), '~dm', M).
formatTime(Time, FormattedTime) :- H is round(Time) div 3600, M is round(Time / 60) mod 60, format(atom(FormattedTime), '~dh ~dm', [H,M]).

testResult_(File, Part, AuxData, ExpectedResult) :- p_testResult(Part, Extension, AuxData, ExpectedResult), p_day(Day), format(atom(File), 'input/~w.~w', [Day, Extension]).

verifyTests(Part) :- verifyTests(Part, true).
verifyTests(_, _) :- current_predicate(skipTest/0), !, testSkipped(Status), format('[~w] ', [Status]).
verifyTests(Part, TimingEnabled) :- p_initDynamicTests(Part),
  (
    not(testResult_(_, Part, _, _)) -> (noTests(Status), FormattedTime = "")
    ; call_time(forall(testResult_(File, Part, AuxData, ExpectedResult), snapshot(verifyTest(File, Part, AuxData, ExpectedResult))), Time), formatTime(Time.cpu, FT), format(atom(FormattedTime), ' (~w)', [FT]), testPassed(Status)
  ),
  (TimingEnabled -> format('[~w]~w', [Status, FormattedTime]) ; format('[~w]', [Status])).

verifyTest(File, Part, AuxData, ExpectedResult) :- getTestData(File, TestData), executeTest(File, Part, TestData, AuxData, ExpectedResult).
getTestData(File, TestData) :- loadData(TestData, File, Error), !, checkTestLoadError(Error).
getTestData(File, _) :- testFailed(Status), format('[~w] Could not load test data ~w', [Status, File]), delayedHalt(1).
checkTestLoadError([]) :- !.
checkTestLoadError(Error) :- testFailed(Status), format('[~w] ~w', [Status, Error]), delayedHalt(2).
executeTest(File, Part, TestData, AuxData, ExpectedResult) :- p_postProcessData(TestData, PostprocessedData), p_result(Part, PostprocessedData, AuxData, TestResult), !, checkError(TestResult, test), verifyResult(File, Part, TestResult, ExpectedResult).
executeTest(File, _, _, _, _) :- testFailed(Status), format('[~w] No solution for test data ~w found', [Status, File]), delayedHalt(3).
verifyResult(_, _, TestResult, TestResult) :- !.
verifyResult(File, _, WrongResult, ExpectedResult) :- testFailed(Status), format("[~w] Test ~w returned ", [Status, File]), writeErrorResults(ExpectedResult, WrongResult), delayedHalt(4).

checkError(error{missing: Missing}, puzzle) :- !, format('~w implementation is missing', [Missing]), delayedHalt(9).
checkError(error{missing: Missing}, test) :- !, testFailed(Status), format('[~w] ~w implementation is missing', [Status, Missing]), delayedHalt(9).
checkError(_, _).

writeDiffChar(Expected, Expected) :- write(Expected), !.
writeDiffChar(missing, Expected) :- !, redbg(Expected, C), write(C).
writeDiffChar(Wrong, missing) :- !, greenbg(Wrong, C), write(C).
writeDiffChar(Wrong, _) :- yellowbg(Wrong, C), write(C).
writeDiffLine([], []).
writeDiffLine([], [ExpectedC1|ExpectedCs]) :- writeDiffChar(missing, ExpectedC1), writeDiffLine([], ExpectedCs).
writeDiffLine([WrongC1|WrongCs], []) :- writeDiffChar(WrongC1, missing), writeDiffLine(WrongCs, []).
writeDiffLine([WrongC1|WrongCs], [ExpectedC1|ExpectedCs]) :- writeDiffChar(WrongC1, ExpectedC1), writeDiffLine(WrongCs, ExpectedCs).
writeDiffLineBreak([], []) :- !. writeDiffLineBreak(_, _) :- writeln("").
writeDiff(_, [], []).
writeDiff(StartPos, [none|WrongLines], [_|ExpectedLines]) :- !,
  moveCursor(StartPos, right), write(none), writeDiffLineBreak(WrongLines, ExpectedLines), writeDiff(StartPos, WrongLines, ExpectedLines).
writeDiff(StartPos, [], [ExpectedLine1|ExpectedLines]) :-
  moveCursor(StartPos, right), string_chars(ExpectedLine1, ExpectedChars1),
  writeDiffLine([], ExpectedChars1), writeDiffLineBreak([], ExpectedLines),
  writeDiff(StartPos, [], ExpectedLines).
writeDiff(StartPos, [WrongLine1|WrongLines], []) :-
  moveCursor(StartPos, right), string_chars(WrongLine1, WrongChars1),
  writeDiffLine(WrongChars1, []), writeDiffLineBreak(WrongLines, []),
  writeDiff(StartPos, WrongLines, []).
writeDiff(StartPos, [WrongLine1|WrongLines], [ExpectedLine1|ExpectedLines]) :-
  moveCursor(StartPos, right), string_chars(WrongLine1, WrongChars1), string_chars(ExpectedLine1, ExpectedChars1),
  writeDiffLine(WrongChars1, ExpectedChars1), writeDiffLineBreak(WrongLines, ExpectedLines),
  writeDiff(StartPos, WrongLines, ExpectedLines).
cursorForDiff(StartPos) :- p_inlineWrongResult, !, cursorPosition(CurPos), StartPos is CurPos - 1, moveCursor(StartPos, left).
cursorForDiff(0) :- writeln("").
writeWrongResult(ExpectedResult, WrongResult) :-
  is_list(WrongResult), is_list(ExpectedResult), !, (isAnsiXterm -> DiffResult = ExpectedResult ; DiffResult = WrongResult),
  cursorForDiff(StartPos), writeDiff(StartPos, WrongResult, DiffResult), (p_inlineWrongResult -> (writeln(""), ResultStartPos is StartPos - 11, moveCursor(ResultStartPos, right)) ; writeln("")).
writeWrongResult(_, WrongResult) :- write(WrongResult), write(" ").
writeExpectedResult(ExpectedResult, WrongResult) :- is_list(WrongResult), is_list(ExpectedResult), !, cursorForDiff(StartPos), writeDiff(StartPos, ExpectedResult, ExpectedResult).
writeExpectedResult(ExpectedResult, _) :- write(ExpectedResult).
writeErrorResults(ExpectedResult, WrongResult) :-
  string(ExpectedResult), split_string(ExpectedResult, "\n", "", [EL1,ELt]), string(WrongResult), split_string(WrongResult, "\n", "", [RL1,RLt]), (ELt \= [] ; RLt \= []), !,
  writeErrorResults([EL1,ELt], [RL1,RLt]).
writeErrorResults(ExpectedResult, WrongResult) :- p_hideExpectedResultForDiff, cursorForDiff(StartPos), writeDiff(StartPos, WrongResult, ExpectedResult), !.
writeErrorResults(ExpectedResult, WrongResult) :- writeWrongResult(ExpectedResult, WrongResult), write("instead of "), writeExpectedResult(ExpectedResult, WrongResult).

noTests(Text) :-    yellow('NO TESTS FOUND', Text).
testPassed(Text) :- green(' TESTS PASSED ', Text).
testFailed(Text) :- red(' TESTS FAILED ', Text).
testSkipped(Text) :- yellow('TESTS  SKIPPED', Text).

/* proxies for methods defined outside this  file */
p_day(Day) :- day(Day).
p_initPredicates :- current_predicate(initPredicates/0) -> initPredicates ; true.
p_postProcessData(Data, PostprocessedData) :- current_predicate(postProcessData/2) -> postProcessData(Data, PostprocessedData) ; PostprocessedData=Data.
p_data_line(Index, Data, Line) :- current_predicate(data_line/3) -> data_line(Index, Data, Line) ; current_predicate(data_line/2) -> data_line(Data, Line) ; Data=Line.
p_result(part1, Data, AuxData, Result) :- AuxData = none -> (current_predicate(resultPart1/2) -> resultPart1(Data, Result) ; Result=error{missing: "resultPart1/2"}) ; (current_predicate(resultPart1/3) -> resultPart1(Data, AuxData, Result) ; Result=error{missing: "resultPart1/3"}).
p_result(part2, Data, AuxData, Result) :- AuxData = none -> (current_predicate(resultPart2/2)-> resultPart2(Data, Result) ; Result=error{missing: "resultPart2/2"}) ; (current_predicate(resultPart2/3) -> resultPart2(Data, AuxData, Result) ; Result=error{missing: "resultPart2/3"}).
p_result(single, Data, AuxData, Result) :- AuxData = none -> (current_predicate(result/2)-> result(Data, Result) ; Result=error{missing: "result/2"}) ; (current_predicate(result/3) -> result(Data, AuxData, Result) ; Result=error{missing: "result/3"}).
p_formatResult(Result, FormattedResult) :- current_predicate(formatResult/2), !, formatResult(Result, FormattedResult). p_formatResult(Result, Result).
p_testResult(single, "test", none, ExpectedResult) :- current_predicate(testResult/1), testResult(ExpectedResult).
p_testResult(Part, "test", none, ExpectedResult) :- current_predicate(testResult/2), member(Part, [part1,part2,single]), testResult(Part, ExpectedResult).
p_testResult(single, Extension, none, ExpectedResult) :- current_predicate(testResult/2), testResult(Extension, ExpectedResult), string(Extension).
p_testResult(Part, Extension, none, ExpectedResult) :- current_predicate(testResult/3), member(Part, [part1,part2,single]), testResult(Part, Extension, ExpectedResult), string(Extension).
p_testResult(single, "test", AuxData, ExpectedResult) :- current_predicate(testResult/2), testResult(auxData(AuxData), ExpectedResult).
p_testResult(Part, "test", AuxData, ExpectedResult) :- current_predicate(testResult/3), member(Part, [part1,part2,single]), testResult(Part, auxData(AuxData), ExpectedResult).
p_testResult(single, Extension, AuxData, ExpectedResult) :- current_predicate(testResult/3), testResult(Extension, AuxData, ExpectedResult).
p_testResult(Part, Extension, AuxData, ExpectedResult) :- current_predicate(testResult/4), member(Part, [part1,part2,single]), testResult(Part, Extension, AuxData, ExpectedResult).
p_finalize(Result) :- current_predicate(finalize/1) -> finalize(Result) ; true.
p_initDynamicTests(Part) :- current_predicate(initDynamicTests/1), !, initDynamicTests(Part).
p_initDynamicTests(_) :- current_predicate(initDynamicTests/0) -> initDynamicTests ; true.
p_hideResult :- current_predicate(hideResult/0).
p_notInlineResult :- \+ isAnsiXterm ; current_predicate(notInlineResult/0).
p_inlineWrongResult :- isAnsiXterm, current_predicate(inlineWrongResult/0).
p_inlineWrongResult :- isAnsiXterm, \+ p_notInlineResult.
p_hideExpectedResultForDiff :- isAnsiXterm, current_predicate(hideExpectedResultForDiff/0).
p_auxData(AuxData) :- current_predicate(auxData/1) -> auxData(AuxData) ; AuxData = none.
p_part("part1") :- current_predicate(resultPart1/2) ; current_predicate(resultPart1/3).
p_part("part2") :- current_predicate(resultPart2/2) ; current_predicate(resultPart2/3).
p_part("single") :- current_predicate(result/2) ; current_predicate(result/3).

/* Misc useful utility functions */
productlist([], 1).
productlist([H|T], Product) :- productlist(T, TProduct), Product is H*TProduct.

mapsum(List, MapFunction, Sum) :- mapAndAggregate(MapFunction, List, sumlist, Sum).

mapAndAggregate(MapFunction, List, AggregateFunction, Result) :- maplist(MapFunction, List, Values), call(AggregateFunction, Values, Result).
mapAndAggregate(MapBiFunction, List1, List2, AggregateFunction, Result) :- maplist(MapBiFunction, List1, List2, Values), call(AggregateFunction, Values, Result).

list(Length, Generator, List) :- functor(Generator, _, 2), !, LengthZero is Length - 1, findall(Elem, (between(0, LengthZero, I), call(Generator, I, Elem)), List).
list(Length, Elem, List) :- list(Length, [_,Elem]>>true, List).