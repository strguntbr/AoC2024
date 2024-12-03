/* the user has to define data_line(RAW_DATA, PARSED_DATA) which converts a string into parsed data */
:- module(util, [
              solve/0,
              getData/1,
              getTestData/1,
              getTestData/2,
              verifyTests/0,
              printResultWithoutTest/0,
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

loadData_(GroupedData, File) :-
  current_predicate(groupData/0), !,
  p_resetData, readLines(File, Lines),
  groupLines(Lines, GroupedLines), groupedData_groupedLines(GroupedData, GroupedLines).
loadData_(Data, File) :- p_resetData, readLines(File, Lines), data_lines(Data, Lines).

loadData(Data, File, []) :- exists_file(File), !, loadData_(Data, File).
loadData([], File, Error) :- format(string(Error), 'File ~w does not exist', [File]).

fileForDay(Day, Extension, File) :- format(atom(File), 'input/~w.~w', [Day, Extension]).

notIgnored(X) :- X \== ignore.
data_lines(Data, Lines) :- maplist(p_data_line, RawData, Lines), include(notIgnored, RawData, Data).

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

solve :- printResult.

printResult :- verifyTests, printResultWithoutTest.
printResultWithoutTest :- getData(Data), executePuzzle(Data).

getTestData(Data) :- p_day(Day), fileForDay(Day, 'test', File), loadData(Data, File, Error), checkLoadError(Error, fail).
getData(Data) :- p_day(Day), fileForDay(Day, 'input', File), loadData(Data, File, Error), !, checkLoadError(Error, []>>halt(6)).
getData(_) :- write('Error: Could not load puzzle data'), halt(5).
checkLoadError([], _) :- !.
checkLoadError(Error, ErrorHandler) :- format('Error: ~w', [Error]), call(ErrorHandler).
executePuzzle(Data) :- p_postProcessData(Data, PostprocessedData), p_result(PostprocessedData, Result), !, write('Result is '), p_formatResult(Result, FormattedResult), writeResult(FormattedResult), p_finalize(Result).
executePuzzle(_) :- writeln('Error: could not find result for puzzle data'), halt(7).

writeFirstResultLine(ResultLine, 0) :- p_notInlineResult, !, writeln(""), white(ResultLine, WhiteResultLine), write(WhiteResultLine).
writeFirstResultLine(ResultLine, StartPos) :- cursorPosition(StartPos), white(ResultLine, WhiteResultLine), write(WhiteResultLine).
writeOtherResultLine(StartPos, ResultLine) :- writeln(""), Move is StartPos - 1, moveCursor(Move, right), white(ResultLine, WhiteResultLine), write(WhiteResultLine).
writeResult(_) :- p_hideResult, !.
writeResult(Result) :- string(Result), !, split_string(Result, "\n", "", Lines), writeResult(Lines). /* split multiline result to list and print as aligned list */
writeResult([SingleLine]) :- !, white(SingleLine, WhiteResult), write(WhiteResult).
writeResult([FirstLine|OtherLines]) :- !, writeFirstResultLine(FirstLine, StartPos), foreach(member(Line, OtherLines), writeOtherResultLine(StartPos, Line)).
writeResult(Result) :- white(Result, WhiteResult), write(WhiteResult).

testResult_(File, AuxData, ExpectedResult) :- p_testResult(Extension, AuxData, ExpectedResult), p_day(Day), format(atom(File), 'input/~w.~w', [Day, Extension]).
findTests(Tests) :- findall([File, ExpectedResult], testResult_(File, ExpectedResult), Tests).

verifyTests :- current_predicate(skipTest/0), !, testSkipped(Status), format('[~w] ', [Status]).
verifyTests :- p_initDynamicTests,
  (
    not(testResult_(_, _, _)) -> noTests(Status)
    ; forall(testResult_(File, AuxData, ExpectedResult), verifyTest(File, AuxData, ExpectedResult)), testPassed(Status)
  ), 
  format('[~w] ', [Status]).

verifyTest(File, AuxData, ExpectedResult) :- getTestData(File, TestData), executeTest(File, TestData, AuxData, ExpectedResult).
getTestData(File, TestData) :- loadData(TestData, File, Error), !, checkTestLoadError(Error).
getTestData(File, _) :- testFailed(Status), format('[~w] Could not load test data ~w', [Status, File]), halt(1).
checkTestLoadError([]) :- !.
checkTestLoadError(Error) :- testFailed(Status), format('[~w] ~w', [Status, Error]), halt(2).
executeTest(File, TestData, AuxData, ExpectedResult) :- p_postProcessData(TestData, PostprocessedData), p_result(PostprocessedData, AuxData, TestResult), !, verifyResult(File, TestResult, ExpectedResult).
executeTest(File, _, _, _) :- testFailed(Status), format('[~w] No solution for test data ~w found', [Status, File]), halt(3).
verifyResult(_, TestResult, TestResult) :- !.
verifyResult(File, WrongResult, ExpectedResult) :- testFailed(Status), format("[~w] Test ~w returned ", [Status, File]), writeErrorResults(ExpectedResult, WrongResult), halt(4).

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
  cursorForDiff(StartPos), writeDiff(StartPos, WrongResult, DiffResult), (p_inlineWrongResult -> write(" ") ; writeln("")).
writeWrongResult(_, WrongResult) :- write(WrongResult), write(" ").
writeExpectedResult(ExpectedResult, WrongResult) :- is_list(WrongResult), is_list(ExpectedResult), !, cursorForDiff(StartPos), writeDiff(StartPos, ExpectedResult, ExpectedResult).
writeExpectedResult(ExpectedResult, _) :- write(ExpectedResult).
writeErrorResults(ExpectedResult, WrongResult) :- p_hideExpectedResultForDiff, cursorForDiff(StartPos), writeDiff(StartPos, WrongResult, ExpectedResult), !.
writeErrorResults(ExpectedResult, WrongResult) :- writeWrongResult(ExpectedResult, WrongResult), write("instead of "), writeExpectedResult(ExpectedResult, WrongResult).

noTests(Text) :-    green('NO TESTS FOUND', Text).
testPassed(Text) :- green(' TEST  PASSED ', Text).
testFailed(Text) :- red(' TEST  FAILED ', Text).
testSkipped(Text) :- yellow(' TEST SKIPPED ', Text).

/* proxies for methods defined outside this  file */
p_day(Day) :- day(Day).
p_resetData :- current_predicate(resetData/0) -> resetData ; true.
p_postProcessData(Data, PostprocessedData) :- current_predicate(postProcessData/2) -> postProcessData(Data, PostprocessedData) ; PostprocessedData=Data.
p_data_line(Data, Line) :- current_predicate(data_line/2) -> data_line(Data, Line) ; Data=Line.
p_result(Data, Result) :- result(Data, Result).
p_result(Data, AuxData, Result) :- AuxData = none -> result(Data, Result) ; result(Data, AuxData, Result).
p_formatResult(Result, FormattedResult) :- current_predicate(formatResult/2), !, formatResult(Result, FormattedResult). p_formatResult(Result, Result).
p_testResult("test", none, ExpectedResult) :- current_predicate(testResult/1), testResult(ExpectedResult).
p_testResult(Extension, none, ExpectedResult) :- current_predicate(testResult/2), testResult(Extension, ExpectedResult), string(Extension).
p_testResult(Extension, AuxData, ExpectedResult) :- current_predicate(testResult/2), testResult([Extension, AuxData], ExpectedResult).
p_testResult("test", AuxData, ExpectedResult) :- current_predicate(testResult/2), testResult(auxData(AuxData), ExpectedResult).
p_testResult(Extension, AuxData, ExpectedResult) :- current_predicate(testResult/3), testResult(Extension, AuxData, ExpectedResult).
p_finalize(Result) :- current_predicate(finalize/1) -> finalize(Result) ; true.
p_initDynamicTests :- current_predicate(initDynamicTests/0) -> initDynamicTests ; true.
p_hideResult :- current_predicate(hideResult/0).
p_notInlineResult :- \+ isAnsiXterm. p_notInlineResult :- current_predicate(notInlineResult/0).
p_inlineWrongResult :- isAnsiXterm, current_predicate(inlineWrongResult/0) ; \+ p_notInlineResult, p_hideExpectedResultForDiff.
p_hideExpectedResultForDiff :- isAnsiXterm, current_predicate(hideExpectedResultForDiff/0).

/* Misc useful utility functions */
productlist([], 1).
productlist([H|T], Product) :- productlist(T, TProduct), Product is H*TProduct.

mapsum(List, MapFunction, Sum) :- mapAndAggregate(MapFunction, List, sumlist, Sum).

mapAndAggregate(MapFunction, List, AggregateFunction, Result) :- maplist(MapFunction, List, Values), call(AggregateFunction, Values, Result).
mapAndAggregate(MapBiFunction, List1, List2, AggregateFunction, Result) :- maplist(MapBiFunction, List1, List2, Values), call(AggregateFunction, Values, Result).

list(Length, Generator, List) :- functor(Generator, _, 2), !, LengthZero is Length - 1, findall(Elem, (between(0, LengthZero, I), call(Generator, I, Elem)), List).
list(Length, Elem, List) :- list(Length, [_,Elem]>>true, List).