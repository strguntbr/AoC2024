day(9). testResult(1928).

:- use_module(lib/solve).

findSpace([empty{size: Size}|T], [], Size, T) :- !.
findSpace([H|T], [H|Before], Size, After) :- findSpace(T, Before, Size, After).

isEmpty([]).
isEmpty([empty{size: _}|T]) :- isEmpty(T).

removeFile([file{no: No, size: Size}|T], MaxSize, [], [file{no: No, size: Size}]) :- Size = MaxSize, isEmpty(T), !.
removeFile([file{no: No, size: Size}|T], MaxSize, [], [file{no: No, size: Size}, empty{size: RemainingSize}]) :- Size < MaxSize, isEmpty(T), !, RemainingSize is MaxSize - Size.
removeFile([file{no: No, size: Size}|T], MaxSize, [file{no: No, size: RemainingSize}], [file{no: No, size: MaxSize}]) :- MaxSize < Size, isEmpty(T), !, RemainingSize is Size - MaxSize.
removeFile([H|T], MaxSize, [H|Tn], File) :- removeFile(T, MaxSize, Tn, File).

moveFiles(DiskMap, CompactedDiskMap) :-
  findSpace(DiskMap, BeforeSpace, EmptySize, AfterSpace),
  removeFile(AfterSpace, EmptySize, AfterFile, File), !,
  append([BeforeSpace, File, AfterFile], NextDiskMap),
  moveFiles(NextDiskMap, CompactedDiskMap).
moveFiles(DiskMap, DiskMap).

checksum(_, [], 0).
checksum(I, [empty{size: Size}|T], Checksum) :- In is I + Size, checksum(In, T, Checksum).
checksum(I, [file{no: No, size: Size}|T], Checksum) :- 
  In is I + Size, FileChecksum is (In*(In-1)-I*(I-1))/2*No,
  checksum(In, T, NextChecksum),
  Checksum is NextChecksum + FileChecksum.

result([DiskMap], Checksum) :-
  moveFiles(DiskMap, CompactedDiskMap),
  checksum(0, CompactedDiskMap, Checksum).

/* required for loadData */
data_line(DiskMap, Line) :- string_chars(Line, Chars), maplist(atom_number, Chars, T), map(T, file, 0, DiskMap).

map([], _, _, []).
map([H|T], file, No, [file{no: No, size: H}|DiskMap]) :- map(T, empty, No, DiskMap).
map([0|T], empty, No, DiskMap) :- !, NextNo is No + 1, map(T, file, NextNo, DiskMap).
map([H|T], empty, No, [empty{size: H}|DiskMap]) :- NextNo is No + 1, map(T, file, NextNo, DiskMap).
