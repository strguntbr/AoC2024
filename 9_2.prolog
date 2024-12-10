day(9). testResult(2858).

:- use_module(lib/solve).

findFile([_|T], File) :- findFile(T, File).
findFile([file{no: No, size: Size}|_], file{no: No, size: Size}).

findSpace(_, [empty{size: Size}|T], Size, [], T) :- !.
findSpace(_, [empty{size: Size}|T], RequiredSize, [], [empty{size: RemainingSize}|T]) :- Size > RequiredSize, !, RemainingSize is Size - RequiredSize.
findSpace(No, [H|T], RequiredSize, [H|Before], After) :- H \= file{no: No, size: _}, findSpace(No, T, RequiredSize, Before, After).

deleteFile(_, [], []).
deleteFile(No, [file{no: No, size: Size}|T], [empty{size: EmptySize}|Tn]) :- !, (T = [empty{size: NextEmptySize}|Tn] -> EmptySize is Size + NextEmptySize ; [Tn, EmptySize] = [T, Size]).
deleteFile(No, [H|T], [H|Tn]) :- deleteFile(No, T, Tn).

moveFiles([], CompactedDiskMap, CompactedDiskMap).
moveFiles([file{no: No, size: Size}|OtherFiles], IntermediateDiskMap, CompactedDiskMap) :-
  findSpace(No, IntermediateDiskMap, Size, Before, After), !, deleteFile(No, After, NextAfter),
  append(Before, [file{no: No, size: Size}|NextAfter], NextDiskMap),
  moveFiles(OtherFiles, NextDiskMap, CompactedDiskMap).
moveFiles([_|H], IntermediateDiskMap, CompactedDiskMap) :- moveFiles(H, IntermediateDiskMap, CompactedDiskMap).

checksum(_, [], 0).
checksum(I, [empty{size: Size}|T], Checksum) :- In is I + Size, checksum(In, T, Checksum).
checksum(I, [file{no: No, size: Size}|T], Checksum) :- 
  In is I + Size, FileChecksum is (In*(In-1)-I*(I-1))/2*No,
  checksum(In, T, NextChecksum),
  Checksum is NextChecksum + FileChecksum.

result([DiskMap], Checksum) :-
  findall(File, findFile(DiskMap, File), Files),
  moveFiles(Files, DiskMap, CompactedDiskMap),
  checksum(0, CompactedDiskMap, Checksum).

/* required for loadData */
data_line(DiskMap, Line) :- string_chars(Line, Chars), maplist(atom_number, Chars, T), map(T, file, 0, DiskMap).

map([], _, _, []).
map([H|T], file, No, [file{no: No, size: H}|DiskMap]) :- map(T, empty, No, DiskMap).
map([0|T], empty, No, DiskMap) :- !, NextNo is No + 1, map(T, file, NextNo, DiskMap).
map([H|T], empty, No, [empty{size: H}|DiskMap]) :- NextNo is No + 1, map(T, file, NextNo, DiskMap).
