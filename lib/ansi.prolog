:- module(ansi, [
        isAnsiXterm/0,
        green/2,
        red/2,
        yellow/2,
        white/2,
        greenbg/2,
        redbg/2,
        yellowbg/2,
        moveCursor/2,
        cursorPosition/1
    ]).

/* ANSI XTERM utlitly methods */
green(Text, ColoredText) :- isAnsiXterm, !, format(atom(ColoredText), '\033[0;32m~w\033[0m', [Text]).
green(Text, Text).
red(Text, ColoredText) :- isAnsiXterm, !, format(atom(ColoredText), '\033[0;31m~w\033[0m', [Text]).
red(Text, Text).
yellow(Text, ColoredText) :- isAnsiXterm, !, format(atom(ColoredText), '\033[0;33m~w\033[0m', [Text]).
yellow(Text, Text).
white(Text, ColoredText) :- isAnsiXterm, !, format(atom(ColoredText), '\033[1;37m~w\033[0m', [Text]).
white(Text, Text).

greenbg(Text, ColoredText) :- isAnsiXterm, !, format(atom(ColoredText), '\033[30m\033[42m~w\033[0m', [Text]).
greenbg(Text, Text).
redbg(Text, ColoredText) :- isAnsiXterm, !, format(atom(ColoredText), '\033[30m\033[41m~w\033[0m', [Text]).
redbg(Text, Text).
yellowbg(Text, ColoredText) :- isAnsiXterm, !, format(atom(ColoredText), '\033[30m\033[43m~w\033[0m', [Text]).
yellowbg(Text, Text).

moveCursor(Distance, Direction) :- isAnsiXterm -> moveCursor_(Distance, Direction) ; true.
moveCursor_(0, _) :- !.
moveCursor_(Distance, 'up') :- format('\033[~dA', [Distance]).
moveCursor_(Distance, 'down') :- format('\033[~dB', [Distance]).
moveCursor_(Distance, 'right') :- format('\033[~dC', [Distance]).
moveCursor_(Distance, 'left') :- format('\033[~dD', [Distance]).

cursorPosition(Position) :-
  isAnsiXterm,
  csi('[6n'), readResponse(Response), !,
  string_codes(ResponseStr, Response),
  split_string(ResponseStr, ";", "", [_, PosStr]),
  number_codes(Position, PosStr).
cursorPosition(0).

readResponse(Response) :-
  get_single_char(C),
  (
    data(C) -> readResponse(ResponseN), Response = [C|ResponseN] 
    ; startCode(C) -> readResponse(Response)
    ; Response = []
  ).

data(C) :- digit(C) ; semicolon(C).
digit(C) :- between(48, 57, C).
semicolon(59).
startCode(27). startCode(91).

csi(Sequence) :- isAnsiXterm -> format('\033~w', [Sequence]) ; true.
csi(Target, Sequence) :- isAnsiXterm -> format(Target, '\033~w', [Sequence]) ; format(Target, '', []).

isAnsiXterm :- stream_property(current_output, tty(true)), current_prolog_flag(color_term, true).
