:- module(screen, [
              string_screen/2,
              string_screen/4
          ]).

/* Library to convert between a "screen" and a string. A screen is a series letters displayed in a 6x5 matrix each    */
/* where the last row of the matrix is always empty. Thus the letter itself is 6x4 pixels.                            */
/* This encoding seems to be used sometimes in Advent of Code puzzles to display the solution.                        */
/* Please note that this encoding only supports uppercase letters and does not support all 26 letter of the alphabet, */
/* because e.g. a 'W' cannot be displayed properly on a 6x4 pixel matrix. I added all letters I encountered so far.   */
/* These are currently: A, B, C, E, F, G, H, J, K, L, P, R, U, Z. If the screen or string contains any other letters  */
/* the conversion will fail.                                                                                  */
/*                                                                                                                    */
/* Use 'string_screen(String, Screen)' to convert. Either String or Screen must be instantiated.                      */
/* By default a lit pixel is represented by a 'X' character and a dark pixel is represented by a space character. If  */
/* you need different representations for dark/lit pixels use 'string_screen(String, Dark, Lit, Screen').             */
/* If you convert from screen to text the value for Dark and Lit must not be specified as they will be deducted       */
/* correctly from the screen. Thus you can simply call 'string_screen(String, _, _, Screen)' or just                  */
/* 'string_screen(String, Screen)' if Screen is instanciated.                                                         */
/* It is also possible to convert partial instanciated screens. E.g.                                                  */
/* 'string_screen(String, [[o,o,o,.,.],[o,.,.,o,.],[o,.,.,o,.],[o,_,_,_,_],[o,_,_,_,_],_])' will return 'P' and 'R'   */
/* for String.                                                                                                        */

string_screen(String, Screen) :- string_screen(String, ., #, Screen), !.
string_screen(String, Screen) :- nonvar(Screen), string_screen(String, _, _, Screen).
string_screen(String, Dark, Lit, Screen) :-
  nonvar(String), !,
  string_transposedscreen(String, TransposedScreen),
  transpose_(TransposedScreen, ConvertedScreen),
  convertscreen(ConvertedScreen, ., Dark, #, Lit, Screen).
string_screen(String, Dark, Lit, Screen) :-
  is_list(Screen), validscreen(Screen),
  convertscreen(Screen, Dark, ., Lit, #, ConvertedScreen),
  transpose_(ConvertedScreen, TransposedScreen),
  string_transposedscreen(String, TransposedScreen).

validscreen([L1,L2,L3,L4,L5,L6]) :- length(L1, L), length(L2, L), length(L3, L), length(L4, L), length(L5, L), length(L6, L), 0 is mod(L, 5). 

convertpixel(Dark, Dark, ConvertedDark, _, _, ConvertedDark).
convertpixel(Lit, _, _, Lit, ConvertedLit, ConvertedLit).
convertline([], _, _, _, _, []).
convertline([Pixel1|OtherPixels], Dark, ConvertedDark, Lit, ConvertedLit, [ConvertedPixel1|ConvertedOtherPixels]) :-
  convertpixel(Pixel1, Dark, ConvertedDark, Lit, ConvertedLit, ConvertedPixel1),
  convertline(OtherPixels, Dark, ConvertedDark, Lit, ConvertedLit, ConvertedOtherPixels).
convertscreen([], _, _, _, _, []).
convertscreen([Line1|OtherLines], Dark, ConvertedDark, Lit, ConvertedLit, [ConvertedLine1|ConvertedOtherLines]) :-
  convertline(Line1, Dark, ConvertedDark, Lit, ConvertedLit, ConvertedLine1),
  convertscreen(OtherLines, Dark, ConvertedDark, Lit, ConvertedLit, ConvertedOtherLines).

string_transposedscreen("", []).
string_transposedscreen(Text, [R1,R2,R3,R4,R5|RemainingScreen]) :-
  nonvar(Text), !,
  sub_string(Text, 0, 1, _, FirstChar), sub_string(Text, 1, _, 0, OtherText),
  char_screen(FirstChar, [R1,R2,R3,R4,R5]),
  string_transposedscreen(OtherText, RemainingScreen).
string_transposedscreen(Text, Screen) :-
  nonvar(Screen), Screen = [R1,R2,R3,R4,R5|RemainingScreen],
  char_screen(Char, [R1,R2,R3,R4,R5]),
  string_transposedscreen(RemainingText, RemainingScreen),
  string_concat(Char, RemainingText, Text).

char_screen(" ", [[.,.,.,.,.,.],
                  [.,.,.,.,.,.],
                  [.,.,.,.,.,.],
                  [.,.,.,.,.,.],
                  [.,.,.,.,.,.]]) :- !.

char_screen("A", [[.,#,#,#,#,#],
                  [#,.,.,#,.,.],
                  [#,.,.,#,.,.],
                  [.,#,#,#,#,#],
                  [.,.,.,.,.,.]]) :- !.

char_screen("B", [[#,#,#,#,#,#],
                  [#,.,#,.,.,#],
                  [#,.,#,.,.,#],
                  [.,#,.,#,#,.],
                  [.,.,.,.,.,.]]) :- !.

char_screen("C", [[.,#,#,#,#,.],
                  [#,.,.,.,.,#],
                  [#,.,.,.,.,#],
                  [.,#,.,.,#,.],
                  [.,.,.,.,.,.]]) :- !.

char_screen("O", [[#,#,#,#,#,#],
                  [#,.,.,.,.,#],
                  [#,.,.,.,.,#],
                  [.,#,#,#,#,.],
                  [.,.,.,.,.,.]]) :- !. /* Just a guess. Did not see any D yet */

char_screen("E", [[#,#,#,#,#,#],
                  [#,.,#,.,.,#],
                  [#,.,#,.,.,#],
                  [#,.,.,.,.,#],
                  [.,.,.,.,.,.]]) :- !.

char_screen("F", [[#,#,#,#,#,#],
                  [#,.,#,.,.,.],
                  [#,.,#,.,.,.],
                  [#,.,.,.,.,.],
                  [.,.,.,.,.,.]]) :- !.

char_screen("G", [[.,#,#,#,#,.],
                  [#,.,.,.,.,#],
                  [#,.,.,#,.,#],
                  [.,#,.,#,#,#],
                  [.,.,.,.,.,.]]) :- !.

char_screen("H", [[#,#,#,#,#,#],
                  [.,.,#,.,.,.],
                  [.,.,#,.,.,.],
                  [#,#,#,#,#,#],
                  [.,.,.,.,.,.]]) :- !.

char_screen("I", [[.,.,.,.,.,.],
                  [#,.,.,.,.,#],
                  [#,#,#,#,#,#],
                  [#,.,.,.,.,#],
                  [.,.,.,.,.,.]]) :- !. /* According to https://github.com/bsoyka/advent-of-code-ocr/blob/main/advent_of_code_ocr/characters.py */

char_screen("J", [[.,.,.,.,#,.],
                  [.,.,.,.,.,#],
                  [#,.,.,.,.,#],
                  [#,#,#,#,#,.],
                  [.,.,.,.,.,.]]) :- !.

char_screen("K", [[#,#,#,#,#,#],
                  [.,.,#,.,.,.],
                  [.,#,.,#,#,.],
                  [#,.,.,.,.,#],
                  [.,.,.,.,.,.]]) :- !.

char_screen("L", [[#,#,#,#,#,#],
                  [.,.,.,.,.,#],
                  [.,.,.,.,.,#],
                  [.,.,.,.,.,#],
                  [.,.,.,.,.,.]]) :- !.

char_screen("O", [[.,#,#,#,#,.],
                  [#,.,.,.,.,#],
                  [#,.,.,.,.,#],
                  [.,#,#,#,#,.],
                  [.,.,.,.,.,.]]) :- !. /* According to https://github.com/bsoyka/advent-of-code-ocr/blob/main/advent_of_code_ocr/characters.py */

char_screen("P", [[#,#,#,#,#,#],
                  [#,.,.,#,.,.],
                  [#,.,.,#,.,.],
                  [.,#,#,.,.,.],
                  [.,.,.,.,.,.]]) :- !.

char_screen("R", [[#,#,#,#,#,#],
                  [#,.,.,#,.,.],
                  [#,.,.,#,#,.],
                  [.,#,#,.,.,#],
                  [.,.,.,.,.,.]]) :- !.

char_screen("S", [[.,#,#,.,.,#],
                  [#,.,.,#,.,#],
                  [#,.,.,#,.,#],
                  [#,.,.,.,#,.],
                  [.,.,.,.,.,.]]) :- !. /* According to https://github.com/bsoyka/advent-of-code-ocr/blob/main/advent_of_code_ocr/characters.py */

char_screen("Y", [[#,#,.,.,.,.],
                  [.,.,#,.,.,.],
                  [.,.,.,#,#,#],
                  [.,.,#,.,.,.],
                  [.,.,.,.,.,.]]) :- !. /* According to https://github.com/bsoyka/advent-of-code-ocr/blob/main/advent_of_code_ocr/characters.py */

char_screen("Z", [[#,.,.,.,#,#],
                  [#,.,.,#,.,#],
                  [#,.,#,.,.,#],
                  [#,#,.,.,.,#],
                  [.,.,.,.,.,.]]) :- !.

transpose_(Matrix, []) :- list_to_set(Matrix, [[]]), !.
transpose_(Matrix, [FirstRow|OtherTransposedRows]) :- maplist(head_tail, Matrix, FirstRow, OtherRows), transpose_(OtherRows, OtherTransposedRows).

head_tail([H|T], H, T).

/* output */
writeline([]) :- !, writeln("").
writeline([FirstPixel|OtherPixels]) :- !, write(FirstPixel), writeline(OtherPixels).

writescreen([]) :- !.
writescreen([FirstLine|OtherLines]) :- !, writeline(FirstLine), writescreen(OtherLines).