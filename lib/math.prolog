:- module(matrix, [
  gcd/3,
  lcm/3,
  lcmall/2
]).

gcd(0, GCD, GCD) :- !.
gcd(X, Y, GCD) :- X < Y, NextY is Y mod X, gcd(X, NextY, GCD).
gcd(X, Y, GCD) :- X > Y, gcd(Y, X, GCD).

lcm(X, Y, LCM):-gcd(X, Y, GCD), LCM is X*Y/GCD.

lcmall([H], H).
lcmall([H|T], LCM) :- lcmall(T, LN), lcm(H, LN, LCM).
