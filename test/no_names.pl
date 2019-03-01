% Tests whose name is the clause itself
is_even(X) :-
    0 is X mod 2.

:- use_module(library(tap)).
:- style_check(-no_effect).

is_even(4).
\+ is_even(5).

% some examples taken nearly from ISO §8.4.1.4
1.0 @< 1.
'1 \\== 1'(fail) :-
    1 \== 1.
aardvark @=< zebra.
foo(_X,a) @< foo(_Y,b).
X @=< X.
X == X.
