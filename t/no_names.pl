% Tests whose name is the clause itself
:- use_module(library(tap)).
:- style_check(-no_effect).

% some examples taken nearly from ISO §8.4.1.4
1.0 @< 1.
'1 \\== 1'(fail) :-
    1 \== 1.
aardvark @=< zebra.
foo(_X,a) @< foo(_Y,b).
X @=< X.
X == X.
