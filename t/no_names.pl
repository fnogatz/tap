% Tests whose name is the clause itself
:- use_module(library(tap)).

% some examples taken nearly from ISO §8.4.1.4
1.0 @< 1.
'1 \\== 1'(fail) :-
    1 \== 1.
aardvark @=< zebra.
'foo(X,a) @< foo(Y,b)'(todo) :-
    foo(_X,a) @< foo(_Y,b).
X @=< X.
X == X.
