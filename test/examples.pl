:- use_module(library(tap)).

'simplest possible test case' :-
    true.

'simplest failing test case'(fail) :-
    fail.

'generates a diagnostic message' :-
    diag("I'm a ~s message", ["diagnostic"]).
