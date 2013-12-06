:- use_module(library(tap)).

'first test fails' :-
    fail.

'second test fails' :-
    fail.

'for good measure, let us have a third' :-
    fail.

% include an unnamed, failing test as well.
9 =:= 7.

'exception thrown when failure expected'(fail) :-
    throw('catch this').
