:- use_module(library(tap)).

'failing todo test without reason'(todo) :-
    fail.

'failing todo test with reason'(todo(reason)) :-
    fail.


'passing todo test without reason'(todo) :-
    true.

'passing todo test with reason'(todo(reason)) :-
    true.


'todo test that throws an exception'(todo) :-
    throw('oops').
