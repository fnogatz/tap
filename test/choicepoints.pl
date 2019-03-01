test_me(a).
test_me(b).

:- use_module(library(tap)).
'leaves choicepoints'(todo('Should never succeed')) :-
    test_me(_).
