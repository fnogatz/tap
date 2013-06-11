%:- use_module(to_be_tested).
% define helper predicates here

:- use_module(library(tap)).
% define test predicates here
'two plus two is four' :-
    4 is 2+2.
'zero not equal to one'(fails) :-
    0 =:= 1.
