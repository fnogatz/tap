term_expansion(List -> Length, (Name :- Test)) :-
    format(atom(Name), 'length(~w, ~w)', [List, Length]),
    Test = (
        length(List, Len),
        Len = Length
    ),
    tap:register_test(Name).

:- use_module(library(tap)).

[a,b] -> 2.
[a,b,c] -> 3.
[] -> 0.
