:- module(tap_expand, []).

:- use_module(library(tap/raw), [term_wants_tap_expansion/0]).

:- dynamic test_case/1.
user:term_expansion((Head:-_), _) :-
    % collect test cases with explicit names
    term_wants_tap_expansion,
    tap:assertz(test_case(Head)),
    fail.  % do no real expansion
user:term_expansion(Clause, (Head :- Clause)) :-
    % collect test cases whose name is the test content
    term_wants_tap_expansion,
    \+ functor(Clause, :-, _),
    Clause \== end_of_file,
    format(atom(Head), "~w", [Clause]),
    writeln(Clause),
    tap:assertz(test_case(Head)).
