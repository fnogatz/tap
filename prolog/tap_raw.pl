:- module(tap_raw, [ tap_call/3
                   , tap_header/1
                   ]).

tap_header(TestCount) :-
    format('TAP version 13~n'),
    format('1..~d~n', [TestCount]).

tap_call(Head, Count0, Count) :-
    Head =.. [_|Options0],
    test_expectation(Options0, Expectation, _Options),
    run_test(Expectation, Head, Count0, Count).

run_test(ok, Test, Count0, Count) :-
    ( call(Test) ->
        test_result(ok, Test, Count0, Count)
    ; % otherwise ->
        test_result('not ok', Test, Count0, Count)
    ).
run_test(fails, Test, Count0, Count) :-
    ( call(Test) ->
        test_result('not ok', Test, Count0, Count)
    ; % otherwise ->
        test_result(ok, Test, Count0, Count)
    ).

test_result(Status, Test, N0, N) :-
    succ(N0, N),
    Test =.. [Name|_Options],
    format('~w ~w - ~w~n', [Status, N0, Name]).

test_expectation([], ok, []).
test_expectation([fails|Options], fails, Options) :- !.
test_expectation([todo|Options], todo, Options) :- !.
test_expectation([_|Options], Type) :-
    test_expectation(Options, Type).
