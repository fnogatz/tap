:- module(tap_raw, [ tap_header/1
                   ]).

tap_header(TestCount) :-
    format('TAP version 13~n'),
    format('1..~d~n', [TestCount]).
