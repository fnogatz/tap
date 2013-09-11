% Tests which must generate failing TAP output
:- use_module(library(tap)).

% a failing test still runs the subsequent, passing one
9 =:= 7.
throw('surprise exception').
