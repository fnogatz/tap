:- module(tap, []).
:- reexport(library(tap/raw), [tap_header/1,tap_footer/3,tap_call/3,tap_call/1,diag/2,register_test/1]).

:- use_module(library(tap/main)).
:- use_module(library(tap/expand)).
