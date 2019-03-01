:- use_module(library(tap)).

'throws an exception'(throws(oh_no)) :-
    throw(oh_no).

'exception annotated with error(_)'(error(oops)) :-
    throw(oops).
