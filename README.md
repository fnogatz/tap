# Synopsis

    :- use_module(to_be_tested).
    % define helper predicates here

    :- use_module(library(tap)).
    % define test predicates here
    
    'two plus two is four' :-
        4 is 2+2.
    
    'zero not equal to one'(fail) :-
        0 =:= 1.
    
    6 is 3*2.

Run tests with standard TAP tools like prove:

    $ prove -v -e 'swipl -q -t main -f' t/example.pl
    TAP version 13
    1..3
    ok 1 - two plus two is four
    ok 2 - zero not equal to one
    ok 3 - 6 is 3*2
    ok

# Description

The [Test Anything Protocol](http://testanything.org/) is a text-based
interface between test scripts and a test harness.  A wide range of
tools exist for running, rendering and analyzing test results.  By
writing your Prolog tests with TAP, you get access to all this
testing infrastructure.  For example,
[interactive HTML output](http://www.spurkis.org/TAP-Formatter-HTML/test-output.html).

TAP tests traditionally reside in a t/ directory in your project's
root.  Each file beneath t/ encapsulates a collection of tests related
to a specific topic. During development, one can run all test files or
just an interesting subset.  In its most basic form, a test file is a
script which generates TAP output.  library(tap) helps you
write these scripts.

To write a test file with library(tap), load all code that you'll need
for testing.  Define any helper predicates.  Then load library(tap).
All predicates defined after loading library(tap) are considered test
cases.  The predicate's name is the test name.  By default, a
predicate must succeed without leaving any choicepoints for the test
to pass.  See Arguments section below to change that behavior.

For small tests (see `6 is 3*2` above), the name can be omitted.  The
test body is then used as the test name.

library(tap) does not yet support the entire TAP specification and is
missing many features found in PlUnit.  Both are temporary shortcomings.
I expect the library to fill these gaps eventually.

# Arguments

A test predicate can optionally include arguments to change TAP's
expectations about the test.  Arguments look like this:

    'test with arguments'(Arg1, Arg2, ...) :-
        ...

Acceptable arguments are:

  * `error(E)` - same as `throws(E)`. Supported for symmetry with PlUnit.
  * `fail` - test is expected to fail
  * `fixme(Reason)` - same as `todo(Reason)`. Supported for symmetry with PlUnit.
  * `todo(Reason)` - test is known to fail but report it in TAP output as "TODO Reason".  TAP tools treat these tests differently.
  * `todo` - same as `todo('')`
  * `throws(E)` - throws exception `E`

# Macros that write tests

It's common for each test case in a test file to follow a similar pattern.
For example, we might have tests for the length/2 predicate:

    :- use_module(library(tap)).
    'length([a,b,c],3)' :-
        length([a,b,c], N),
        N = 3.
    'length([a,b],2)' :-
        length([a,b], N),
        N = 2.
    ...

Because of all the similarity, that's tedious to write and tedious to
read.  We can factor out the redundancy by creating a macro:

    % ... macro goes here ...
    
    :- use_module(library(tap)).
    [a,b,c] -> 3.
    [a,b] -> 2.

That's much better.  A regular term_expansion/2 macro that calls
tap:register_test/1 does the job:

    term_expansion(List -> Length, (Head :- Test)) :-
    format(atom(Head), 'length(~w, ~w)', [List, Length]),
    Test = (
        length(List, Len),
        Len = Length
    ),
    tap:register_test(Head)

Without registering, our nicely constructed test case won't run.  Macros are
especially convenient when testing multiple modes of a single predicate.  You
can decribe the relationship once and have the macro write a separate test case
for each mode.

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(tap).

Source code available and pull requests accepted at
https://github.com/mndrix/tap

This module uses [semantic versioning](http://semver.org/).

@author Michael Hendricks <michael@ndrix.org>
@license BSD
