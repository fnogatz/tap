:- module(tap_main, []).

:- use_module(library(tap/raw), [
    tap_state/1,
    term_wants_tap_expansion/0
]).
:- use_module(library(lists), [append/3]).

:- dynamic user:main/0.
user:term_expansion(end_of_file, _) :-
    % build main/0
    term_wants_tap_expansion,
    prolog_load_context(script, true),
    findall(tap_call(Head), tap:test_case(Head), Tests0),
    length(Tests0, TestCount),
    tap_state(State0),
    thread_state(Tests0, Tests1, State0, State),
    append(Tests1, [tap_raw:tap_footer(TestCount, State0, State)], Tests2),
    xfy_list(',', Body, [tap_raw:tap_header(TestCount)|Tests2]),
    user:assertz((main :- Body)),

    % undo all database side effects
    tap:retractall(test_case(_)),
    fail.

% Thread a state variable through a list of predicates.  This is similar
% to a DCG expansion, but much simpler.
thread_state([], [], Out, Out).
thread_state([P0|Preds0], [tap_raw:P|Preds], In, Out) :-
    P0 =.. [Functor|Args],
    append(Args, [In, Tmp], NewArgs),
    P =.. [Functor|NewArgs],
    thread_state(Preds0, Preds, Tmp, Out).

% Identical to list_util:xfy_list/3.  Copied here so that library(tap)
% can have no pack dependencies.  That lets other packs use library(tap)
% without circular dependencies.
xfy_list(Op, Term, [Left|List]) :-
    Term =.. [Op, Left, Right],
    xfy_list(Op, Right, List),
    !.
xfy_list(_, Term, [Term]).
