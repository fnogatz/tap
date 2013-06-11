:- module(tap, []).
:- reexport(library(tap_raw), [ tap_header/1, tap_call/3 ]).

% Thread a state variable through a list of predicates.  This is similar
% to a DCG expansion, but much simpler.
thread_state([], [], Out, Out).
thread_state([P0|Preds0], [P|Preds], In, Out) :-
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

% True if the current context implies that the user wants this
% term to be expanded as a test predicate.
term_wants_tap_expansion :-
    prolog_load_context(module, user).

:- dynamic test_case/1, user:main/0.
user:term_expansion((Head:-_), _) :-
    % collect test cases as each predicate is defined
    term_wants_tap_expansion,
    tap:assertz(test_case(Head)),
    fail.
user:term_expansion(end_of_file, _) :-
    % build main/0
    term_wants_tap_expansion,
    findall(tap_call(Head), tap:test_case(Head), Tests0),
    length(Tests0, TestCount),
    thread_state(Tests0, Tests, 1, _),
    xfy_list(',', Body, [tap_header(TestCount)|Tests]),
    user:assertz((main :- Body)),

    % undo all database side effects
    tap:retractall(test_case(_)),
    fail.
