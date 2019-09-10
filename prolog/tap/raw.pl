:- module(tap_raw, [ tap_call/1
                   , tap_call/3
                   , tap_header/1
                   , tap_footer/3
                   , tap_state/1
                   , diag/2
                   , is_test_running/0
                   , term_wants_tap_expansion/0
                   , register_test/1
                   ]).

%% tap_header(+TestCount:integer) is det.
%
%  Output a TAP header.  This includes the supported
%  TAP version and the number of tests we expect to run.
tap_header(TestCount) :-
    format('TAP version 13~n'),
    format('1..~d~n', [TestCount]).


%% tap_footer(+TestCount:integer, +StartState, +EndState) is det.
%
%  Output a TAP footer.  This includes the number of 
%  run, passed, and possibly failing tests.
tap_footer(TestCount, state(_,_,Time0), state(_,PassedCount1,Time1)) :-
    format('~n'),
    Duration is (Time1-Time0)*1000,
    format('# time=~1fms~n', [Duration]),
    format('# tests ~d~n', [TestCount]),
    format('# pass  ~d~n', [PassedCount1]),
    ( PassedCount1 < TestCount ->
        FailedCount is TestCount-PassedCount1,
        format('# fail  ~d~n', [FailedCount])
    ; % otherwise ->
        true
    ).


%% tap_call(+Head, +State0, -State) is det.
%
%  Calls Head as a test case and generates TAP output for
%  the results.  State0 and State are opaque state used for
%  generating correct TAP output.
%
%  See tap_state/1 and tap_call/1
tap_call(Head, State0, State) :-
    Head =.. [_|Options0],
    test_expectation(Options0, Expectation, _Options),
    setup_call_cleanup(
        assertz(is_test_running,Ref),
        run_test(Expectation, Head, State0, State),
        erase(Ref)
    ).

% Call Goal and bind Ending to explain how it turned out.
% The predicate always succeeds.
% `Ending=fail` if Goal failed.
% `Ending=det` if Goal succeeded without choicepoints.
% `Ending=choicepoints` if Goal succeeded and left choicepoints.
% `Ending=exception(E)` if threw an exception.
call_ending(Goal, Ending) :-
    catch( call_cleanup(Goal,Cleanup=det)
         , Exception
         , Cleanup=exception(Exception)
         ),
    ( var(Cleanup) -> Ending=choicepoints ; Ending=Cleanup ),

    % cut any choicepoints left by Goal, after checking Cleanup.
    % also cut second clause of call_ending/2
    !.
call_ending(_, fail).


%% tap_call(+Head) is det.
%
%  Like tap_call/3 but automatically generates a State.
%  This is helpful for running a single test predicate
%  from the toplevel.
tap_call(Head) :-
    tap_state(State),
    tap_call(Head, State, _).

%% tap_state(-State) is det.
%
%  Unifies State with an opaque, starting state.
%  You should almost never need to call this directly.
%  Use tap_call/1 instead.
tap_state(state(1,0,Time)) :-
    get_time(Time).

% Run a single test, generating TAP output based on results
% and expectations.
run_test(ok, Test, State0, State) :-
    call_ending(Test, Ending),
    ( Ending = det ->
        test_result(ok, Test, State0, State)
    ; Ending = choicepoints ->
        test_result('not ok', Test, 'left unexpected choice points', State0, State)
    ; % otherwise ->
        test_result('not ok', Test, Ending, State0, State)
    ).
run_test(fail, Test, State0, State) :-
    call_ending(Test, Ending),
    ( Ending = fail ->
        test_result(ok, Test, State0, State)
    ; % otherwise ->
        test_result('not ok', Test, State0, State)
    ).
run_test(todo(Reason), Test, State0, State) :-
    format(atom(Todo), 'TODO ~w', [Reason]),
    call_ending(Test, Ending),
    ( Ending=det ->
        test_result(ok, Test, Todo, State0, State)
    ; % otherwise ->
        test_result('not ok', Test, Todo, State0, State)
    ).
run_test(throws(E), Test, State0, State) :-
    call_ending(Test,Ending),
    ( Ending = exception(E) ->
        test_result(ok, Test, State0, State)
    ; % otherwise ->
        test_result('not ok', Test, State0, State)
    ).

% Helper for generating a single TAP result line
test_result(Status,Test,State0,State) :-
    test_result(Status,Test,_,State0,State).
test_result(Status, Test, Comment, State0, State) :-
    State0 = state(Count0,Passed0,_Time0),
    succ(Count0,Count),
    State = state(Count,Passed,Time),
    get_time(Time),
    ( Status = ok ->
        succ(Passed0, Passed)
    ; % otherwise ->
        Passed0 = Passed
    ),
    Test =.. [Name|_Options],
    ( var(Comment) ->
        format('~w ~w - ~w~n', [Status, Count0, Name])
    ; % otherwise ->
        format('~w ~w - ~w # ~w~n', [Status, Count0, Name, Comment])
    ).

% Determine the expected result based on a test predicate's arguments
test_expectation([], ok, []).
test_expectation([fail|Options], fail, Options) :- !.
test_expectation([todo|Options], todo(''), Options) :- !.
test_expectation([todo(Reason)|Options], todo(Reason), Options) :- !.
test_expectation([fixme(Reason)|Options], todo(Reason), Options) :- !.
test_expectation([throws(E)|Options], throws(E), Options) :- !.
test_expectation([error(E)|Options], throws(E), Options) :- !.
test_expectation([_|Options], Type) :-
    test_expectation(Options, Type).


% True if the current context implies that the user wants this
% term to be expanded as a test predicate.
term_wants_tap_expansion :-
    prolog_load_context(module, user).


%% is_test_running is semidet.
%
%  True if a TAP test is in progress.  It's true for all goals inside
%  the dynamic scope of a TAP test.  See also diag/2.
:- dynamic is_test_running/0.


%% diag(+Format,+Args) is det.
%
%  Like debug/3 for TAP tests.  When a TAP test is running
%  (see is_test_running/0) sends a diagnostic message to the TAP output.
%  It behaves as a noop in other circumstances.  Format and Args are
%  passed through to format/2.
diag(Format,Args) :-
    is_test_running,
    !,
    with_output_to(user_error, (
        write('# '),
        format(Format,Args),
        nl
    )).
diag(_,_).


register_test(Head) :-
    tap:assertz(test_case(Head)).
