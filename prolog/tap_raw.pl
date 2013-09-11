:- module(tap_raw, [ tap_call/1
                   , tap_call/3
                   , tap_header/1
                   , tap_state/1
                   ]).

%% tap_header(+TestCount:integer) is det.
%
%  Output a TAP header.  This includes the supported
%  TAP version and the number of tests we expect to run.
tap_header(TestCount) :-
    format('TAP version 13~n'),
    format('1..~d~n', [TestCount]).


%% tap_call(+Head, +State0, -State) is det.
%
%  Calls Head as a test case and generates TAP output for
%  the results.  State0 and State are opaque state used for
%  generating correct TAP output.
%
%  See tap_state/1 and tap_call/1
tap_call(Head, Count0, Count) :-
    Head =.. [_|Options0],
    test_expectation(Options0, Expectation, _Options),
    run_test(Expectation, Head, Count0, Count).

% Call Goal and bind Ending to explain how it turned out.
% If Goal fails, call_ending fails too.
% `Ending=det` if Goal succeeded without choicepoints.
% `Ending=choicepoints` if Goal succeeded and left choicepoints.
% `Ending=exception(E)` if threw an exception.
call_ending(Goal, Ending) :-
    catch( call_cleanup(Goal,Cleanup=det)
         , Exception
         , Cleanup=exception(Exception)
         ),
    ( var(Cleanup) -> Ending=choicepoints ; Ending=Cleanup ),
    !.  % cut any choicepoints left by Goal, after checking Cleanup


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
tap_state(1).

% Run a single test, generating TAP output based on results
% and expectations.
run_test(ok, Test, Count0, Count) :-
    ( call_ending(Test, Ending) ->
        ( Ending = det ->
            test_result(ok, Test, Count0, Count)
        ; Ending = choicepoints ->
            test_result( 'not ok'
                       , Test
                       , 'left unexpected choice points'
                       , Count0
                       , Count
                       )
        ; % otherwise ->
            test_result('not ok', Test, Ending, Count0, Count)
        )
    ; % call failed ->
        test_result('not ok', Test, failed, Count0, Count)
    ).
run_test(fail, Test, Count0, Count) :-
    ( call_ending(Test, _) ->
        test_result('not ok', Test, Count0, Count)
    ; % otherwise ->
        test_result(ok, Test, Count0, Count)
    ).
run_test(todo(Reason), Test, Count0, Count) :-
    format(atom(Todo), 'TODO ~w', [Reason]),
    ( call_ending(Test, Ending), Ending=det ->
        test_result(ok, Test, Todo, Count0, Count)
    ; % otherwise ->
        test_result('not ok', Test, Todo, Count0, Count)
    ).
run_test(throws(E), Test, Count0, Count) :-
    ( call_ending(Test,exception(E)) ->
        test_result(ok, Test, Count0, Count)
    ; % otherwise ->
        test_result('not ok', Test, Count0, Count)
    ).

% Helper for generating a single TAP result line
test_result(Status,Test,N0,N) :-
    test_result(Status,Test,_,N0,N).
test_result(Status, Test, Comment, N0, N) :-
    succ(N0, N),
    Test =.. [Name|_Options],
    ( var(Comment) ->
        format('~w ~w - ~w~n', [Status, N0, Name])
    ; % otherwise ->
        format('~w ~w - ~w # ~w~n', [Status, N0, Name, Comment])
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
