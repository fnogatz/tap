:- prolog_load_context(directory, TestDir),
   absolute_file_name('..', Root, [relative_to(TestDir), file_type(directory)]),
   pack_attach(Root,[]).
:- consult('examples.pl').
