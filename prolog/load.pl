
:- set_prolog_flag(double_quotes, string).
:- set_prolog_flag(unknown, warning).

:- use_module(htmlpages).
:- use_module(dbctrl).
:- use_module(dic_tree).
:- use_module(form).
:- use_module(impose).
:- use_module(basic_eq).
:- use_module(make_eq).
:- use_module(calc).
:- use_module(editor).
:- use_module(db).
:- use_module(vp52_compat).
:- use_module(cgitools).
:- use_module(russian).
:- use_module(tests).

main :-
  prompt(_, ''),
  current_prolog_flag(argv, [H|T]),
  protocol('protocol.txt'),
  define_db,
  editor:run(T).

tests :-
  protocol('protocol.txt'),
  define_db,
  editor:run([tests]).

dbg_tests :-
  guitracer,
  spy(tests/0),
  tests.

dbg :-
  guitracer,
  spy(main/0),
  spy(cgitools:cgi_GetParmList/1),
  main.

txtdbg :-
  spy(main/0),
  spy(editor:changeIdInConcept/2),
  main.

:- check.
