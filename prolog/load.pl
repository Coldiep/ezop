:- set_prolog_flag(double_quotes, string).
:- set_prolog_flag(unknown, warning).
%:- set_prolog_flag(encoding, octet).
%:- set_prolog_flag(verbose_file_search, true).

%:- file_search_path(lib, 'D:\\ezop-project\\WebEzop').

%:- preprocessor(_, 'D:\\ezop-project\\WebEzop\\vpi_pp.bat %f').
%:- assert(library_directory('D:\\ezop-project\\WebEzop')).
%:-     set_stream(user_input, tty(false)).
%:-     set_stream(user_output, tty(false)).
%:-     set_stream(current_output, tty(false)).



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
%     trace(v52_compat:readstring/2),
     protocol('protocol.txt'),
     define_db,
     editor:run(T).


tests :-
     protocol('protocol.txt'),
     define_db,
     editor:run([tests]).

dbg_tests :-
        guitracer,
        %:-spy(v52_compat:consult/2).
        spy(tests/0),
        tests.


dbg :-
        guitracer,
        %:-spy(v52_compat:consult/2).
        spy(main/0),
        %spy(impose:impose/5),
        %spy(impose:tokll/1),
        %spy(dbctrl:set_envTemplates/1),
        %spy(editor:changeIdInConcept/2),
        spy(cgitools:cgi_GetParmList/1),
        %trace(vp52_compat:readstring),
        main.


txtdbg :-
        spy(main/0),
        spy(editor:changeIdInConcept/2),
        main.



:- check.
