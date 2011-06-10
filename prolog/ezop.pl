
% Библиотечный модуль проекта ЭЗОП.
:- module(ezop,[get_onto_list/4]).

:- encoding(utf8).
:- style_check(+string).
:- set_prolog_flag(double_quotes, string).

% Используем модули.
%:- use_module(htmlpages).
%:- use_module(dbctrl).
%:- use_module(dic_tree).
%:- use_module(form).
%:- use_module(impose).
%:- use_module(basic_eq).
%:- use_module(make_eq).
%:- use_module(calc).
%:- use_module(editor).
%:- use_module(db).
%:- use_module(vp52_compat).
%:- use_module(cgitools).
%:- use_module(russian).
%:- use_module(tests).

% Возвращает список онтологий системы.
get_onto_list(Name, Id, Parent, Content) :-
  style_check(+string),
  set_prolog_flag(double_quotes, string),
  set_prolog_flag(encoding, utf8),
  consult("base/onto.ezp"),
  name_file(Name, Id, Parent, Content).

