


:-module(tests,[
run_parser_tests/0
]).

:- style_check(+string).
:- use_module(database).
:- set_prolog_flag(double_quotes, string).




test_impose(S) :- !,
write("test:"), write(S), write("\n"),
  impose_all(S,Term,Type),
write(Term), write("\n").


run_parser_tests :-
write("runing parser tests..."),write("\n"),
consult("kernel.tmt",templates), %загрузка шаблонов
consult("init.ntn", concept), %загрузка основной среды
test_impose("1+2?"),
test_impose("Какие противоречия?"),
test_impose("какие противоречия?").
