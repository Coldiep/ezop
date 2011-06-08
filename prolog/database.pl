

:- module(database,[
        define_database/2,
        list_database/2,
        list_database_term/2,
        save_database/2,
        load_database/2,
        retract_database/1,
        retract_database/2,
        define_database_module/1,
        define_single/1,
        db_single/1,
        define_determ/1
]).

:- dynamic
  db_reg/2, db_mod/1, db_single/1.


% protable implementation of save





%save_database( FileName, DBName)

%consult_database( Filename, DBName)

def_db(_, []) :- !.

def_db(DbName, [H|PredList]) :-
        assert(db_reg(DbName, H)),
        def_db(DbName, PredList).


define_database_module(Mod) :-
         retractall(db_mod(_)),
         assert(db_mod(Mod)).


define_database(DbName, [H|T]) :- !,
        retractall(db_reg(DbName,_)),
        def_db(DbName, [H|T]).

list_database_term(DbName, Term) :-
        db_reg(DbName, Functor/Arity),
        length(List, Arity),
        apply(Functor, List),
        Term0 =.. [Functor|List],
        Term = Term0.

retract_database(DbName, Term) :-
%        var(Term),
        db_mod(Mod),
        list_database_term(DbName, Term),
         Mod:retractall(Term).

        


        
test_list(Functor, Arity, Term) :-
%        db_reg(DbName, Functor/Arity),
%        functor(Term, Functor, Arity),
        length(List, Arity),
        apply(Functor, List),
        Term =.. [Functor|List].
        



check_database(DbName) :-
                       db_reg(DbName, _) -> true ; throw(error('unknown database',DbName)).

list_database(DbName, List) :-
        findall(Y, db_reg(DbName, Y), List).


predicate_list(Functor, Arity, List) :-
        functor(Term, Functor, Arity),
        %write('functor: '), writeln(Functor/Arity),
        duplicate_term(Term, T), clause(T, true),
        %write('Term: '), writeln(Term),
        !,
        findall(Term,Term,List).



save_list(_, []) :- !.

save_list(Stream, [H|T]) :-
                  write_term(Stream, H, [quoted(true)]), write(Stream, '.\n'),
                  save_list(Stream, T).


save_db(DbName, Stream) :-
        db_reg(DbName, Functor/Arity),
        %write('db_reg: '), writeln(Functor/Arity),
        predicate_list(Functor, Arity, List),
        save_list(Stream, List),
        fail.
        
save_db(_,_) :-  !.
        

save_database(DbName, File) :- !,
        open(File, write, Stream), 
        save_db(DbName, Stream),
        close(Stream), !.


database_exists(DbName) :- !,
        bagof(_, db_reg(DbName, _), _).


retract_db_list(_DbName, []) :-  !.

retract_db_list(DbName, [H/A|T]) :-
         db_mod(Mod),
         functor(Term, H, A),
         Mod:retractall(Term),
         retract_db_list(DbName, T).


retract_database(DbName) :-
        check_database(DbName),
        list_database(DbName, List),
        %length(List, 0) -> throw('retract_database: unknown database') ;
        retract_db_list(DbName, List).





put_terms([], _).

put_terms([H|T], Mod) :-
                 %write('assert term:'), write(H), writeln(''),
                 assert(Mod:H),
                 put_terms(T, Mod).


load_database(DbName, File) :- !,
     database_exists(DbName), % -> true; throw('no database'),
     retract_database(DbName),
     save_database(DbName, 'load_database.txt'),
     % TODO: load via catch
     read_file_to_terms(File, Terms, [double_quotes(string)]),
% TODO Mod by default if
     db_mod(Mod),
     put_terms(Terms, Mod).


define_single([]) :- !.

define_single([H|T]) :-
     assert(db_single(H)),
     define_single(T).


define_determ(L) :- define_single(T).


