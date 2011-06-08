
:-module(vp52_compat,[
substring/4,
searchstring/3,
frontstr/4,
concat/3,
str_int/2,
str_real/2,
str_char/2,
subchar/3,
bound/1,
free/1,
format/7,
format/6,
format/5,
format/4,
format/3,
str_len/2,
searchchar/3,
term_str/3,
openwrite/2,
openread/2,
writedevice/1,
readdevice/1,
closefile/1,
writef/2,
writef/3,
writef/4,
readterm/2,
retractall/2,
retract/2,
random/2,
filenamepath/3,
trap/3,
existfile/1,
consult/2,
marktime/2,
timeout/1,
upper_lower/2,
frontchar/3,
save/2,
errorexit/0,
deletefile/1,
fronttoken/3,
write/1,
write/2,
write/3,
db_close/1,
db_delete/2,
db_create/3,
db_open/3,
chain_insertz/5,
chain_terms/5,
cutbacktrack/1,
getbacktrack/1,
copyfile/2,
exit/0,
%cgi_GetParmList/1,
assert/1,
assert/2,
asserta/1,
eof/1,
envsymbol/2,
char_int/2,
list_to_string/3,
readstring/2,
comline(1),
op(700, xfx, user:(<>)),
op(700, xfx, user:(>)),
op(700, xfx, user:(<))
]).

:- style_check(+string).


:- use_module(database).


:- set_prolog_flag(double_quotes, string).


%:- op(700, xfx, user:(=)).
:- op(700, xfx, user:(<>)).

%:- redefine_system_predicate(user:'='(_,_)).
:- redefine_system_predicate(user:'>'(_,_)).
user:'>'(X,Y) :-
              (number(X), number(Y))
              -> X > Y
              ;  X @> Y.

:- redefine_system_predicate(user:'<'(_,_)).
user:'<'(X,Y) :-
              (number(X), number(Y))
              -> X < Y
              ;  X @< Y.


%user:'='(X,Y) :- X is Y.

user:'<>'(X,Y) :- not( X == Y).

%:- dynamic db_term/2;


:- user:redefine_system_predicate(assert(_)).
user:assert(Term) :-
             functor(Term, Functor, Arity),
             functor(Term0, Functor, Arity),
             predicate_property(Term, imported_from(Module)),
             (db_single(Functor/Arity) -> Module:retractall(Term0); true ),
             %write('assert: '), writeln(Module:Term),
             system:assert(Module:Term).

:- user:redefine_system_predicate(asserta(_)).
user:asserta(Term) :-
             functor(Term, Functor, Arity),
             functor(Term0, Functor, Arity),
             predicate_property(Term, imported_from(Module)),
             (db_single(Functor/Arity) -> Module:retractall(Term0); true ),
             %write('asserta: '), writeln(Module:Term),
             system:asserta(Module:Term).



:- user:redefine_system_predicate(write(_)).
user:write(Term) :-
     system:write_term(Term, [quoted(true)]).
     

:- user:redefine_system_predicate(write(_,_)).
user:write(Arg1, Arg2) :-
            write(Arg1),
            write(Arg2).



:- user:redefine_system_predicate(assert(_,_)).
user:assert(Fact, _FactsSectionName) :- user:assert(Fact).


/*
searchstring (STRING SourceStr, STRING SearchStr, UNSIGNED Position)

Flow pattern    (i, i, o)

The first character in SourceStr has position 1. The search is case sensitive.

*/


searchstring(SourceStr, SearchStr, Position) :-
        var(Position),
        sub_string(SourceStr, Position0, _, _, SearchStr),
        Position is Position0 + 1.

/*
frontstr (UNSIGNED NumberOfChars, STRING SrcString, STRING StartStr, STRING EndString)

Flow pattern     (i, i, o, o) 

Split a string into two strings
*/

% sub_string(+String, ?Start, ?Length, ?After, ?Sub)

frontstr(NumberOfChars, SrcString, StartStr, EndString) :-
        NumberOfCharsN is NumberOfChars,
        sub_string(SrcString, 0, NumberOfCharsN, RestLen, StartStr),
        sub_string(SrcString, NumberOfCharsN, RestLen, _,  EndString).
        



/*
frontchar (STRING String, CHAR FirstChar, STRING RestString)

Flow patterns    (i, o, o), (i, i, o), (i, o, i), (i, i, i), (o, i, i)

Return the first character in a string
*/

frontchar(String, FirstChar, RestString) :-
        var(FirstChar) ->
        frontstr(1, String, StartStr, RestString),
        str_char(StartStr, FirstChar) ;
        str_char(S1, FirstChar),
        string_concat(S1, RestString, String).

        
        
        


/*
concat (STRING String1, STRING String2, STRING LongString)

Flow patterns   (i, i, o), (o, i, i), (i, o, i), (i, i, i) 

Concatenate two strings 
*/

concat(String1, String2, LongString) :-
        string_concat(String1, String2, LongString).


/*
str_int (STRING StringArg, INTEGER IntArg)

Flow patterns    (i, o), (o, i), (i, i)
*/

string_to_number_checked(StringArg, IntArg) :-
       nonvar(StringArg)
       -> catch(atom_number(StringArg, IntArg),_, (fail) )
       ; (atom_number(Atom, IntArg), string_to_atom(StringArg, Atom)).
       


str_int(StringArg, IntArg) :-
                   string_to_number_checked(StringArg, IntArg).
    %atom_number(StringArg, IntArg).


/*
str_real (STRING StringArg, REAL RealArg)

Flow patterns    (i, o), (o, i), (i, i)
*/

str_real(StringArg, RealArg) :-
                    string_to_number_checked(StringArg, RealArg).
    %atom_number(StringArg, RealArg).

/*
str_char(STRING StringArg, CHAR CharArg)

Flow patterns    (i, o), (o, i), (i, i)

Convert between a single character string and a character
*/

str_char(StringArg, CharArg) :-
    nonvar(StringArg)
       -> subchar(StringArg, 0, CharArg)
       ;  string_to_list(StringArg, [CharArg]).


/*
subchar (STRING String, UNSIGNED Position, CHAR RetChar)

Flow patterns    (i, i, o)

Returns the character RetChar at a given Position in the specified String.
*/


subchar(String, Position, RetChar) :-
    PositionN is Position,
    sub_string(String, PositionN, 1, _, RetStr),
    string_to_list(RetStr, [Code]),
    char_code(RetChar, Code).


/*
bound (<VARIABLE> Variable)

Flow pattern    any

Test whether a variable is bound to a value
*/

bound(Var) :-
   nonvar(Var).

free(Var) :-
   var(Var).


swritef(Str, Format, Args) :-
  fix_format_str(Format, Fixed),
  system:swritef(Str, Fixed, Args).


/*
format (STRING OutputString, STRING FormatString, Arg1, Arg2, ..., ArgN)

Flow pattern    (o, i, i, i, i....)

Format several arguments into a string
*/

format(OutputString, FormatString, Arg1, Arg2, Arg3, Arg4, Arg5) :-
    swritef(OutputString, FormatString, [Arg1, Arg2, Arg3, Arg4, Arg5]).


format(OutputString, FormatString, Arg1, Arg2, Arg3, Arg4) :-
    swritef(OutputString, FormatString, [Arg1, Arg2, Arg3, Arg4]).

format(OutputString, FormatString, Arg1, Arg2, Arg3) :-
    swritef(OutputString, FormatString, [Arg1, Arg2, Arg3]).

format(OutputString, FormatString, Arg1, Arg2) :-
    swritef(OutputString, FormatString, [Arg1, Arg2]).

format(OutputString, FormatString, Arg1) :-
    swritef(OutputString, FormatString, [Arg1]).


/*
str_len (STRING String, UNSIGNED Length)

Flow patterns   (i, i), (i, o), (o, i)

Correlate strings and their lengths
*/

str_len(Str, Len) :-
    string_length(Str, Len).


/*
searchchar (STRING String, CHAR SearchingChar, UNSIGNED Pos)

Flow pattern     (i, i, o)

Locate the character SearchingChar in a string

he first character has position 1. The search is case sensitive.

*/

searchchar(SourceString, SearchingChar, Position) :-
    string_to_list(SearchStr, [SearchingChar]),
    searchstring(SourceString, SearchStr, Position).
    

/*
substring (STRING Source, UNSIGNED Pos, UNSIGNED Len, STRING Part)

Flow pattern    (i, i, i, o)

Return part of string
Remarks

The first character in Source has position 1. It is an error to address outside the string, except that it is not an error to ask for 0 characters at the extreme end of it.
*/

substring(String, Offset, Length, Sub) :-
        Offset0 is Offset - 1,
        sub_string(String, Offset0, Length, _After, Sub).




/*
term_str (<domainName>, <TERM> Term, STRING String)

Flow patterns   procedure (i, o, i), procedure (i, i, o), determ (i, i, i)

Conversion between a term and a string
*/


term_str(_, Term, String) :-
    nonvar(Term),!,
    with_output_to(string(String), write_term(Term, [quoted(true)])).


term_str(_, Term, String) :-
    nonvar(String),!,
    atom_to_term(String, Term, _).
    % don't know how...
    %fail.


/*

openwrite/2
writedevice/1
closefile/1

*/


eof(Sym) :-
   atomic(Sym),
   stream_property(Stream, alias(Sym)),
   at_end_of_stream(Stream).


openwrite(Sym, FN) :-
              atomic(Sym),
               open(FN, write, _, [alias(Sym)]).

openread(Sym, FN) :-
              atomic(Sym),
              open(FN, read, _, [alias(Sym)]).

readdevice(keyboard) :- !.
readdevice(Sym) :-
                var(Sym)
                   -> current_input(Sym)
                   ; (is_stream(Sym)
                     -> set_input(Sym)
                     ; true).

writedevice(Sym) :-
                var(Sym) -> current_output(Sym)
                         ; set_output(Sym).


closefile(keyboard) :- !.
closefile(Sym) :-
               close(Sym).

% single % symbol has %s meaning
% sub_string(+String, ?Start, ?Length, ?After, ?Sub)

%fix_format_str("", "").

fix_format_str(S, Fixed) :-
   wildcard_match("*%[\"<> \n]*", S),
   sub_string(S, Start, Length, After, "%"),
   sub_string(S, 0,     Start,  _,   Fixed_Start),
   EndStart is Start + Length,
   sub_string(S, EndStart, After,  _,   Fixed_End),!,
   fix_format_str(Fixed_End, Fixed_End_Fixed),

   system:swritef(Fixed, "%t%t%t", [Fixed_Start, "%t", Fixed_End_Fixed]).


fix_format_str(S, S).


writef(Str, Arg1) :-
    fix_format_str(Str, Fixed),
    system:writef(Fixed, [Arg1]).

writef(Str, Arg1, Arg2) :-
    fix_format_str(Str, Fixed),
    writef(Fixed, [Arg1, Arg2]).
    
writef(Str, Arg1, Arg2, Arg3) :-
    fix_format_str(Str, Fixed),
    writef(Fixed, [Arg1, Arg2, Arg3]).

/*
readterm(<domainName>, <TERM> Term)

Flow pattern    (i, o), (i, i)

Read a term from a given domain
Remarks
*/

readterm(_Domain, Term) :-
    read(Term).


% TBD: use Fact to match predicates to delete
retractall(Fact, DbName) :-
                 %write('retractall: '), write(Fact), writeln(DbName),
                 var(Fact)
                 -> retract_database(DbName)
                 ;  retractall(Fact).
%                 retract_database(DbName).


random(Max, Value) :- Value is random(Max).


/*

filenamepath(STRING QualName, STRING Path, STRING Name)

Flow patterns   (i, o, o), (o, i, i)

Is used to compose and decompose a fully qualified OS filename around its path and filename.

*/

filenamepath(QualName, Path, Name) :-
    nonvar(QualName)
      -> ( absolute_file_name(QualName, Absolute),
           file_base_name(Absolute, Name),
           file_directory_name(Absolute, P),
           atom_concat(P, "/", Path))
      ;   (atom_concat(Path, "/", P1), atom_concat(P1, Name, QualName)).



/*
trap( PredicateCall, INTEGER ExitCode,  ErrorPredicate)

Flow pattern    (i, o,i)

Catch exit, break and run-time errors
*/

trap( PredicateCall, ExitCode,  ErrorPredicate) :-
     catch(PredicateCall, ExitCode,  ErrorPredicate).
     


existfile(File) :-
    exists_file(File).


/*
consult(STRING OSFileName, FactsSectionName)

Flow pattern    (i, i)

Read facts from a text file to a specified internal facts section
*/
consult(OSFileName, FactsSectionName) :-
                    %string_to_atom(DbName, FactsSectionName),
                    load_database(FactsSectionName, OSFileName).




/*
upper_lower (STRING UpperCase, STRING LowerCase)
upper_lower (CHAR UpperCase, CHAR LowerCase)

Flow patterns   (i, i), (i, o), (o, i)

Convert between upper and lower case characters
*/


upper_lower(UpperCase, LowerCase) :-
    nonvar(UpperCase),!, % Upper -> Lower
    downcase_atom(UpperCase, LowerCase).

upper_lower(UpperCase, LowerCase) :-
    nonvar(LowerCase),!, % Lower -> Upper
    upcase_atom(LowerCase, UpperCase).

upper_lower(X, X).




/*
save(STRING OSFileName, <FactsSectionName> FactsSectionName)

Flow pattern     (i, i)
*/

save(OSFileName, FactsSectionName) :-
                 save_database(FactsSectionName, OSFileName).


exit :-
   halt.

errorexit :-  exit.
          %halt.

deletefile(File) :-
                delete_file(File).


/*
fronttoken (STRING String, STRING Token, STRING RestString)

Flow patterns    (i, o, o), (i, i, o), (i, o, i), (i, i, i), (o, i, i)

Separates the first token in a string
Remarks

The fronttoken operates as if it were defined by the equation:

String = (the concatenation of Token and RestString)

A group of one or more characters constitutes a token in the following cases:

·       The text group constitutes a <name> according to Visual Prolog syntax.
·       The group constitutes a valid string representation of a Visual Prolog integer or real (a preceding sign is returned as a separate token).
·       The group is a single character, but not the ASCII space character (ASCII 32), Horizontal Tab (ASCII 9 - '\t' <HT>) or other white-space character (Line Feed (ASCII 10 <LF>), Vertical Tab (ASCII 11 <VT>), Form Feed (ASCII 12 <FF>), Carriage Return (ASCII 13 (CR)), SUBstitute (ASCII 26 <SUB> or <EOF> - Ctrl+Z)).

Notice that in DOS-like strings, new lines ('\n') are represented as the couples of the <CR> and the <LF> characters. Contrary, in UNIX -like strings, new lines are represented as the single <LF> character (see also str_dosstr/2 predicate).

Fronttoken() skips all white space characters (blanks,tabs) and separates from the resulting string the first valid token.
The remainder is matched with RestString. A valid token is either a variable or name 'A'...'Z','a'...'z','?','0'...'9', a number '0'...'9' or a single character. It fails if String was empty or contained only whitespace.

A name starts with a letter, or an underscore character, followed by any combination of zero or more letters, digits, and underscores up to 250 characters long.

*/


/*
русские буквы - char_type() вряд ли поддерживает

*/

char_type_alpha(C) :-
                   char_type(C, alpha) ; C > 127.
                   



find_token([], [], [], _).


find_token([H|T], Token, Rest, token_name) :- !,
    (H == 95 /* '_' */ ; char_type_alpha(H); char_type(H, digit))
       -> (Token = [H|TToken], find_token(T, TToken, Rest, token_name)) ;
    (Token = [], Rest = [H|T]).


find_token([H|T], Token, Rest, token_number) :- !,
    (H == 46 /* '.' */ ; char_type(H, digit))
       -> (Token = [H|TToken], find_token(T, TToken, Rest, token_number)) ;
    (Token = [], Rest = [H|T]).


find_token([H|InpTail], [H|Token], Rest) :- !,
    char_type_alpha(H) -> find_token(InpTail, Token, Rest, token_name) ;
    char_type(H, digit) -> find_token(InpTail, Token, Rest, token_number) ;
%    (char_type(H, space) ; char_type(H, white)) -> find_token(InpTail, [H|Token], Rest) ;
    (Token = [], Rest = InpTail).

/*
find_token_dl([],L-L).
find_token_dl([H|T],L2-L3) :- find_token_dl(H, L1-L2),find_token_dl(T,L2-L3).
ft_test(L, F) :-find_token_dl(L, F - []),!.
app(A-B, B-C, A-C).
*/

skip_white_space([], []) :- !.

skip_white_space([H|ListIn], ListOut) :-
        (char_type(H, white) ; char_type(H, space))
         -> skip_white_space(ListIn, ListOut) ;
         ListOut = [H|ListIn].
         


%skip_white_space(ListIn, ListIn).


fronttoken(String, Token, RestString) :-
    nonvar(String),!,
    string_to_list(String, InpList),
    skip_white_space(InpList, InpList1),
    find_token(InpList1, TokenL, RestStringL),
    string_to_list(Token, TokenL),
    string_to_list(RestString, RestStringL).

fronttoken(InpStr, Token, RestStr) :- !,
                    var(InpStr),
                    concat(Token, RestStr, Str),
                    InpStr = Str.


test_ff :-
%    fronttoken('+test', '+', 'test'),
%    fronttoken('all kids do fine', 'all', ' kids do fine'),
%    fronttoken('all+kids do fine', 'all', '+kids do fine'),
%    fronttoken('22all kids do fine', '22', 'all kids do fine'),
%    fronttoken('22.66all kids do fine', '22.66', 'all kids do fine'),
%    fronttoken('.66all kids do fine', '.', '66all kids do fine'),
%    fronttoken('-22.66all kids do fine', '-', '22.66all kids do fine'),
    fronttoken('   all_kids do fine', 'all_kids', ' do fine').




write(Arg1, Arg2, Arg3) :-
            write(Arg1),
            write(Arg2),
            write(Arg3).



/*
marktime(INTEGER CentiSeconds, REAL Ticket)

Flow pattern    (i, o)

Return time-stamp with expiration
*/

marktime(_CentiSeconds, _Ticket) :- throw(error('marktime','')).


/*
timeout (REAL Ticket)

Flow pattern    (i)

Test time-stamp for expiration
Remarks

The Ticket must be a time-stamp returned by marktime. The timeout predicate succeeds when Ticket has expired.
*/
            
timeout(_Ticket) :- throw(error('timeout','')).


/*
nondeterm retract(<of_corresponding_facts_domain> Fact, <FactsSectionName> FactsSectionName)

Flow pattern    (i, i) (o,i)

Remove a Fact from a named internal facts section FactsSectionName
*/

retract(Fact, FactsSectionName) :-  retract_database(FactsSectionName, Fact).
%              var(Fact)
%              -> (retract_database(FactsSectionName, Fact))
%              ; retract(Fact).



/*
db_create(DB_SELECTOR Dbase, STRING DbaseName, PLACE Place)

Flow pattern     (i, i, i)

Create an external database
*/

db_create(_DbName, _FileName, _Place) :- throw('db_xxxx').



/*
db_open(DB_SELECTOR Dbase, STRING DbaseName, PLACE Place)

Flow pattern     (i, i, i)

Open a database
*/

db_open(_Dbase, _DbaseName, _Place) :- true. %throw('db_xxxx').

db_close(_DbName) :- true. %throw('db_xxxx').

db_delete(_Dbase, _Place) :- true. %throw('db_xxxx').


/*
chain_insertz(DB_SELECTOR Dbase, STRING ChainName, <domainName>, <TERM> Term, REF RefNumber)

Flow pattern    (i, i, i, i, o)

Insert a term into a database at end of a chain
*/

chain_insertz(_Dbase, _ChainName, _DomainName, _Term, _RefNumber) :- throw('chain_insertz').
               %      assert(db_term(Dbase, Term)).

/*
nondeterm chain_terms(DB_SELECTOR Dbase, STRING ChainName, <domainName>, <TERM> Term, REF RefNumber)

Flow pattern    (i, i, i, i, o) (i, i, i, o, o)

Non-deterministically returns terms in a chain
*/

chain_terms(_Dbase, _ChainName, _DomainName, _Term, _RefNumber) :- throw('chain_terms').
             %      db_term(Dbase).


/*
cutbacktrack (UNSIGNED Btop)

Flow pattern     (i)

Implement a dynamic cut, together with getbacktrack

*/
cutbacktrack(_Btop)  :- true.
               %throw('cutbacktrack').


/*
getbacktrack(UNSIGNED Btop)

Flow pattern    (o)

Implement a dynamic cut, together with cutbacktrack

*/

getbacktrack(_Btop) :- true.
               %throw('getbacktrack').


/*
copyfile (STRING SourceFileName, STRING DestFileName)

Flow pattern    (i, i)

Copy the specified source file SourceFileName to another file DestFileName.
*/

copyfile(SourceFileName, DestFileName) :-
   format(string(Cmd), 'copy ~s ~s', [SourceFileName, DestFileName]),
   shell(Cmd).







save_term_file(T,File) :-
    open(File,write, Stream, [close_on_abort(true)]),
    write_term(Stream, T, [quoted(true)]),
    write(Stream, '.'),
    close(Stream).

load_term_file(T,File) :-
    open(File,read, Stream, [close_on_abort(true)]),
    read_term(Stream, T, [double_quotes(string)]),
    close(Stream).



%:- use_module(library(cgi)).

% menu_item=cnpt_intocur&curcnpt_id=68983474&old_id=

% dlg_ask=&
%msg_to_display=&
%menu_item=DIC_all_ext_templ&
%curcnpt_id=45037582&
%old_id=&
%curcnpt_name=sksksksks%28v.1%29&
%env_id=28000960&
%curcnpt_text=%2F*+%CE%EF%F0%E5%E4%E5%EB%E5%ED%E8%FF+%E2%F1%F2%F0%EE%E5%ED%ED%EE%E3%EE+%F2%E8%EF%E0+%E4%E0%ED%FB%F5+boolean+*%2F%0D%0A%0D%0AFalse%2CTrue+-%FD%EB%E5%EC%E5%ED%F2%FB+%EE%E1%EB%E0%F1%F2%E8+boolean.%0D%0AAND%3A+boolean+x+boolean+-%3Eboolean.%0D%0AOR+%3A+boolean+x+boolean+-%3E+boolean.%0D%0ANOT+%3A+boolean+-%3Eboolean.%0D%0A%2F*+%D2%E0%E1%EB%E8%F6%E0+%E4%E5%E9%F1%F2%E2%E8%FF+%EB%EE%E3%E8%F7%E5%F1%EA%E8%F5+%EE%EF%E5%F0%E0%F6%E8%E9+*%2F%0D%0A%0D%0ANOT%28False%29%3DTrue.+NOT%28True%29%3DFalse.%0D%0AAND%28False%3B+False%29%3DFalse.+AND%28False%3B+True%29%3DFalse.%0D%0AAND%28True%3B+False%29%3DFalse.+AND%28True%3B+True%29%3DTrue.%0D%0A%0D%0AOR%28False%3B+False%29%3DFalse.+OR%28False%3B+True%29%3DTrue.%0D%0AOR%28True%3B+False%29%3DTrue.+OR%28True%3B+True%29%3DTrue.&
%cmd=&
%rest=&
%usedIds=%5B%5D&
%mode=&
%inset=CC&
%toolbar_button=&
%explorer_selected_left=&
%explorer_selected_right=&
%new_folder_name=&new_cnpt_name=

unique_file_name(File, N, FileOut) :-
       atom_number(Str, N),
       concat(File, Str, File1),
       not(exists_file(File1)),
       FileOut = File1.


unique_file_name(File, N, FileOut) :-
       Q is N + 1,
       unique_file_name(File, Q, FileOut).


/*
cgi_GetParmList(List) :-
      exists_file('param_cgi.txt')
         -> load_term_file(List,'param_cgi.txt')
         ;
       (
       cgi_get_form(Arguments),
       conv_args(Arguments, List),
       unique_file_name('param_cgi_out.', 0, File),
       save_term_file(List, File)).
*/

conv_args([], []).
conv_args([A0|T], [ParmListH | ParmListT] ) :-
        A0 =.. [Name, Value],
        string_to_atom(S_Name, Name),
        string_to_atom(S_Value, Value),
        ParmListH = parm(S_Name, S_Value),
        conv_args(T, ParmListT).


             
%test(X,Y):- X is Y.


remove_par(S, SN) :-
             string_length(S, L0),
             L is L0 - 1,
             sub_string(S, 0, 1, _, '"'),
             sub_string(S, L, 1, _, '"'),!,
             L2 is L0 - 2,
             sub_string(S,1,L2,_,SN).
             
remove_par(S, S).


/*
envsymbol(STRING EnvId, STRING SymbolStr)

Flow pattern    (i, o)

Read the value of an environment symbol
*/

envsymbol(EnvId, S1) :-
                 getenv(EnvId, A),
                 string_to_atom(S, A),
                 remove_par(S, S1).



/*
char_int (CHAR CharArg, INTEGER IntArg)

Flow patterns    (i, o), (o, i), (i, i)

Convert between characters and their ASCII values
*/

char_int(CharArg, IntArg) :-
                  char_code(CharArg, IntArg).
                  
                  
/*
PROCEDURE list_to_string( SLIST  StrList,
                          STRING Separator,
                          STRING OutPutString )
                                                - (i,i,o) language c as "_STR_List_To_String"
*/

list_to_string(StrList, Separator, OutPutString) :-
    atomic_list_concat(StrList, Separator, A),
    string_to_atom(OutPutString, A).

/*
readblock (UNSIGNED Length, BINARY BinBlock)

Flow pattern    (i, o)
*/

read_chars(0, []) :- !.


read_chars(Length, [H|T]) :-
                   get_code(H),
                   %H  = -1 -> (throw(error('Wrong number of bytes read from file','')))
                   L is Length  - 1, read_chars(L, T).

read_chars_all(-1, []) :- !.
read_chars_all(C, [C|T]) :-
                   get_code(H),
                   read_chars_all(H, T).

no_tty :-
    current_stream(0, read, ConIn), set_stream(ConIn, tty(false)),
    current_stream(1, write, ConOut), set_stream(ConOut, tty(false)),
    current_stream(2, write, ConErr), set_stream(ConErr, tty(false)).



readstring(0, String) :- !,
                   get_code(C),
                   read_chars_all(C, Chars),
                   atom_codes(Atom, Chars),
                   string_to_atom(String, Atom).




readstring(Length, String) :- !,
                   read_chars(Length, Chars),
                   atom_codes(Atom, Chars),
                   string_to_atom(String, Atom).


comline(Cmd) :-
             current_prolog_flag(argv, List),
             [_|[Cmd|_]] = List,!.


comline(Cmd) :-
             Cmd = "".


ctest :-
     get_code(C),
     writeln(C),
     char_code(A, C),
     char_code(A, C1),
     writeln(C1),
     put_code(C1).


% movemem, readblock
