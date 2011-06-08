/*****************************************************************************

		Copyright (c) 1984 - 2000 Prolog Development Center A/S

 FileName: CGITOOLS.PRO
 Purpose: No description
 Written by: Visual Prolog
 Comments:
******************************************************************************/



:- module(cgitools,[
  cgi_GetParmList/1
]).

:- style_check(+string).
:- set_prolog_flag(unknown, fail).

:- use_module(vp52_compat).
:- use_module(russian).




  string_to_ilist("",[]) :- !.
  string_to_ilist(Str,[First|RestList]) :- !,
	frontchar(Str,Char,RestStr),
	char_int(Char, First),
	string_to_ilist(RestStr,RestList).

ilist_to_slist([],[]) :- !.
ilist_to_slist([Code|T], [Str|T1]) :- !,
        char_int(Char, Code),
        str_char(Str, Char),
	ilist_to_slist(T,T1).
	
   	
 
  ilist_to_string([], "") :- !.
  ilist_to_string(List, Str) :- !,
        ilist_to_slist(List, SList),
	list_to_string(SList,"",Str).        
	
%  
%  Str contains Separator
%  create list of string chunks between separators
%
%  first chunk - not has a separator before
%  second and other chunk - have a separator before
%
  str2list(Str,Seperator,[First|RestList]):-
	searchchar(Str,Seperator,Pos),

	Len is Pos - 1,
        
	frontstr(Len,Str,First,RestStr0),
	frontchar(RestStr0,_,RestStr),!,
	str2list(RestStr,Seperator,RestList).

  str2list(Str,_,[Str]).


%%%%%%%%%%%%%%%%%
%  decode string passed to cgi form server
%
% 
%
%%%%%%%%%%%%%%%%%
  cgi_DecodeString(EncodedString,DecodedString):-
	str2list(EncodedString,'+',SLIST),
	list_to_string(SLIST," ",StringWithPlusConverted),
	cgi_DecodeString1(StringWithPlusConverted,DecodedString1),
	cgi_DecodeString2(DecodedString1, DecodedString).


  cgi_DecodeString1("",""):-!.
  cgi_DecodeString1(EncodedString,DecodedString):-
	str2list(EncodedString,'%',SLIST),
	SLIST = [H|T],!,
	decodelist(T,DecodedList),
	list_to_string([H|DecodedList],"",DecodedString).
  cgi_DecodeString1(_,_):-errorexit.



  proc_utf8([], []) :- !.
  proc_utf8([C], [C]) :- !.


  utf_8_utf_16(U8_1, U8_2, U16) :-
     U16 is ((U8_1 /\ 0x1F) << 6) \/ (U8_2 /\ 0x3F).

 proc_utf8([208|[C2|Rest]], [Code|T]) :- !,
      utf_8_utf_16(208, C2, Code),
      proc_utf8(Rest, T).

 proc_utf8([209|[C2|Rest]], [Code|T]) :- !,
      utf_8_utf_16(209, C2, Code),
      proc_utf8(Rest, T).


  proc_utf8([C|Rest], [C|Rest1]) :- !,
      proc_utf8(Rest,Rest1).
      
   
  cgi_DecodeString2("",""):-!.
  cgi_DecodeString2(EncodedString,DecodedString):-
	string_to_ilist(EncodedString,ILIST),
	proc_utf8(ILIST, ILIST2),
	ilist_to_string(ILIST2, DecodedString).

%
%  take 2 sybmols for each element of the list
%  convert it into one symbol with code HighQu * 16 + LowQu
%
%
  decodelist([],[]):-!.
  decodelist([H|T],[DecodedH|DecodedT]):-
        frontchar(H,C0,Rest1),
        frontchar(Rest1,C1,Rest),!,
        char_hex(C0,HighQu),
        char_hex(C1,LowQu),

        AsciiCode is HighQu * 16 + LowQu,

        char_int(Symb,AsciiCode),
        frontchar(DecodedH,Symb,Rest),
	decodelist(T,DecodedT).
	
  decodelist(_,_):-errorexit.

  % Primitive char_to_hex convertion

  char_hex(C,HV) :-
	char_type(C,  xdigit(HV)).


/*=============================================================================
	str_namelist
=============================================================================*/

  str_namelist("",[]):-!.
  str_namelist(STR,[PARM|RestList]):-
	getparm(STR,RestStr, PARM),
	str_namelist(RestStr,RestList).

  getparm(Str,RestStr,Parm):-
	searchchar(Str,'&',Pos),

	Len is Pos - 1,

	frontstr(Len,Str,ParmStr,RestStr0),
	frontchar(RestStr0,_,RestStr),!, % Ignore the '&'
	str_parm(ParmStr,Parm).
	
  getparm(Str,"",Parm):-!,
	str_parm(Str,Parm).
	
  getparm(_,_,_):-errorexit.

  str_parm(Str,cgiparm(Name,Val)):-
	searchchar(Str,'=',Pos),

	Len is Pos - 1,

	frontstr(Len,Str,EncodedName,RestStr0),
	frontchar(RestStr0,_,EncodedVal),!, % Ignore the '='
	cgi_DecodeString(EncodedName,Name),
	cgi_DecodeString(EncodedVal,Val).
	
  str_parm(_,_):-errorexit.
	
/*=============================================================================
 Retrieves the query string sent by the server with "POST" request method .
    If request is made with "POST" method we are to read the data from standard input.
    The data size we need to read from standard input
    is located by WEB-server in ENV variable "CONTENT_LENGTH"
=============================================================================*/
  retrieve_POST_string(CGI_String) :-
        envsymbol("CONTENT_LENGTH",LenStr),
        str_int(LenStr,Len),!,

	readstring(Len, CGI_String).
	
	
	
  retrieve_POST_string(_) :-
	errorexit.

        


/*=============================================================================
      Retrieves the query string sent by the server with "GET" request method .
 If request is made with "GET" method the data is placed
 in the "QUERY_STRING" environment variable
=============================================================================*/
  retrieve_GET_string( CGI_String ) :-
        envsymbol("QUERY_STRING",CGI_String),!.
  retrieve_GET_string("").


/*=============================================================================
                        get_CGI_string(CGI_String)
      Retrieves the query string sent by the server .
=============================================================================*/
  get_CGI_string_by_method("POST",CGI_String) :-
        retrieve_POST_string(CGI_String),!.
  get_CGI_string_by_method("GET",CGI_String) :-
        retrieve_GET_string(CGI_String),!.
  get_CGI_string_by_method(_,_) :- 
	errorexit.

  cgi_GetString( CGI_String ) :-
        envsymbol("REQUEST_METHOD",Method),!,
        get_CGI_string_by_method(Method,CGI_String).
  cgi_GetString("").


         



save_term_file(T,File) :-
    open(File,write, Stream, [close_on_abort(true)]),
    write_term(Stream, T, [quoted(true)]),
    write(Stream, '.'),
    close(Stream).

load_term_file(T,File) :-
    open(File,read, Stream, [close_on_abort(true)]),
    read_term(Stream, T, [double_quotes(string)]),
    close(Stream).



  cgi_GetParmList( ParmList ) :- % для отладки загрузка из файла
         existfile("param_cgi.txt"), 
         openread(input, "param_cgi.txt"),
         readdevice(input),
         readterm(parmList, ParmList),!,
         readdevice(keyboard),
         closefile(input).
  cgi_GetParmList([]) :- 
         existfile("param_cgi.txt"),
         readdevice(keyboard),
         closefile(input),
         fatalErrorMsg("Параметры в файле param_cgi.txt написаны неверно."),
         !. 


  cgi_GetParmList( ParmList ) :- % реальная загрузка по CGI
	cgi_GetString(CGI_String),
	str_namelist(CGI_String,ParmList),

	save_term_file(ParmList, "param_cgi_out.txt")

	.

/*=============================================================================
	Return HTML from the <BODY> Section in a file
=============================================================================*/
/*
  gethtmlfromfile(FileName,BODY):-
	syspath(ExeStartupPath,_ProgName),
	filenamepath(FullName,ExeStartupPath,FileName),
	file_str(FullName,TEXT),
  	upper_lower(TEXT,LOWERTEXT),
	searchstring(LOWERTEXT,"<body",LEN1),
	frontstr(LEN1,LOWERTEXT,_,TXT1),
	searchchar(TXT1,'>',BODY_TAG_LEN),

ifndef iso_prolog
	STARTPOS = LEN1 + BODY_TAG_LEN + 1,
elsedef
	STARTPOS is LEN1 + BODY_TAG_LEN + 1,
enddef

	searchstring(LOWERTEXT,"</body>",ENDPOS),!,

ifndef iso_prolog
	LEN = ENDPOS - STARTPOS,
elsedef
	LEN is ENDPOS - STARTPOS,
enddef

	substring(TEXT,STARTPOS,LEN,BODY).
  gethtmlfromfile(_,_):-
	errorexit.
*/

/*=============================================================================
	Lookup parameter in a parameterlist; if not found return default value
=============================================================================*/

  lookupParm(_,[],Default,Default).
  lookupParm(ParmName,[cgiparm(ParmName,Value)|_],_,Value):-!.
  lookupParm(ParmName,[_|List],Default,Value):-
	lookupParm(ParmName,List,Default,Value).


/*=============================================================================
	Remove empty parameter values
=============================================================================*/

  remove_empty_params([],[]).
  remove_empty_params([cgiparm(_,"")|Rest],Filtered):-!,
	remove_empty_params(Rest,Filtered).
  remove_empty_params([H|Rest],[H|Filtered]):-
	remove_empty_params(Rest,Filtered).


