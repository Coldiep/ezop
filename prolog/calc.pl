



:- module(calc, [
calc_on_approx/2, %(expr,expr) - determ (i,o)
calc/2, %(expr,expr) -determ (i,o)  %(i, r(o))
expr_to_str/2, %(expr,string) - (i,o)
replaceStr_all/4, %(string Old, string New,string Str,string NewStr) - determ (i,i,i,o)
error_msg/2, %(STRING,INTEGER) - determ (i,i)
calc_type/2, %(expr,expr) - (i,o)
 make_morphism/3, %(expr,expr,expr) - (i,i,i)
list_not_repeat/2, %(slist,slist) - (i,o)
list_not_repeat/2, %(expr_list,expr_list) - (i,o)
isa_calc_type/2, %(expr Term,expr Type) - (i,i)
replace_var_on_ex/4, %(expr_list Arglist,expr_list ListVar,expr Term,expr TermRes) - determ (i,i,i,o) % заменяет в Term переменные из ListVar на соотв. выражения из ArgList
subobjects_of/2, %(expr,expr_list) - (i,o)
scan/2, %(string,slist) -(i,o)
listStr_listWord/2, %(slist,list_w_v_t) - (i,o)
gen_term_num/1, %(integer)-(o)
weigh/2  %(expr,integer)-(i,o)
]).
:- style_check(+string).

:- use_module(vp52_compat).
:- use_module(editor).
:- use_module(impose).
:- use_module(dbctrl).
:- use_module(basic_eq).
:- use_module(make_eq).
:- use_module(db).


%include "editor.pre"




append([],L,L):-!.
append([H|T1],L,[H|T2]):-append(T1,L,T2).



exprlist_to_slist([],[]).
exprlist_to_slist([H|T],[Hstr|Tstr]):-
	expr_to_str(H,Hstr),
	exprlist_to_slist(T,Tstr).



exprlist_to_str([],"").
exprlist_to_str([H],Str):-!,expr_to_str(H,Str).
exprlist_to_str([H|T],Str):-expr_to_str(H,StrH),
   exprlist_to_str(T,StrT),
   concat(StrH,", ",Str1),
   concat(Str1,StrT,Str).

exprlist_to_order_str_nl(ExprList,  Str):-
	exprlist_to_slist(ExprList,StrList),
	order_string(StrList,OrderStrList),
	slist_to_str_nl(OrderStrList,Str).

slist_to_str_nl([],""):-!.
slist_to_str_nl([H],Str):-!,
	concat("\n	",H,Str1),
	concat(Str1,".",Str).
slist_to_str_nl([H|T],Str):-
	concat("\n	",H,Str1),
	concat(Str1,",",Str2),
	slist_to_str_nl(T,StrT),
	concat(Str2,StrT,Str).

  exprlist_to_str_nl([],"").
exprlist_to_str_nl([H],Str):-!,
   expr_to_str(H,Str1),
   concat("\n	",Str1,Str2),
   concat(Str2,".",Str).
exprlist_to_str_nl([H|T],Str):-expr_to_str(H,StrH1),
   concat("\n	",StrH1,StrH),
   exprlist_to_str_nl(T,StrT),
   concat(StrH,",",Str1),
   concat(Str1,StrT,Str).

expr_to_str(Ex,""):-retractall(glob_string(_)),translate(Ex,Text),
   assert(glob_string(Text)),fail.

expr_to_str(_,Text):-retract(glob_string(Text)),!.

expr_to_str(_,"** ? **").

finish_translation([],[],Curr,Curr).

finish_translation([var(_)|T1],[H2|T2],Curr,Res):-!,
   append(Curr,[H2],New),
   finish_translation(T1,T2,New,Res).
   
finish_translation([w(H1)|T1],Textlist,Curr,Res):-
   append(Curr,[H1],New),
   finish_translation(T1,Textlist,New,Res).

listW_to_string([],"").
listW_to_string([w(H)|T],S):-!,listW_to_string(T,Tmp),
   	concat(H," ",Tmp1),concat(Tmp1,Tmp,S).
listW_to_string([var(H)|T],S):-!,listW_to_string(T,Tmp),
	expr_to_str(H,StrH),
	concat("@",StrH,Str),
   	concat(Str," ",Tmp1),concat(Tmp1,Tmp,S).

list_to_string_s([],"").
list_to_string_s([H|T],S):-!,list_to_string_s(T,Tmp),
   	concat(H," ",Tmp1),concat(Tmp1,Tmp,S).


list_var([_],[var(ex("@",[])),w(")")]).
list_var([_|T],[var(ex("@",[])),w(",")|T1]):-list_var(T,T1).


translate(nc(Word),Res):-!,concat("\"",Word,Tmp1),concat(Tmp1,"\"",Res).

translate(v(Name,_),Name):-!.

translate(r(R),Str):-str_real(Str,R),!.

translate(undef,"Выражение не определено"):-!.

translate(nil,""):-!.

translate(ex(Opname,[]),Text):-
   get_in_template(_,_,_,_,Opname,_,List,_),!,
   listW_to_string(List,Text).
   
translate(ex(Opname,Arglist),Text):-
   get_in_template(_,_,_,_,Opname,_,List,_),!,
   translate_list(Arglist,Textlist),
     finish_translation(List,Textlist,[],Reslist),
   list_to_string_s(Reslist,Text).

/* Удалено 23.12.02
translate(ex(Opname,[]),Text):-
   template(_,_,Opname,_,List,_),!,
   listW_to_string(List,Text).
 */ 
   
translate(ex(Opname,Arglist),Text):-
   get_template(_,_,_,Opname,_,List,_),
   translate_list(Arglist,Textlist),
     finish_translation(List,Textlist,[],Reslist),
   list_to_string_s(Reslist,Text),!.

translate(ex(Opname,[]),Opname):-!.   

translate(ex(Opname,Arglist),Text):-!,
   list_var(Arglist,Listvar1),
   Listvar=[w("(")|Listvar1],   
   translate_list(Arglist,Textlist),
   finish_translation([w(Opname)|Listvar],Textlist,[],Reslist),
   list_to_string_s(Reslist,Text).
   
translate_list([],[]).
   
translate_list([H1|T1],[H2|T2]):-translate(H1,H2),translate_list(T1,T2).


order_string(List,OrderList):-order_string_aux(List,[],OrderList).

order_string_aux([],L,L):-!.
order_string_aux([H|T],Order,Res):-
		insert(H, Order,Order1),
		order_string_aux(T,Order1,Res).

insert(H, [],[H]):-!.
insert(H, [H1|T],[H,H1|T]) :- 
	
	H @< H1,
	
	!.
insert(H, [H1|T],[H1|T1]):-
	insert(H, T,T1).
	

%end form\\trans.pro
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

all_inconsists(X):-
	not(inconsist(_,_,_)),!,
	X="\n\n     противоречий нет.".
all_inconsists(_):-
	retractall(glob_string(_)),
	assert(glob_string("\n         Равны и неравны:")),
	fail.
all_inconsists(_):-
	inconsist(_,A,B),
	retract(glob_string(Str)),
	expr_to_str(A,StrA),
	expr_to_str(B,StrB),
	concat("\n",StrA,StrA1),
	concat(StrA1," и ",StrA2),
	concat(StrA2,StrB,StrB1),
	concat(StrB1,";",StrAB),
	concat(Str,StrAB,Res),
	assert(glob_string(Res)),
	fail.
all_inconsists(X):-
	retract(glob_string(X)).

%append([],L,L):-!.
%append([H|T1],L,[H|T2]):-append(T1,L,T2).


attribute_list(_,[]).
attribute_list(Ob,[H|T]):-
	calc(ex(attribute,[Ob,H]),_),
	attribute_list(Ob,T).
/*
  d_template(Text):-
 get_in_template(_,_,_Descr,Text,_Opname,Comment,List,Type).
 */

get_in_template_or_template(Dostup, ID_Concept,Id_template,New_Synonym,Opname,Comment,List,TypeRes):-
	get_in_template(Dostup, ID_Concept,Id_template,New_Synonym,Opname,Comment,List,TypeRes).
	
get_in_template_or_template(2, ID_Concept,Id_template,New_Synonym,Opname,Comment,List,TypeRes):-
	get_template( ID_Concept,Id_template,New_Synonym,Opname,Comment,List,TypeRes).
            

 d_template(Text,Vars,Type):-
 	chek_text_nil(Text),
	 impose_all(Vars,TermVars,_TypeV),!,
      lst_var(TermVars,ListVar),
      upper_lower(Text, LowerText),
      form_listArg(LowerText,ListVar,List, ListVTmpl), 
       get_template_Op(Text,_IdDostup,IdC_t,_Id,_Text,Op,_Comment,List,Type),
      chek_ListVar(ListVar,ListVTmpl),
      del_templ(IdC_t,Op,List,Type).
 
del_templ(IdC_t,Op,List,Type):-
	idconcept(IdC),
	IdC=IdC_t,!,
	retractall(in_template(_,IdC,_,_,Op,_,List,Type)).
del_templ(_,Op,List,Type):-
	idconcept(IdC),
      assert(del_template(IdC,Op,List,Type)).

create_new_synonym_template(Text,Vars,Type,NewText,NewComment):-
      chek_text_nil(Text),
      impose_all(Vars,TermVars,_TypeV),!,
      lst_var(TermVars,ListVar),
      upper_lower(Text, LowerText),
      form_listArg(LowerText,ListVar,List, ListVTmpl), 
       get_template_Op(Text,IdDostup,_IdC_t,_Id,_,Op,_Comment,List,Type),!, 
      chek_ListVar(ListVar,ListVTmpl),
       chek_text_nil(NewText),
      upper_lower(NewText, LowerNewText),
      form_listArg(LowerNewText,ListVar,NewList, NewListVTmpl), 
      chek_ListVar(ListVar, NewListVTmpl),
      chk_template(NewList,Type),
     gen_term_num(IdN),
      str_int(NewId,IdN),
      idconcept(IdC),
      str_int(StrIdC,IdC),
      concat(StrIdC,".",S1),
      concat(S1,NewId,NewOp),
      retractall(del_template(_,NewOp,NewList,Type)),
     asserta(in_template(IdDostup,IdC,NewId,NewText,NewOp,NewComment,NewList,Type)),
     VarRes=ex("var",[nc("result"),Type]),
     assert(exec_templ(IdC,NewOp,[VarRes|NewListVTmpl],nc(""),ex("make_equal",[ex(Op,ListVTmpl),VarRes]))).
	


make_synonym(nc(NewSyn),Ex2):-!,
	create_new_synonym(NewSyn,Ex2,"").
make_synonym(Ex1,nc(NewSyn)):-!,
	create_new_synonym(NewSyn,Ex1,"").
make_synonym(Ex1,Ex2):-
	calc(Ex1,D1),
	calc(Ex2,D2),
	retractall(synonym_desc(_,_)),
	assert(synonym_desc(D2,D1)),
	assert(synonym_desc(D1,D2)),
	make_equal(D1,D2),
	retractall(synonym_desc(_,_)).

	

replace_template(Text,Vars,Type,NewText,NewComment):-
      chek_text_nil(Text),
      impose_all(Vars,TermVars,_TypeV),!,
      lst_var(TermVars,ListVar),
      upper_lower(Text, LowerText),
      form_listArg(LowerText,ListVar,List, _ListVTmpl), 
      /*
       get_template_Op(Text,IdDostup,IdC_t,_Id,_,Op,_Comment,List,Type), 
      chek_ListVar(ListVar,ListVTmpl),
       chek_text_nil(NewText),
      upper_lower(NewText, LowerNewText),
      form_listArg(LowerNewText,ListVar,NewList, NewListVTmpl), 
      chek_ListVar(ListVar, NewListVTmpl),
     gen_term_num(IdN),
      str_int(NewId,IdN),
      idconcept(IdC),
      str_int(StrIdC,IdC),
      concat(StrIdC,".",S1),
      concat(S1,NewId,NewOp),
     assert(in_template(IdDostup,IdC,NewId,NewText,NewOp,NewComment,NewList,Type)),
     VarRes=ex("var",[nc("result"),Type]),
	assert(exec_templ(IdC,NewOp,[VarRes|NewListVTmpl],nc(""),ex("make_equal",[ex(Op,ListVTmpl),VarRes]))),
	*/
	get_template_Op(Text,_IdDostup,IdC_t,_Id,_,Op,_Comment,List,Type),!,
	create_new_synonym_template(Text,Vars,Type,NewText,NewComment),
	del_templ(IdC_t,Op,List,Type).
	
get_template_Op(_,IdDostup,IdC,Id,Text,Op,Comment,List,Type):-
	get_in_template(IdDostup,IdC,Id,Text,Op,Comment,List,OpType),
	is_equal(OpType,Type),!.
	
get_template_Op(_,2,IdC,Id,Text,Op,Comment,List,Type):- 
	get_template(IdC,Id,Text,Op,Comment,List,OpType),
	is_equal(OpType,Type),!.
	
get_template_Op(Text,_IdDostup,_IdC,_Id,_Text,_Op,_Comment,_List,Type):-
	expr_to_str(Type,StrType),
	%format(Str,"Шаблон \"%\" с типом результата % не найден.",Text,StrType),
	%dlg_error(Str),
	msg_n(err,1,[Text,StrType],b_true),
	fail.

create_rwt_rule(StrVars,StrPredCond, StrL,StrR,StrPostCond,Msg):-
      impose_all(StrVars,TermVars,_TypeV),!,
      	lst_var(TermVars,ListVar),
      	calc_list(ListVar,ListCalcVar), % исправить!!! имена этих  переменных уже могут
      impose_all_not_message(StrPredCond,PredCond1,TypePredCond),
      	replace_new_var(ListCalcVar,PredCond1,PredCond),
      	chek_termchek(PredCond,TypePredCond,PredCondB),!,
      impose_all(StrL,Lterm1,TypeL),!, 
      	expr_without_bracket(Lterm1,Lterm2),
     	 replace_new_var(ListCalcVar,Lterm2,Lterm),%если есть переменные типа new
      impose_all(StrR, Rterm1,TypeR),!, 
      	expr_without_bracket(Rterm1,Rterm2),
      	replace_new_var(ListCalcVar,Rterm2,Rterm),%если есть переменные типа new
      	chek_is_subtype(TypeR,TypeL),
     impose_all_not_message(StrPostCond,PostCond1,TypePostCond),
    	replace_new_var(ListCalcVar,PostCond1,PostCond),
      	chek_termchek(PostCond,TypePostCond,PostCondB),!,
     listVar_to_ListVarNameType(ListCalcVar,ListVarNameType),
     	replace_var_on_ex(ListVarNameType,ListCalcVar,Lterm,Lterm_withVar),
     	replace_var_on_ex(ListVarNameType,ListCalcVar,Rterm,Rterm_withVar),
     	replace_var_on_ex(ListVarNameType,ListCalcVar,PredCondB,PredCond_withVar),
     	replace_var_on_ex(ListVarNameType,ListCalcVar,PostCondB,PostCond_withVar),
     vars_of( Lterm_withVar,[], ListVarL),
     vars_of( Rterm_withVar,[], ListVarR),
     vars_of( PredCond_withVar,[], ListVarPredCond),
     vars_of( PostCond_withVar,[], ListVarPostCond),
        chek_ListVar(ListVarNameType,ListVarL),
    	chek_ListVar(ListVarR,ListVarL),
    	chek_ListVar(ListVarPredCond,ListVarL), 
    	chek_ListVar(ListVarPostCond,ListVarL),
     gen_term_num(IdN),
     str_int(Id,IdN),
     idconcept(IdC),
     assert(rwt_rule(IdC,Id,Msg,ListVarNameType,Lterm_withVar,Rterm_withVar,PredCond_withVar,PostCond_withVar)),
     delete_listvar(ListCalcVar).

chek_is_subtype(TypeR,TypeL):-
	is_subobject(TypeR,TypeL),!.
chek_is_subtype(TypeR,TypeL):-
	expr_to_str(TypeR,StrTypeR),
	expr_to_str(TypeL,StrTypeL),
	%format(Str,"Тип правой части  \"%\" не является  подтипом \"%\" - левой части правила.",StrTypeR,StrTypeL),
	msg_n(err,2,[StrTypeR,StrTypeL],b_false),
	fail.	

expr_without_bracket(ex("()",[Term]),Term):-!.
expr_without_bracket(ex(Op,ArgList),ex(Op,ArgListRes)):-!,
	exprlist_without_bracket(ArgList,ArgListRes).
expr_without_bracket(Term,Term).

exprlist_without_bracket([],[]):-!.
exprlist_without_bracket([H|T],[HRes|TRes]):-
	expr_without_bracket(H,HRes),
	exprlist_without_bracket(T,TRes).

listVar_to_ListVarNameType([],[]):-!.
listVar_to_ListVarNameType([H|T],[v(HName,HType)|TVarNameType]):-
	expr_to_str(H,HName),
	type(H,HType),
	listVar_to_ListVarNameType(T,TVarNameType).
	
vars_of(nc(_), ListVar,ListVar):-!.
vars_of(r(_), ListVar,ListVar):-!.
vars_of(r(_), ListVar,ListVar):-!.
vars_of(v(N,T), ListVar,ListVar):-
	member(v(N,T),ListVar),!.
vars_of(v(N,T), ListVar,[v(N,T)|ListVar]):-!.
vars_of(ex(_,ArgList), ListVarAux,ListVar):-
	vars_of_list(ArgList,ListVarAux,ListVar).
	
vars_of_list([],ListVar,ListVar):-!.
vars_of_list([H|T],ListVarAux,ListVar):-
	vars_of(H, ListVarAux,ListVarAux1),
	vars_of_list(T,ListVarAux1,ListVar).

create_new_synonym(New_Synonym,_Old_Termin,_):-
      searchstring(New_Synonym,"@",_Position),!,
      % format(Str,"Строка \"%\" содержит переменную и не является термином.",New_Synonym),
	msg_n(err,3,[New_Synonym],b_false),
	fail.	
create_new_synonym(New_Synonym,Old_Termin,_):-
	type(Old_Termin,T1),
      upper_lower(New_Synonym, LowerNew_Synonym),
      str_to_wlist(LowerNew_Synonym,List),
      get_in_template_or_template( _,ID_Concept,_Id_template,_New_Synonym,_Opname,_Comment,List,TypeRes),
      idconcept(CurrId_Concept),
      ID_Concept<>CurrId_Concept,
      compatible(T1,TypeRes),!,
      expr_to_str(TypeRes,StrTypeRes),
        % format(Str,"Термин \"%\" уже используется в типе %.",New_Synonym,StrTypeRes),
	msg_n(err,4,[New_Synonym,StrTypeRes],b_false),
	fail.	
create_new_synonym(New_Synonym,nc(OldTermin),Text):-
	upper_lower(OldTermin, LowerOldTermin),
      str_to_wlist(LowerOldTermin,List),
      get_in_template_or_template(IdDostup, _ID_Concept,_Id_template,TextTmpl,Opname,OldComment,List,Type),
      !,
      upper_lower(New_Synonym, LowerNewText),
      str_to_wlist(LowerNewText,NewList),
      chk_template(NewList,Type),
      gen_term_num(IdN),
      str_int(NewId,IdN),
      idconcept(IdC),
      create_new_comment(OldComment,Text,TextTmpl,NewComment),
      retractall(del_template(_,Opname,NewList,Type)),
     asserta(in_template(IdDostup,IdC,NewId,New_Synonym,Opname,NewComment,NewList,Type)).
create_new_synonym(_New_Synonym,nc(OldTermin),_Text):-
	%format(Str,"Термина \"%\" нет в системе.",OldTermin),
	msg_n(err,5,[OldTermin],b_false),
	fail. 
create_new_synonym(New_Synonym,ex(Opname,[]),Text):-
      get_in_template(IdDostup,_Id_Concept,_Id_template,Old_Termin,Opname,OldComment,_List,Type),
      !,
      upper_lower(New_Synonym, LowerNewText),
      str_to_wlist(LowerNewText,NewList),
      chk_template(NewList,Type),
      gen_term_num(IdN),
      str_int(NewId,IdN),
      idconcept(IdC),
      create_new_comment(OldComment,Text, Old_Termin,NewComment),
      retractall(del_template(_,Opname,NewList,Type)),
     asserta(in_template(IdDostup,IdC,NewId,New_Synonym,Opname,NewComment,NewList,Type)).
create_new_synonym(New_Synonym,Ex,_):-
	make_equal(nc(New_Synonym),Ex).

create_new_comment(OldComment,Text, Old_Termin,NewComment):-
 	not(fronttoken(Text,_,_)),!,
 	concat(OldComment,"; \nсиноним термина ",Comm1),
        concat(Comm1,Old_Termin,NewComment).
create_new_comment(_OldComment,Text, _Old_Termin,Text).

replace_termin(Old,Type,New,Comment):-
	singl(Old,Type,Id_template,Opname),
	create_new_synonym(New,ex(Opname,[]),Comment),
	retractall(in_template(_,_,Id_template,_,_,_,_,_)).
	
get_in_teplate_type(Old,Type,Id_template,Opname):-
	upper_lower(Old, LowerOld),
      str_to_wlist(LowerOld,OldList),
      get_in_template(_IdDostup,_Id_Concept,Id_template,_Old_Termin,Opname,_OldComment,OldList,Type1),
       is_subobject(Type1,Type).

singl(Old,Type,Id_template,Opname):-
	get_in_teplate_type(Old,Type,Id_template,Opname),
	get_in_teplate_type(Old,Type,Id_template1,_Opname1),
	Id_template<>Id_template1,!,
	expr_to_str(Type,StrType),
        %format(Str,"Существует несколько терминов \"%\", совместимых с  типом %. Укажите тип точнее.",Old,StrType),
	msg_n(err,6,[Old,StrType],b_false),
	fail.
singl(Old,Type,Id_template,Opname):-
	get_in_teplate_type(Old,Type,Id_template,Opname),
	!.



create_in_temtplate(Text,Vars,StrVarOb,Comment,Chek,Exec,Dostup):-
      chek_text_nil(Text),
      impose_all(StrVarOb,VarOb,_IputVar),!,
      chek_typeRes(VarOb,VarRes,Ob),
      impose_all(Vars,TermVars,_TypeV),!,
      lst_var(TermVars,ListVar),
      upper_lower(Text, LowerText),
      form_listArg(LowerText,ListVar,List, ListVTmpl),
      chk_template(List,Ob), % проверка на повтор, неоднозначность языка
      chek_ListVar(ListVar,ListVTmpl),
      calc_list(ListVTmpl,LcalcVar), % исправить!!! имена этих  переменных уже могут
      impose_all_not_message(Chek,TermChek,TypeChek),
      chek_termchek(TermChek,TypeChek,_),!,
      chek_dostup(Dostup,IdDostup),
      gen_term_num(IdN),
      str_int(Id,IdN),
      idconcept(IdC),
      str_int(StrIdC,IdC),
      concat(StrIdC,".",S1),
      concat(S1,Id,Op),
      retractall(del_template(_,Op,List,Ob)),
      asserta(in_template(IdDostup,IdC,Id,Text,Op,Comment,List,Ob)),
      impose_all(Exec,TermExec1,_TypeExec),!, 
      %chek_TermExec(TermExec),
     replace_new_var(LcalcVar,TermExec1,TermExec),%если есть переменные типа new
     assert(exec_templ(IdC,Op,[VarRes|LcalcVar],TermChek,TermExec)),
	delete_listvar([VarRes|LcalcVar]).

 replace_new_var([],TermExec,TermExec):-!.
 replace_new_var([ex(Op,[])|T],TermExec1,TermExec):-
 	get_in_template(_,_,_,Name,Op,_,_,ex("new",[])),!,   
 	replace_in_list([TermExec1], ListTermExec2, nc(Name), ex(Op,[])),
 	ListTermExec2=[TermExec2],
 	replace_new_var(T,TermExec2,TermExec).
 replace_new_var([_|T],TermExec1,TermExec):-
 	replace_new_var(T,TermExec1,TermExec).
 	
 chk_template(List,Ob):-
 	get_template(ID_Concept,_Id_template, Text,  _Opname, _Comment, List, Type),
 	not(template_curr_notion(ID_Concept,Ob,Type)),
 	compatible(Ob,Type),!,
 	expr_to_str(Type,StrType),
 	%format(Str,"Уже сушествует внешний шаблон \"%\" типа %, который совместим с типом вводимого шаблона.",Text,StrType),
	msg_n(err,7,[Text,StrType],b_false),
	fail.
 chk_template(List,Ob):-
 	get_in_template(_, _ID_Concept,_Id_template, Text,  _Opname, _Comment, List, Type),
 	compatible(Ob,Type),!,
 	expr_to_str(Type,StrType),
 	%format(Str,"Уже сушествует внутренний шаблон \"%\" типа %, который совместим с типом вводимого шаблона.",Text,StrType),
	msg_n(err,7,[Text,StrType],b_false),
	fail.
 chk_template(_,_).
 
 compatible(Ob,Type):-
 	descriptor(_,X,_),
 	is_subobject(X,Ob),
 	is_subobject(X,Type),!.	
 
 template_curr_notion(ID_Concept,Ob,Ob):-
 	idconcept(ID_Concept).	
 	
chek_dostup("внешний",2):-!.
chek_dostup("наследуемый",1):-!.
chek_dostup("локальный",0):-!.
chek_dostup(X,_):-
	%format(Str,"Термин  \"%\" не является типом  доступа шаблона.",X),
	msg_n(err,8,[X],b_false),
	fail.	

chek_text_nil(Text):-
	not(fronttoken(Text,_,_)),!,
	%format(Str,"Текст шаблона не может быть пустым."),      
      	%dlg_error(Str),
      	msg_n(err,9,[],b_true),
      	fail.
chek_text_nil(_).
		
chek_typeRes(VarOb,_,_):-
	type(VarOb,Input_var),
	not(Input_var=ex("input_var",[])),!,
	expr_to_str(Input_var,StrType),
    	expr_to_str(VarOb,StrVarOb), 
    	%format(Prompt,"Выражение \"%\" типа \"%\" не задает переменную результата.",StrVarOb,StrType),
    	msg_n(err,10,[StrVarOb,StrType],b_false),
    	fail.
chek_typeRes(VarOb,VarRes,Ob):-
	calc(VarOb,VarRes),!,
	calc_type(VarRes,Ob),
	 expr_to_str(Ob,Type1),
        %format( Str,"Строится шаблон типа %",Type1), 
        msg_n(diagn,11,[Type1],b_false).

chek_ListVar([],_):-!.
chek_ListVar([H|T],ListVarTempl):-
	member(H,ListVarTempl),!,
	chek_ListVar(T,ListVarTempl).
chek_ListVar([H|_],ListVarTempl):-
	expr_to_str(H,StrH),
	exprlist_to_str(ListVarTempl,StrList),
	%format(Str,"Переменная % не входит в список переменных:\n%.", StrH,StrList),
	msg_n(err,12,[StrH,StrList],b_true),
	%dlg_note(Str),
	fail.

member(X,[X|_]):-!.
member(X,[_|L]):-member(X,L).   
      	

lst_var(nc(""),[]):-!.
lst_var(ex("l_command",[Term1,Term2]),ListVar):-!,
	lst_var(Term1,ListVar1),
	lst_var(Term2,ListVar2),
	append(ListVar1,ListVar2,ListVar).
lst_var(ex(";",[Term]),ListVar):-!,
	lst_var(Term,ListVar).
lst_var(ex("var",[ex(",",[Term1,Term2]),Type]),ListVar):-!,
	lst_var(ex("var",[Term1,Type]),ListVar1),
	lst_var(ex("var",[Term2,Type]),ListVar2),
	append(ListVar1,ListVar2,ListVar).
lst_var(ex("var",[nc(NameVar),Type]),[ex("var",[nc(NameVar),Type])]):-!.
lst_var(Term,_):-
	expr_to_str(Term,StrTerm),
	%write(Term,"\n"),
	%format(Str,"Выражение % не определяет новую переменную.", StrTerm),
	msg_n(err,14,[StrTerm],b_false),
	fail.

form_listArg(Text,_ListVar,[],[]):-
	not(fronttoken_my(Text,_,_)),!.
form_listArg(Text,ListVar,[w(H)|T],ListVTpl):-
	fronttoken_my(Text,H,Rest),
	not(H="@"),!,
	form_listArg(Rest,ListVar,T,ListVTpl).
form_listArg(Text,ListVar,[var(Type)|T], [Var|Tl]):-
	fronttoken_my(Text,H1,Rest1),
	H1="@",!,
	get_typeVar(Rest1,ListVar, Type,Var,Rest),
	form_listArg(Rest,ListVar,T,Tl).

get_typeVar(Rest1,_, _,_,_):-
	not(fronttoken(Rest1, _,_)),!,
	%msg(err,"Нет имени переменной в конце шаблона."),
	msg_n(err,15,[],b_false),
	fail.
get_typeVar(Rest1,ListVar, Type,Var,Rest):-
	fronttoken(Rest1, X,Rest),!,
	in_list(X,ListVar,Type,Var).

in_list(X,[ex("var",[nc(X),Type])|_],Type,ex("var",[nc(X),Type])):-!.
in_list(X,[_|Tail],Type,Var):-	in_list(X,Tail,Type,Var),!.
in_list(X,[],_,_):-!,
	%format(Str,"Переменной \"%\" нет в списке переменных шаблона.",X),
	msg_n(err,16,[X],b_false),
	fail. 
	
chek_termchek(nc(""),_TypeChek,ex("true_bool",[])):-!.
chek_termchek(TermChek,TypeChek,TermChek):-
          is_subobject(TypeChek,ex("expr_bool",[])),!.
chek_termchek(TermChek,TypeChek,_):-
           expr_to_str(TermChek,StrTmChk),
           expr_to_str(TypeChek,StrTpChk),
           %format(Str,"Условие  \"%\" имеет тип % и не является булевым выражением.",StrTmChk,StrTpChk),
           msg_n(diagn,17,[StrTmChk,StrTpChk],b_false),
           fail.



delete_listvar([ex(Id,[])|T]):-!,
	retractall(in_template(_,_,_,_,Id,_,_,_)),
	retractall(descriptor(_,ex(Id,[]),_)),
	retractall(element(_,ex(Id,[]),_)),
	delete_listvar(T).
delete_listvar([]):-!.

do_exec(Opname,Arglist,ListVar,Chek,Exec,Res):- 
	ListVar=[_ResVar|ArgVar],
	replace_var_on_ex(Arglist,ArgVar,Chek,ChekTerm),
	chk_condTempl(Opname,ChekTerm),
	concat("return",Opname,RetOp),
	idconcept(Id),
	assert(descriptor(Id,ex(RetOp,[]),32001)),
	replace_var_on_ex([ex(RetOp,[])|Arglist],ListVar,Exec,ExecTerm), 
	calc_resVar(Opname,ExecTerm,Res).
	
replace_var_on_ex([],_,Term,Term):-!.	
replace_var_on_ex([HArg|TArg],[HVar|TVar],Term,ResTerm):-
	replace_in_list([Term],ListTerm1,HVar,HArg), 
	ListTerm1=[Term1],	
	replace_var_on_ex(TArg,TVar,Term1,ResTerm).
	
chk_condTempl(_,ChekTerm):-ChekTerm=nc(""),!.
chk_condTempl(_,ChekTerm):-
	calc_on_approx(ChekTerm,Desc1),
	calc(ex("true_bool",[]),Desc2),
	Desc1=Desc2,!.
chk_condTempl(Opname,ChekTerm):-
	get_in_template(_,_,_,TextTmpl,Opname,_,_,_),
	expr_to_str(ChekTerm,StrTerm),
	%format(Str,"В аппроксимации не выполнено условие  \"%\"\n шаблона \"%\".",StrTerm,TextTmpl),
	msg_n(err,18,[StrTerm,TextTmpl],b_false),
	fail.


calc_true(Ex,Res):-calc(Ex,Res),!.
calc_true(_,undef).

calc_resVar(Opname,ExecTerm,Res):-
	calc_true(ExecTerm,_ResT), 
	concat("return",Opname,RetOp),
	calc_on_approx(ex(RetOp,[]),Res),
	not(Res=ex(RetOp,[])),!,
	retractall(descriptor(_,ex(RetOp,[]),_)),
	retractall(resalt_op(_,ex(RetOp,[]),_)).
calc_resVar(Opname,ExecTerm,_Res):-
	get_in_template(_,_,_,TextTmpl,Opname,_,_,_),
	expr_to_str(ExecTerm,StrTerm),
	%format(Str,"Не определена переменная результата \n шаблона \"%\";\nвозможна ошибка в действии этого шаблона .",TextTmpl),
	msg_n(err,13,[TextTmpl],b_false),	
	%format(Str1,"Выражение действия шаблона:\n\"%\".",StrTerm),
	msg_n(diagn,14,[StrTerm],b_false),
	concat("return",Opname,RetOp),
	retractall(descriptor(_,ex(RetOp,[]),_)),
	retractall(resalt_op(_,ex(RetOp,[]),_)),
	fail.

expr_to_exprlist(ex(",",[Ex1,Ex2]),Exprlist):-!,
	expr_to_exprlist(Ex1,List1),
	expr_to_exprlist(Ex2,List2),
	append(List1,List2,Exprlist),!.
/*
expr_to_exprlist(ex(",new",[Ex1,Ex2]),Exprlist):-!,
	expr_to_exprlist(Ex1,List1),
	expr_to_exprlist(Ex2,List2),
	append(List1,List2,Exprlist),!.
*/
expr_to_exprlist(Expr,[Expr]).


is_attributs(_,[],[]):-!.
is_attributs(DescOb,[H|T],[Hattr|Tattr]):-
	calc(ex(dom,[H]),Ob1),
	is_subobject(DescOb,Ob1),!,
	calc(ex(cod,[H]),Hattr),
	is_attributs(DescOb,T,Tattr).
is_attributs(DescOb,[_|T],Tattr):-
	is_attributs(DescOb,T,Tattr).

list_not_repeat([],[]).
list_not_repeat([H|T],[H|Tres]):-
   remove_from_list(T,H,[],T1),list_not_repeat(T1,Tres).


 loadInConcept(ID):-
 	used_concept(_,_,ID),!.
 loadInConcept(ID):-
 	idconcept(IdC),
 	ID=IdC,!,
 	%format(Str,"Имя загружаемого понятия  не должно совпадать с именем текущего понятия"),
 	msg_n(err,20,[],b_false),
 	retractall(fatal_error),
	assert(fatal_error),
 	fail.
 loadInConcept(ID):-
 	insertConcept_inCurrent(ID),
 	not(fatal_error),
         str_int(S_ID,ID),
         name_file(Concept_name,S_ID,_,_),!,
         %format(Msg,"Загружено понятие  \"%\".",Concept_name),
        msg_n(approx,19,[Concept_name],b_false).   
/*
 is_subEnvironment(F,F):- !.
 is_subEnvironment(F1,F3):-
 	name_file(_,F1,F2),!,
 	is_subEnvironment(F2,F3). 
 */



get_term_from_concept(ID,_):-
	str_int(Fname,ID),
    	notion_base(CatalogFile),
	filenamepath(CatalogFile,  Path, _Name),
	filenamepath(PathFname,  Path, Fname),
   	/*********************
   	TO UNCOMMENT
   	consult_concept_unzip(PathFname,unzip_concept),
   	waitFile(PathFname,unzip_concept), 
      	not(existfile(PathFname)),!,
    	*********************/ 
    	not(existfile(PathFname)),!,
      
   	%format(Str,"Отсутствует файл %, содержащий понятие - описание понятия недоступно.",Fname),
   	msg_n(err,21,[Fname],b_false),
   	retractall(fatal_error),
	assert(fatal_error),
   	fail.
get_term_from_concept(ID,Term):-
   	str_int(Fname,ID),
   	format(Prompt1," Невозможно открыть файл %s.",Fname),
   	notion_base(CatalogFile),
	filenamepath(CatalogFile,  Path, _Name),
	filenamepath(PathFname,  Path, Fname), 
   	trap(openread(input,PathFname),Err1,error_msg(Prompt1,Err1)),
   	readdevice(input),
   	readterm_repeat(input),
   	format(Prompt2," Невозможно прочитать терм в файле %s.",Fname),
   	trap(readterm(concept,Term),Err2,error_msg(Prompt2,Err2)).
get_term_from_concept(_,_):-
   	readdevice(keyboard),closefile(input),
   	fail.

insertConcept_inCurrent(ID):-
     	retractall(_,tmpConcept),
     	get_term_from_concept(ID,Term),
     	term_ToConceptOrTmp(Term),
    	fail.
insertConcept_inCurrent(_):- %дозагрузка  used_concept(_,IdC) из tmpConcept
	retract(tmp(Term)),
	assert(Term, concept),
	fail.
insertConcept_inCurrent(_).



error_msg(S,E) :-
	format(STR,"% - Error code = %",S,E),
	msg(err,STR),
	retractall(fatal_error),
	assert(fatal_error).

insertElConcept_inCurrent(ID,Ref):-
     retractall(_,tmpConcept),
     str_int(StrID,ID),
     str_int(StrRef,Ref),
     get_term_from_concept(ID,Term), %  возврат сюда при неуспехе
     term_str(concept, Term,StrTerm),
     replaceStr_all(StrID,StrRef,StrTerm,NewStrTerm),
     format(Msg,"Ошибка при замене IdConcept % на ссылку %.",StrID,StrRef),
     trap(term_str(concept, NewTerm, NewStrTerm),_, error_msg( Msg,0)),
     term_ToConceptOrTmp(NewTerm),
    fail.
insertElConcept_inCurrent(_,_):- %дозагрузка  used_concept(_,_) из tmpConcept
	retract(tmp(Term)),
	assert(Term, concept),
	fail.
insertElConcept_inCurrent(IdConcept,IdRef):-
	idconcept(IdCurrent),
	retractall(used_concept(_,IdCurrent,IdRef)),
	retractall(used_concept(_,IdCurrent,IdConcept)),
	assert(used_concept(0,IdCurrent,IdConcept)).


replaceStr_all(Old,_,Str,Str):-
	not(searchstring(Str,Old,  _)),!.
replaceStr_all(Old,New,Str,NewStr):-
	searchstring(Str,Old,  Pos),

	P1 is Pos-1,
	
	frontstr(P1, Str,  StartStr,  EndStr1),
	str_len(Old,L),
	frontstr(L,EndStr1,_,EndStr),
	replaceStr_all(Old,New,EndStr, NewEnd),
	concat(StartStr,New,S1),
	concat(S1,NewEnd,NewStr).
	
%При изменении в concept изменить term_ToConceptOrTmp из calctls.pro и changeIdInConcept в editor.pro 

term_ToConceptOrTmp(
	in_template(IdDostup, IdC, Id_template,  Text,  Opname,  Comment, List, TypeRes))
	:-!,
	IdDostup=1,
	not(used_concept(_,_,IdC)),
%!!!!! Здесь нужна проверка на противоречивость языка и прерывание !!!!!!!!!!!!!!!!!!!!
	assert(in_template(IdDostup, IdC, Id_template,  Text,  Opname,  Comment, List, TypeRes)).
term_ToConceptOrTmp(exec_templ(IdC, Opname, LcalcVar,TermChek, TermExec))
	:-!,
	not(used_concept(_,_,IdC)),
	assert(exec_templ(IdC, Opname, LcalcVar,TermChek, TermExec)).
term_ToConceptOrTmp(del_template(IdC, Opname,List,Type))
	:-!,
	not(used_concept(_,_,IdC)),
	assert(del_template(IdC, Opname,List,Type)).
term_ToConceptOrTmp(element_concept(IdC, Expr, RefConcept, ID_Concept_Type) )
	:-!,
	not(used_concept(_,_,IdC)),
	assert(element_concept(IdC, Expr, RefConcept, ID_Concept_Type)).
term_ToConceptOrTmp(resalt_op(IdC,Expr,Res)):-!,
	not(used_concept(_,_,IdC)),
	assert(resalt_op(IdC,Expr,Res)).
term_ToConceptOrTmp(rwt_rule(IdC, Id_rule,  Msg, ListVar, Lterm, Rterm, PredCond, PostCond)):-!,
	not(used_concept(_,_,IdC)),
	assert(rwt_rule(IdC, Id_rule,  Msg, ListVar, Lterm, Rterm, PredCond, PostCond)).
term_ToConceptOrTmp(descriptor(IdC,Expr,W)):-!,
	not(used_concept(_,_,IdC)),
	assert(descriptor(IdC,Expr,W)).
term_ToConceptOrTmp(not_eq_desc(N,E1,E2)):-!,
	not(used_concept(_,_,N)),
	assert(not_eq_desc(N,E1,E2)).
term_ToConceptOrTmp(subobject(N,E1,E2)):-!,
	not(used_concept(_,_,N)),
	assert(subobject(N,E1,E2)).
term_ToConceptOrTmp(not_subobject(N,E1,E2)):-!,
	not(used_concept(_,_,N)),
	assert(not_subobject(N,E1,E2)).
term_ToConceptOrTmp(element(N,E1,E2)):-!,
	not(used_concept(_,_,N)),
	assert(element(N,E1,E2)).
term_ToConceptOrTmp(not_element(N,E1,E2)):-!,
	not(used_concept(_,_,N)),
	assert(not_element(N,E1,E2)).
term_ToConceptOrTmp(current_inconsist(N)):-!,
	not(used_concept(_,_,N)),
	assert(current_inconsist(N)).
term_ToConceptOrTmp(inconsist(N,E1,E2))	:-!,
	not(used_concept(_,_,N)),
	assert(inconsist(N,E1,E2)).
%этот особый случай должен быть в конце	
term_ToConceptOrTmp(used_concept(T,IdC,Id_used)):-!,
	not(used_concept(_,_,Id_used)),
	assert(tmp(used_concept(T,IdC,Id_used))).
term_ToConceptOrTmp(idconcept(IdC)):-!,
	idconcept(Id),
	assert(tmp(used_concept(0,Id,IdC))).

/*
%Эти предикаты не вводятся!!!!!!
term_ToConceptOrTmp:-
	tmp(defin1(N,S)),
	assert(defin1(N,S)),
	fail.
term_ToConceptOrTmp:-
	tmp(defin2(N,S)),
	assert(defin2(N,S)),
	fail.
term_ToConceptOrTmp:-
	tmp(last_cmd(N,S)),
	assert(last_cmd(N,S)),
	fail.
*/	

readterm_repeat(_).
readterm_repeat(File):-not(eof(File)),readterm_repeat(File).

 
 make_elements(ex(",",[Elmt,Elmts]),Ob):-!,
           make_elements(Elmt,Ob),
           make_elements(Elmts,Ob).
/*
make_elements(ex(",new",[Elmt,Elmts]),Ob):-!,
           make_elements(Elmt,Ob),
           make_elements(Elmts,Ob).
           
*/           
make_elements(Elmt,Ob):-
           make_element(Elmt,Ob).


make_subobjects(_,[]):-!.
make_subobjects(Ob,[H|T]):-
	calc(ex(object,[H]),Ob1),
	make_subobject(Ob1,Ob),!,
	make_subobjects(Ob,T).



remove_from_list([],_,L,L).
remove_from_list([H|T],H,L,ResL):-!,remove_from_list(T,H,L,ResL).
remove_from_list([H|T],N,L,[H|ResT]):-remove_from_list(T,N,L,ResT).

resalt_op_txt(""):-
   not(resalt_op(_,_,_)),!.
resalt_op_txt(_):-
   retractall(glob_string(_)),
   assert(glob_string("\n\n	Соотношения:")),
   fail.
resalt_op_txt(_):-
   resalt_op(_,Ex,Desc),
   retract(glob_string(Str1)),
   expr_to_str(Ex,Str2),
   expr_to_str(Desc,Str3),
   calc_type(Desc,Type),
   expr_to_str(Type,T),
   row_resalt_op_txt(T,Str2,Str3,Str4),
   concat(Str1,Str4,Str),
   assert(glob_string(Str)),
   fail.
resalt_op_txt(Str):-
   retract(glob_string(Str)).

row_resalt_op_txt(T,Str1,Str2,Str):-
   Str1=Str2,T="ob",!,
   format(Str,"%s%s%s","\n",Str1," - область;").
row_resalt_op_txt(T,Str1,Str2,Str):-
   Str1=Str2,T="mor",!,
   format(Str,"%s%s%s","\n",Str1," - отображение;").   
row_resalt_op_txt(_,Str1,Str2,Str):-
   format(Str,"%s%s%s%s%s","\n",Str1," = ",Str2,";").
   
scan(Str,[H|T]):-fronttoken(Str,H,Rest),!,
			scan(Rest,T).
scan(Str,[]):-not(fronttoken(Str,_,_)).

%set_dostup_in_template(TextTemplate,Dostup):-
	

subobjects_of(DescOb,ListSubob):-bound(DescOb),
   findall(D,subobject(_,D,DescOb),ListOb),
   subobjects_of_list(ListOb,L1Subob),
   append(ListOb,L1Subob,ListSubob),!.

subobjects_of_list([],[]):- !.
subobjects_of_list([H|T],ListSub):-
   subobjects_of(H,HL),
   subobjects_of_list(T,TL),
   append(HL,TL,List),
   list_not_repeat(List,ListSub).


what_attrs(Ob,X):-
         calc(ex("attrs",[]),Attrs),
         calc(Ob,DescOb),
         findall(El,element_of_object(El,Attrs),ListEl),
         list_not_repeat(ListEl,List),
         is_attributs(DescOb,List,ListAttr),
         not(ListAttr=[]),!,
         exprlist_to_str_nl(ListAttr,X).
what_attrs(_,X):- X= " свойства неизвестны".

 world_concept(X):-
   retractall(glob_string(_)),
   X="\n                     МИР ТЕКУЩЕГО ПОНЯТИЯ\n",
   assert(glob_string(X)),
   fail.

world_concept(X):-
   retract(glob_string(X1)),
   resalt_op_txt(X2),
   concat(X1,X2,X),
   assert(glob_string(X)),
   fail.   
/*
world_concept(X):-
   retract(glob_string(X1)),
   not_eq_desc_txt(X2),
   concat(X1,X2,X),
   assert(glob_string(X)),
   fail.
world_concept(X):-
   retract(glob_string(X1)),
   subobject_txt(X2),
   concat(X1,X2,X),
   assert(glob_string(X)),
   fail.
world_concept(X):-
   retract(glob_string(X1)),
   element_txt(X2),
   concat(X1,X2,X),
   assert(glob_string(X)),
   fail.      
*/
world_concept(X):-
   retract(glob_string(X1)),
   all_inconsists(X2),
   concat(X1,X2,X),
   assert(glob_string(X)),
   fail.
world_concept(X):-   
   retract(glob_string(X)).

is_concept(Term):-
	is_element(Term,ex("concepts",[])),!.
is_concept(Term):-
	expr_to_str(Term,StrTerm),
	%format(Str,"\"%\" не является именем доступного понятия.",StrTerm),
	msg_n(err,68,[StrTerm],b_false),
	fail.

create_constant_ifnew(nc(Name),Term,Desc):-
	 impose_all_not_message(Name,Ex,Type),
         is_subobject(Type,Term),!,
	calc(Ex,Desc).	
create_constant_ifnew(Name,Term,DescConcept):-
	create_new_constant(Name,Term,DescConcept).
/*	
predoksAnd_I(_, _):-
	retractall(_,tmpConcept),
	fail.
predoksAnd_I(ID, _):-
	get_term_from_concept(ID,parent(Which,Who)),
	assert(tmp(parent(Which,Who)),
	fail.
predoksAnd_I(IdConcept, ListConcept):-
	formList_predoks([],[IdConcept],ListConcept),	
	retractall(_,tmpConcept).

%formList_predoks(list Old,list New,list Res)		
formList_predoks(Res,[],Res):-!.
formList_predoks(New,Old,Res):-
	append(Old,New,Old1),
	connected(Old1,New,New1),
	formList_predoks(Old1,New1,Res).

connected(Old,Bord,New):-
	retractall(list(_)),
	assert(list([])),
	conrepeat(Old,Bord,New).

conrepeat(Old,[H|T],_New):-
	tmp(parent(H,X)),
	not(member(X,Old)),
	list(List),
	not(member(X,List),
	retract(list(List)),
	assert(list([X|List])),
	fail.
conrepeat(Old,[_H|T],_):-
	conrepeat(Old,T,_),
	fail.
conrepeat(_,_,New):-
	retract(list(New)).
	
%insertElConcept_inCurrent(IdConcept,ListConceptId,IdRef)	
insertElConcept_inCurrent(ID,ListId,Ref):-
     retractall(_,tmpConcept),
     liststr_listint(ListStrId,ListId),
     str_int(StrRef,Ref),
     get_term_from_concept(ID,Term), %  возврат сюда при неуспехе
     term_str(concept, Term,StrTerm),
     replaceListStr_all(ListStrId,StrRef,StrTerm,NewStrTerm),
     format(Msg,"Ошибка при замене IdConcept % на ссылку %.",StrID,StrRef),
     trap(term_str(concept, NewTerm, NewStrTerm),_, error_msg( Msg,0)),
     term_ToConceptOrTmp(NewTerm), %!!!!!! Нужно переделать
    fail.
insertElConcept_inCurrent(_,_,_):- %дозагрузка  used_concept(_,_) из tmpConcept
	retract(tmp(Term)),
	assert(Term, concept),
	fail.
insertElConcept_inCurrent(IdConcept,_ListId,IdRef):-
	idconcept(IdCurrent),
	retractall(used_concept(_,IdRef)),
	retractall(used_concept(IdCurrent,IdConcept)),
	assert(used_concept(IdCurrent,IdConcept)).

 replaceListStr_all(ListStrId,StrRef,StrTerm,NewStrTerm):-
  	retractall(str(_)),
  	assert(str(StrTerm)),
  	replace_repeat_all(ListStrId,StrRef,NewStrTerm).

 replace_repeat_all([H|T],StrRef,_):-
 	retract(str(StrTerm)),
 	replaceStr_all(H,StrRef,StrTerm,StrTerm1),
 	assert(str(StrTerm1)),
 	replace_repeat_all(T,StrRef,_).
 replace_repeat_all(_,_,NewStrTerm):-
 	retract(str(NewStrTerm)).
 */
 
 str_to_wlist(Str,[]):-
	not(fronttoken_my(Str,_,_)),!.
str_to_wlist(Str,[w(H)|T]):-
	fronttoken_my(Str,H,Rest),
	str_to_wlist(Rest,T).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
%include "form\\select.pro"

select_report(Ex,Ob,Report):-
        %retractall(text_mode_answer(0)),
        %assert(text_mode_answer(0)),
	expr_to_exprlist(Ex,Exlist),
	calc(Ob,DescOb),
	chk_attr(Exlist,Ob,DescOb,Listmor),
	exprlist_to_slist(Exlist,HList),
	listlen(HList,0,N),
	respond(Listmor,DescOb,List),
	append(HList,List,Listreport),!,
	create_report(N,Listreport,Report).
	
chk_attr([],_,_,[]).
chk_attr([H|T],Ob,DescOb,[Hmor|Tmor]) :-
	subobject_or_element(DescOb,Ob1),
        calc_on_approx(ex(disting_mor,[H,Ob1]),Hmor),!,
	chk_attr(T,Ob,DescOb,Tmor).
chk_attr([H|_],Ob,_,_) :-
	expr_to_str(H,Hstr),
	expr_to_str(Ob,Obstr),
	format(_Prompt,"% не является свойством %.",Hstr,Obstr),
	%dlg_note("Противоречие",Prompt),!,
	fail.

respond([],_,[]):-!.
respond(Listmor,Ob,List) :-
	findall(El,element_of_object(El,Ob),Listexpr),
	list_not_repeat(Listexpr,ListEl),
	respond1(Listmor,ListEl,[],List).
respond1(_,[],List,List):-!.
respond1(Listmor,[HEl|TEl],Aux,List) :-
	make_row(Listmor,HEl,Listrow),
	append(Aux,Listrow,Aux1),
	respond1(Listmor,TEl,Aux1,List).
	
listlen([],L,L).
listlen([_|T],CurrL,L):- NewCurrL=CurrL+1,listlen(T,NewCurrL,L).

make_row([],_,[]).
make_row([Hmor|Tmor],El,[H|T]) :-
	calc_or_undefined(ex("appl",[Hmor,El]),H),
	make_row(Tmor,El,T).

calc_or_undefined(Ex,X):-
	calc_on_approx(Ex,DescEx),
	not(DescEx = Ex),!,
	expr_to_str(DescEx,X).
calc_or_undefined(_,"-").

subobject_or_element(DescOb,DescOb).
subobject_or_element(DescOb,Ob1):-
        subobject(_,DescOb,Ob1).
/*subobject_or_element(DescOb,Ob1):-
        element(DescOb,Ob1).
subobject_or_element(DescOb,Ob1):-
        element(DescOb,Ob2),
        strong_subobject(Ob2,Ob1).
*/
subobject_or_element(DescOb,Ob1):-
        subobject(_,DescOb,Ob2),
        strong_subobject(Ob2,Ob1).
        
strong_subobject(Ob2,Ob1):-
        subobject(_,Ob2,Ob1).
strong_subobject(Ob2,Ob1):-
        subobject(_,Ob2,Ob),
	strong_subobject(Ob,Ob1).

/* Добавила 20.01.03. Программа create_report  новая*/
create_report(N,Listreport,Report):-
	get_width_colons(N,Listreport,[],ListWidth),

	%TabSize = edit_GetTabSize(),
	%get_spaces_colons(ListWidth,TabSize,ListSpaces),
	%create_report1(N,ListSpaces,TabSize,Listreport,"",Report).

	create_report2(N,ListWidth,Listreport,"",Report).

get_width_colons(_,[],ListW,ListW):- !.
get_width_colons(N,ListS,Aux,ListW):-
	get_rowlist(ListS,List1,N,Rest),
	get_listlength(List1,LW),
	list_max_of_lists(Aux,LW,Aux1),
	get_width_colons(N,Rest,Aux1,ListW).

get_rowlist([H|T],[H|T1],C,Rest):- C>0,!,
	C1=C-1, get_rowlist(T,T1,C1,Rest).
get_rowlist(Rest,[],_,Rest):- !.

get_listlength([],[]):- !.
get_listlength([H|T],[HLen|TL]):-
	str_len(H,HLen),get_listlength(T,TL).

list_max_of_lists([],LW,LW):- !.
list_max_of_lists([H|T],[H1|T1],[H|T2]):-
	H>H1,!,list_max_of_lists(T,T1,T2).
list_max_of_lists([_|T],[H1|T1],[H1|T2]):-
	list_max_of_lists(T,T1,T2).

empty_string(_,R,Str,Str,R):- !.
empty_string(Empt,C,Aux,Str,R):- C<R,!,
	concat(Empt,Aux,Aux1),C1=C+1,
	empty_string(Empt,C1,Aux1,Str,R).

check_max_len_str(Str,mlen,Str):- str_len(Str,Len),Len < mlen +1,!.
check_max_len_str(Str,mlen,Str1):- frontstr(mlen,Str,Str1,_).





/* Вариант 2 с '_' */
create_report2(_,_,[],Report,Report):- !. 
create_report2(N,ListWidth,List,Aux,Report):- 
	get_rowlist(List,List1,N,Rest),
	list_to_string2(ListWidth,"",List1,Str),
	check_max_len_str(Str,mlen,Str1),
	format(Aux1,"%\n%",Aux,Str1),
	create_report2(N,ListWidth,Rest,Aux1,Report).

list_to_string2(_,Str,[],Str):- !.
list_to_string2([HW|TW],Aux,[H|T],Str):-
	str_len(H,Len),
	R=HW-Len,
	empty_string("_",0,"",ZStr,R),
	concat(H,ZStr,H1),
	format(Aux1,"%\t%",Aux,H1), 
	list_to_string2(TW,Aux1,T,Str).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
monomorphism(A,J,B):-
        is_equal(J,ex(inclusion,[A,B])),!.

monomorphism(_,J,_):-
        J=ex(mor_eq,[_,_]),!.
monomorphism(A,J,B):-morphism(A,J,B),
	is_equal(
	ex(com,[ex(proj1,[A,A]),ex(mor_eq,[ex(com,[J,ex(proj1,[A,A])]),ex(com,[J,ex(proj2,[A,A])])])])
        ,
	ex(com,[ex(proj2,[A,A]),ex(mor_eq,[ex(com,[J,ex(proj1,[A,A])]),ex(com,[J,ex(proj2,[A,A])])])])
        ).



morphism(A,J,B):-is_equal(ex(dom,[J]),A),
	is_equal(ex(cod,[J]),B).	

make_morphism(A,J,_):-
        make_equal(ex(dom,[J]),A),fail.
make_morphism(_,J,B):-	
	make_equal(ex(cod,[J]),B).	



% CATEVAL.PRO



/* ?®?­®??­?? ???«?????­®?® ?®?®? 
sss:-

	make_morphism(ex(names_of_objects,[]),ex(name_of_id,[]),ex(names_of_morphisms,[])),
	make_morphism(ex(names_of_objects,[]),ex(name_of_domain,[]),ex(names_of_morphisms,[])),
	make_morphism(ex(names_of_objects,[]),ex(name_of_cod,[]),ex(names_of_morphisms,[])),

	make_equal(ex(com,[ex(name_of_domain,[]),ex(name_of_id,[])]),ex(id,[ex(names_of_objects,[])])),
	make_equal(ex(com,[ex(name_of_cod,[]),ex(name_of_id,[])]),ex(id,[ex(names_of_objects,[])])),

	M2=ex(eq,
	  [ex(com,[ex(name_of_domain,[]),ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])]),
	   ex(com,[ex(name_of_cod,[]),ex(proj2,[ex(names_of_morphisms,[]),   ex(names_of_morphisms,[])])])]),

	I1=ex(com,[ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])]),
	   ex(mor_eq,[ex(com,[ex(name_of_domain,[]),ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])]),
	   ex(com,[ex(name_of_cod,[]),ex(proj2,[ex(names_of_morphisms,[]),   ex(names_of_morphisms,[])])])])]),

	I2=ex(com,[ex(proj2,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])]),ex(mor_eq,[ex(com,[ex(name_of_domain,[]),
	   ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])]),ex(com,[ex(name_of_cod,[]),
           ex(proj2,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])])])])]),
	   /*    w(M2),w(I1),w(I2),*/


/*
	make_morphism(M2,I1,ex(names_of_morphisms,[])),
	make_morphism(M2,I2,ex(names_of_morphisms,[])),
	make_morphism(M2,ex(name_of_com,[]),ex(names_of_morphisms,[])),
	
	*/
	

	make_equal(
		ex(com,[ex(name_of_dom,[]),ex(name_of_com,[])])
		,
		ex(com,[ex(name_of_dom,[]),I2])
		),

	make_equal(
		ex(com,[ex(name_of_cod,[]),ex(name_of_com,[])])
		,
		ex(com,[ex(name_of_cod,[]),I1])
		),


	make_equal(

 ex(com,[ex(name_of_com,[]),ex(in_eq,[ex(com,[ex(name_of_domain,[]),
 ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])]),ex(com,[ex(name_of_cod,[]),
 ex(proj2,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])]),ex(prod_mor,[ex(com,[
 ex(name_of_id,[]),ex(name_of_cod,[])]),ex(id,[ex(names_of_morphisms,[])])])])]),
 ex(id,[ex(names_of_morphisms,[])])),

	make_equal(

 ex(com,[ex(name_of_com,[]),ex(in_eq,[ex(com,[ex(name_of_domain,[]),
 ex(proj1,[ex(names_of_morphisms,[]),ex(nams_of_morphisms,[])])]),ex(com,[ex(name_of_cod,[]),
 ex(proj2,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])]),ex(prod_mor,[ex(id,[ex(names_of_morphisms,[])]),
 ex(com,[ex(name_of_id,[]),ex(name_of_dom,[])])])])]),ex(id,[ex(names_of_morphisms,[])])).


b:-
	M2=ex(
	eq,[ex(com,[ex(name_of_domain,[]),
	ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])]),ex(com,[ex(name_of_cod,[]),
	ex(proj2,[ex(names_of_morphisms,[]),   ex(names_of_morphisms,[])])])]),

	I1=ex(com,[ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])]),
	ex(mor_eq,[ex(com,[ex(name_of_domain,[]),ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])]),
	    ex(com,[ex(name_of_cod,[]),ex(proj2,[ex(names_of_morphisms,[]),   ex(names_of_morphisms,[])])])])]),

	I2=ex(com,[ex(proj2,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])]),ex(mor_eq,[ex(com,[ex(name_of_domain,[]),
	ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])]),ex(com,[ex(name_of_cod,[]),
	ex(proj2,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])])])])]),
	     /*  w(M2),w(I1),w(I2),*/


	M3=ex(eq,[ex(com,[I2,ex(proj1,[M2,M2])]),ex(com,[I1,ex(proj2,[M2,M2])])]),
/*	w(M3),*/

	J1=ex(com,[ex(proj1,[M2,M2]),ex(eq,[ex(com,[I2,ex(proj1,[M2,M2])]),ex(com,[I1,ex(proj2,[M2,M2])])])]),
/*	w(J1),*/
	J2=ex(com,[ex(proj2,[M2,M2]),ex(eq,[ex(com,[I2,ex(proj1,[M2,M2])]),ex(com,[I1,ex(proj2,[M2,M2])])])]),
/*	w(J2),*/



	make_equal(
	ex(com,[ex(name_of_com,[]),ex(in_eq,[ex(com,[ex(name_of_domain,[]),
	ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])]),ex(com,[ex(name_of_cod,[]),
	ex(proj2,[ex(names_of_morphisms,[]),ex(name_of_com,[])])]),ex(prod_mor,[ex(com,[of_com,[]),J1]),
	ex(com,[I2,J2])])])]),
	ex(com,[ex(name_of_com,[]),ex(in_eq,[ex(com,[ex(name_of_domain,[]),
	ex(proj1,[ex(names_of_morphisms,[]),ex(names_of_morphisms,[])])]),ex(com,[ex(name_of_cod,[]),
	ex(proj2,[ex(names_of_morphisms,[]),ex(name_of_com,[])])]),ex(prod_mor,[ex(com,[ex(com,[ex(name_of_com,[]),J2])])])])).

  этот текст временно исключен */





check_conditions(ex("dom",[F,El])) :-!,
		 is_element(El,ex("mor",[F])).
	
	check_conditions(ex("appl",[F,El])) :-!,
		 is_element(El,ex("dom",[F])).
	check_conditions(ex("com",[F1,F2])) :-!,
		 is_subobject(ex("dom",[F1]),ex("cod",[F2])).
	
	%check_conditions(ex(dom,[_])):-!.
	%check_conditions(ex(cod,[_])):-!.
	%check_conditions(ex(fin,[_])):-!.
	%check_conditions(ex(init,[_])):-!.
	%check_conditions(ex(id,[_])):-!.


	%check_conditions(ex(final,[])):-!.
	%check_conditions(ex(initial,[])):-!.

	%check_conditions(ex(prod_ob,[_,_])):-!.
	%check_conditions(ex(proj1,[_,_])):-!.
	%check_conditions(ex(proj2,[_,_])):-!.
	check_conditions(ex(prod_mor,[F,G])):-!,
	                   is_equal(ex(dom,[F]),ex(dom,[G])).
	
	%check_conditions(ex(coprod_ob,[_,_])):-!.
	%check_conditions(ex(coproj1,[_,_])):-!.
	%check_conditions(ex(coproj2,[_,_])):-!.
	check_conditions(ex(coprod_mor,[F,G])):-!,
	                   is_equal(ex(cod,[F]),ex(cod,[G])).

	check_conditions(ex(eq,[F1,F2])):-!,
	        is_equal(ex(dom,[F1]),ex(dom,[F2])),
		is_equal(ex(cod,[F1]),ex(cod,[F2])).
	
	check_conditions(ex(mor_eq,[F1,F2])):-!,
	        is_equal(ex(dom,[F1]),ex(dom,[F2])),
		is_equal(ex(cod,[F1]),ex(cod,[F2])).

	check_conditions(ex(in_eq,[F1,F2,F])):-!,
	        is_equal(ex(dom,[F1]),ex(dom,[F2])),
		is_equal(ex(cod,[F1]),ex(cod,[F2])),
		is_equal(ex(cod,[F]),ex(dom,[F1])),
		is_equal(ex(com,[F1,F]),ex(com,[F2,F])).

	%check_conditions(ex(ob_mor,[_,_])):-!.
	%check_conditions(ex(ev,[_,_])):-!.
	check_conditions(ex(lambda,[G,T,T1])):-!,
	        is_equal(ex(dom,[G]),ex(prod_ob,[T,T1])).


%check_conditions(ex(true,[])):-!.
%check_conditions(ex(false,[])):-!.
%check_conditions(ex(omega,[])):-!.
check_conditions(ex(char_f,[F])):-!,
                 monomorphism(ex(dom,[F]),F,ex(cod,[F])),!.


check_conditions(ex(isom_ver_dom,[F])):-!,
                 monomorphism(ex(dom,[F]),F,ex(cod,[F])),!.
check_conditions(ex(ver_dom,[G])):-!,
                 is_equal(ex(cod,[G]),ex(omega,[])).

check_conditions(ex(inclusion,[A,B])):-!,
                 is_subobject(A,B).

check_conditions(ex(image,[F,B])):-!,
         	is_subobject(B,ex(dom,[F])).

                 
check_conditions(ex(terms,[])):-!.
check_conditions(ex(descs,[])):-!.
check_conditions(ex(concepts,[])):-!.
check_conditions(ex(objcts,[])):-!.
check_conditions(ex(maps,[])):-!.

check_conditions(ex(names_of_objects,[])):-!.
check_conditions(ex(names_of_morphisms,[])):-!.
check_conditions(ex(name_ob,[X])):-
       calc_type(X,T),
       calc(ex("ob",[]),Ob),
       T=Ob,!.
check_conditions(ex(name_mor,[X])):-
       calc_type(X,T),
       calc(ex("mor",[]),Mor),
       T=Mor,!.
check_conditions(ex(denote_ob,[Elm])):-
       is_element(Elm,ex(names_of_objects,[])),!.
check_conditions(ex(denote_mor,[Elm])):-
       is_element(Elm,ex(names_of_morphisms,[])),!.

check_conditions(ex(disting_mor,[_,_])):-!.
       
check_conditions(_).



	depth(Ex,D):-descriptor(_,Ex,D),!.
	depth(ex("set",_),1):-!.
	depth(ex(_,Exlist),D):-!,
			depthlist(Exlist,N),
			D=N+1.
	depth(_,0).
		
	depthlist([],-1).
	depthlist([HE|TE],D):-depth(HE,D),
		depthlist(TE,DT),
		D>=DT,!.
	depthlist([HE|TE],D):-depth(HE,DE),
		depthlist(TE,D),
		D>=DE,!.		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555 


 
	cat_eval(Exp,Exp):-
	        descriptor(_,Exp,_),!.
	cat_eval(Exp,Desc):-
		resalt_op(_,Exp,Desc),!.
	cat_eval(Exp,Desc):-
		%calc_type(Exp,_),!,
		check_conditions(Exp),
		act(Exp),
		reduce(0,Exp,Redex), %исправить так, чтобы сначала приводилось, а потом добавлялось
		make_equal(Exp,Redex),
		calc(Exp,Desc),!.
		
	act(Exp):-bound(Exp),
		weigh(Exp,We),
		idconcept(Id),
		assert(descriptor(Id,Exp,We)),
		%assert(resalt_op(Exp,Exp)), % Может быть убрать ???
		message_new_desc(Exp),
		basic_equalities(Exp),!,
                message_basic_equalities(Exp).
             
                
        weigh(ex(_,[]),1):-!.        
        weigh(Exp,We):-depth(Exp,Depth),
                       We =Depth+1.
        
        message_new_desc(Exp):-
              expr_to_str(Exp,Str),
              %format(Msg,"Построен новый дескриптор:\n%",Str),
             msg_n(approx,45,[Str],b_false),
             !.
       message_new_desc(_).

        message_basic_equalities(Exp):-
               expr_to_str(Exp,Str),
               %format(Msg, "Введены определяющие соотношения для \n%",Str),
               msg_n(approx,46,[Str],b_false),
               !.
         message_basic_equalities(_).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55


/* Добавила 20.01.03 для множеств */
go_to_result_set([],ex("set",[])):- !.
go_to_result_set(R,ex("set",[L])):- exprlist_to_comma_expr(R,L),!.

exprlist_to_comma_expr([Ex],Ex):- !.
exprlist_to_comma_expr([Ex1,Ex3|T],ex(",",[Ex1,Ex2])):-
	exprlist_to_comma_expr([Ex3|T],Ex2),!.

comma_expr_to_order_list(H,List):- !, 
	expr_to_exprlist(H,L1), 
	calc_set_in_exprlist(L1,L3), % новое 3.02.03!!!!! либо убрать
	list_not_repeat(L3,L2),				%L1,L2
	order_listexpr(L2,[],List).

/* Добавила 3.02.03 для вычисления простых множеств в множествах с множествами */
/*новое!!!!!!!*/
calc_set_in_exprlist([],[]):- !.
calc_set_in_exprlist([H|T],[H1|T1]):- free(H1),
	H=ex("emptyset",_),!,calc(H,H1),calc_set_in_exprlist(T,T1). 
calc_set_in_exprlist([H|T],[H1|T1]):- free(H1),
	H=ex("set",_),!,calc(H,H1),calc_set_in_exprlist(T,T1). 
calc_set_in_exprlist([H|T],[ex(TypeOp,[L1,L2])|T1]):-			% либо [H1|T1]
	type(H,Type),is_subobject(Type,ex("expr_set",[])),!,
	H=ex(TypeOp,[Ex1,Ex2]),						% либо убрать
	calc_set_in_exprlist([Ex1],[L1]),calc_set_in_exprlist([Ex2],[L2]),% либо calc(H,H1)
	calc_set_in_exprlist(T,T1). 
calc_set_in_exprlist([H|T],[ex("s_skobki",[H1])|T1]):-
	H=ex("s_skobki",[Ex]),
	expr_to_exprlist(Ex,L1),!,
	calc_set_in_exprlist(L1,L2),
	exprlist_to_comma_expr(L2,H1),
	calc_set_in_exprlist(T,T1). 	 
calc_set_in_exprlist([H|T],[H|T1]):-
	calc_set_in_exprlist(T,T1). 

%упорядочивание списка термов по возрастанию
order_listexpr([H|T],Aux,L):- ins(H,Aux,Aux1),!,order_listexpr(T,Aux1,L).
order_listexpr([],L,L):- !.

ins(H,[],[H]):- !.
ins(H,[K|T],[H,K|T]):- order_expr(K,H,K,H),!.
ins(H,[K|T],[K|T1]):- ins(H,T,T1).

/*Изменила 2.02.03*/
%операции над упорядоченными списками
inters_order_list([],_,L,L):- !.
inters_order_list(_,[],L,L):- !.

inters_order_list([H|T],[H|T1],Aux,[H|Res]):- !,
	inters_order_list(T,T1,Aux,Res).

inters_order_list([H|T],[K|T1],Aux,Res):-
	order_expr(K,H,K,H),!,
	inters_order_list(T,[K|T1],Aux,Res).

inters_order_list([H|T],[K|T1],Aux,Res):-
	order_expr(H,K,H,K),!,
	inters_order_list([H|T],T1,Aux,Res).		

union_order_list([],[],L,L):- !.
union_order_list([],L,Aux,Res):- !,append(Aux,L,Res).
union_order_list(L,[],Aux,Res):- !,append(Aux,L,Res).

union_order_list([H|T],[H|T1],Aux,[H|Res]):- !,
	union_order_list(T,T1,Aux,Res).

union_order_list([H|T],[K|T1],Aux,[H|Res]):-
	order_expr(K,H,K,H),!,
	union_order_list(T,[K|T1],Aux,Res).

union_order_list([H|T],[K|T1],Aux,[K|Res]):-
	order_expr(H,K,H,K),!,
	union_order_list([H|T],T1,Aux,Res).		

differ_order_list([],_,L,L):- !.
differ_order_list(L,[],Aux,Res):- !, append(Aux,L,Res).

differ_order_list([H|T],[H|T1],Aux,Res):- !,
	differ_order_list(T,T1,Aux,Res).

differ_order_list([H|T],[K|T1],Aux,[H|Res]):-
	order_expr(K,H,K,H),!,
	differ_order_list(T,[K|T1],Aux,Res).

differ_order_list([H|T],[K|T1],Aux,Res):-
	order_expr(H,K,H,K),!,
	differ_order_list([H|T],T1,Aux,Res).	
/*конец изменения*/

/* Добавила 3.02.03 для операций над действительными числами*/
calc_deg(R1,R2,R):- R1>0.0,!,R=exp(R2*ln(R1)).
calc_deg(R1,R2,0.0):- R1=0.0,R2>0.0,!.
calc_deg(R1,R2,_):- R1=0.0,!,
	str_real(SR2,R2),
	%format(S,"Основание степени равно 0, показатель равен  %",R2),
	msg_n(diagn,47,[SR2],b_false),
	msg_n(err,48,[],b_true),
	fail.
calc_deg(R1,R2,R):- str_real(S,R2),str_int(S,Int),!,
	get_int_deg(R1,Int,0,1.0,R).
calc_deg(R1,R2,_):- !,
	str_real(SR1,R1),
	str_real(SR2,R2),
	%format(S,"Основание степени(%) отрицательно, показатель % не является целым числом",SR1,SR2),
	msg_n(diagn,49,[SR1,SR2],b_false),
	msg_n(err,48,[],b_true),
	fail.

get_int_deg(_,Int,Int,R,R):- !.	
get_int_deg(R1,Int,C,Aux,R):- Int>0,
	C<Int,!,Aux1=Aux*R1,C1=C+1,
	get_int_deg(R1,Int,C1,Aux1,R).
get_int_deg(R1,Int,C,Aux,R):- Int<0,
	C>Int,!,Aux1=Aux/R1,C1=C-1,
	get_int_deg(R1,Int,C1,Aux1,R).

calc_div(_,R2,_):- R2=0.0,!,
	msg_n(diagn,50,[],b_false),
	msg_n(err,48,[],b_true),
	fail.
	
calc_div(R1,R2,R):- !,R=R1/R2. 

 smaller(R1,R2,B):-  
        R1<R2,!, 
       calc_on_approx(ex("true_bool",[]),B).
smaller(_,_,B):-  
	calc_on_approx(ex("false_bool",[]),B).
	
bigger(R1,R2,B):-  
        R1>R2,!, 
       calc_on_approx(ex("true_bool",[]),B).
bigger(_,_,B):-  
	calc_on_approx(ex("false_bool",[]),B).


/******************************************
 calc - Вычисляет ex - выражение.
*******************************************/
/*

calc(r(R),r(R)):-!.


%  арифметические выражения 

calc(ex("r_minus1",[X]),Res):- calc_on_approx(ex("r_minus1",[X]),Res),!.
calc(ex("r_plus",[X,Y]),Res):- calc_on_approx(ex("r_plus",[X,Y]),Res),!.
calc(ex("r_minus",[X,Y]),Res):-calc_on_approx(ex("r_minus",[X,Y]),Res),!. 
calc(ex("r_times",[X,Y]),Res):- calc_on_approx(ex("r_times",[X,Y]),Res),!.
calc(ex("r_div",[X,Y]),Res):- calc_on_approx(ex("r_div",[X,Y]),Res),!.
%calc(ex("r_skobki",[X]),R):-calc_on_approx(ex("r_skobki",[X]),R),!.
calc(ex("r_mtimes",[X,Y]),Res):- calc_on_approx(ex("r_mtimes",[X,Y]),Res),!.
calc(ex("r_mdiv",[X,Y]),Res):-calc_on_approx(ex("r_mdiv",[X,Y]),Res),!.
calc(ex("r_index",[X,Y]),Res):- calc_on_approx(ex("r_index",[X,Y]),Res),!.

calc(ex("set",[]),ex("set",[])):- !.
calc(ex("set",Arg),Res):- calc_on_approx(ex("set",Arg),Res),!.
calc(ex("inters",Arg),Res):- calc_on_approx(ex("inters",Arg),Res),!.
calc(ex("union",Arg),Res):- calc_on_approx(ex("union",Arg),Res),!.
calc(ex("differ",Arg),Res):- calc_on_approx(ex("differ",Arg),Res),!.
%calc(ex("br_set",Arg),Res):- calc_on_approx(ex("br_set",Arg),Res),!.

calc(ex("not_bool",L),R):- calc_on_approx(ex("not_bool",L),R),!. 
calc(ex("equiv",L),R):- calc_on_approx(ex("equiv",L),R),!. 
calc(ex("impl",L),R):- calc_on_approx(ex("impl",L),R),!. 
calc(ex("dizj",L),R):- calc_on_approx(ex("dizj",L),R),!. 
calc(ex("conj",L),R):- calc_on_approx(ex("conj",L),R),!. 
%calc(ex("br_bool",[L]),Res):- calc_on_approx(L,Res),!.
calc(ex("noteq_bool",L),R):- calc_on_approx(ex("noteq_bool",L),R),!. 
*/
calc(ex("ifthenelse",[Bool,Ex1,Ex2]),Res1):-
	calc_on_approx(ex("ifthenelse",[Bool,Ex1,Ex2]),Res1),
	!.
calc(ex("ifthenelse",[Bool,Ex1,_]),Res1):-
	calc(Bool,L1),L1=ex("true_bool",[]),!,
	calc(Ex1,Res1).
calc(ex("ifthenelse",[Bool,_,Ex2]),Res2):-
	calc(Bool,L1),L1=ex("false_bool",[]),!,
	calc(Ex2,Res2).
/*
calc(ex("ifthenelse",[Bool,_Ex1,_]),_Res1):-!,
	expr_to_str(Bool,StrBool),
	format(Str,"Не определено условие \"%\" условного оператора.",StrBool),
	msg(err,Str),
	fail.
*/
/*конец добавления*/



%calc(_,nc("No")):-!,	 msg("\n >> Выражение не выполнено. "), fail.

/* 
%Для включения этого шаблона нужно исправить type(nc(_),"new_word") и включить template2("10",
calc(ex("new_text",[nc(Word1), nc(Word2)]),nc(Text)):-!,
calc(ex("new_text",[nc(Word1), nc(Word2)]),nc(Text)):-!,
	concat(Word1," ",Text1),
	concat(Text1,Word2,Text).
calc(ex("new_text",[nc(Word), ExText]),nc(Text)):-!,	
	calc(ExText,Res1), Res1=nc(TextRes1),
	concat(Word," ",Text1),
	concat(Text1,TextRes1,Text).
*/

calc(ex("var",[nc(N),Type]),Desc):-!,
	create_new_constant(nc(N),Type,Desc),
	retractall(descriptor(_,Desc,_)),
	idconcept(IdC),
	assert(descriptor(IdC,Desc,1601)),
	Desc=ex(Op,[]),
	retract(in_template(_,_,Id,Name,Op,_,WList,T)),!,
	expr_to_str(T,StrT),
	format(Str,"Переменная типа %",StrT),
	assert(in_template(1,IdC,Id,Name,Op,Str,WList,T)),
	make_element(Desc,Type).
	
calc(ex("var",[ex(",",[Expr1,Expr2]), Type]),ex(",",[Res1,Res2])):-!,
	calc(ex("var",[Expr1,Type]),Res1),
	calc(ex("var",[Expr2,Type]),Res2).

	
calc(ex("s_skobki",[Ex]),Res):-!, calc(Ex,Res).  %Синтаксические скобки

calc(ex(";",[Command]),Res):-!, calc(Command,Res).  
calc(ex("l_command",[L_command, Command]),Res):-!,  % список команд с разделителем ";"
								 calc(L_command,_),
								 calc(Command,Res).

calc(ex("new_in_template",[nc(Text),nc(Vars),nc(ExOb),nc(Comment),nc(Chek),nc(Exec),nc(Dostup)]),ex("true",[])):-!,
       create_in_temtplate(Text,Vars,ExOb,Comment,Chek,Exec,Dostup).
calc(ex("d_templ",[nc(Text),nc(Vars),Type]),ex("true",[])):-!,
       d_template(Text,Vars,Type).
calc(ex("replace_templ",[nc(Text),nc(Vars),Type,nc(NewText),nc(NewComment)]),ex("true",[])):-!,
       replace_template(Text,Vars,Type,NewText,NewComment).
calc(ex("new_synonym",[nc(New_Synonym),Ex]),ex("true",[])):-!,
       create_new_synonym(New_Synonym,Ex,"").
calc(ex("new_synonym_comment",[nc(New_Synonym),Ex,nc(Comment)]),ex("true",[])):-!,
       create_new_synonym(New_Synonym,Ex,Comment).
calc(ex("replace_termin",[nc(OldTermin),TypeOldTermin,nc(NewTermin),nc(Comment)]),ex("true",[])):-!,
       replace_termin(OldTermin,TypeOldTermin,NewTermin,Comment).

/*Ввести новый синоним шаблона [@Шаблон] с переменными [@Переменные] в виде [@Новый_шаблон] с комментарием [@Комментарий]*/
calc(ex("new_synonym_templ",[nc(Text),nc(Vars),Type,nc(NewText),nc(NewComment)]),ex("true",[])):-!,
       create_new_synonym_template(Text,Vars,Type,NewText,NewComment).
calc(ex("synonym",[Ex1,Ex2]),ex("true",[])):-!,
	make_synonym(Ex1,Ex2).

calc(ex("type_of",[Term]),Type):-!, type(Term,Type).
    
%calc(ex("set_dostup_in_template",[nc(TextTemplate),nc(Dostup)],ex(true,[])):-!,
%	set_dostup_in_template(TextTemplate,Dostup).

% старые
/* если включить, то не работает создание второго экз. той же онтологии 
calc(nc(X),Desc):-
	in_template(_, _,_, X,Op,_,_,Type),!,
	expr_to_str(Type, StrType),
	format(Msg, "Строка \"%\" вычислена как выражение типа %.", X, StrType),
	calc(ex(Op,[]),Desc),
	msg(calc,Msg).	
*/
calc(nc(X),nc(X)):-!.

calc(v(X,Type),v(X,Type)):-!.

calc(ex("nil_command",[]), ex("nil_command",[])):-!.

calc(ex("msg",Arglist),ex("msg",Arglist)):-!. %обработка сообщения системы

calc(ex("()",[Expr]),Res):-!,calc(Expr,Res).

calc(ex("object",[N]),Desc):-!,
	create_new_constant(N,ex("ob",[]),Desc).
	
calc(ex("create_el_concepts",[nc(NameEl),nc(ConceptName)]),Desc):-!,
	get_IdConcept(ConceptName, IdConcept),
	%is_concept(nc(ConceptName)),
	create_constant_ifnew(nc(ConceptName),ex("ob",[]),DescConcept),
	create_new_constant(nc(NameEl),DescConcept,Desc),
	 make_element(Desc,DescConcept),
	get_newIdConcept(IdRef),
	idconcept(IdCurrent),
	%predoksAnd_I(IdConcept, ListConceptId),
	%insertElConcept_inCurrent(IdConcept,ListConceptId,IdRef),
	insertElConcept_inCurrent(IdConcept,IdRef),
	not(fatal_error),
	assert(element_concept(IdCurrent, Desc, IdRef, IdConcept) ),
	 %format(Msg,"Создан объект % из онтологии  \"%\".",NameEl, ConceptName),
        msg_n(approx,51,[NameEl, ConceptName],b_false).
 
 calc(ex("ob_metod",[nc(X), ex(Opname,Arg)]),Desc):-       
	get_in_template(_, _,_, X,OpX,_,_,Type),!,
	expr_to_str(Type, StrType),
	%format(Msg, "Строка \"%\" вычислена как выражение типа %.", X, StrType),
	calc(ex(OpX,[]),ElDesc),
	msg_n(calc,52,[X,StrType],b_false),
	element_concept(_, ElDesc,RefConcept, _),!,
	get_in_template(_, _,_,  Text, Opname, _,_, _),!,
	get_in_template(_, RefConcept,_,  Text, NewOpname, _,_, _),!,
	calc(ex(NewOpname,Arg),Desc).	
calc(ex("ob_metod",[El_Concept, ex(Opname,Arg)]),Desc):- 
	!, % если убрать, то в случае неуспеха формируетя терм
	calc(El_Concept,ElDesc),
	element_concept(_, ElDesc,RefConcept, _),!,
	get_in_template(_, _,_,  Text, Opname, _,_, _),!,
	get_in_template(_, RefConcept,_,  Text, NewOpname, _,_, _),!,
	calc(ex(NewOpname,Arg),Desc).
  /* новый вариант 
  	type(El_Concept, ExConcept),
  	expr_to_str(ExConcept, NameConcept),
  	get_IdConcept(NameConcept,IdConcept),
  	in_template(_,IdConcept,_,  Text, Opname, _,_, _),!.
  */

 calc(ex("new_сrwt_rule",[nc(StrVars),nc(StrPredCond),nc(StrL),nc(StrR),nc(Msg),nc(StrPostCond)]),ex("true",[])):-!, 	
	create_rwt_rule(StrVars,StrPredCond, StrL,StrR,StrPostCond,Msg).  	
 calc(ex("new_rwt_rule",[nc(StrVars),nc(StrL),nc(StrR),nc(Msg)]),ex("true",[])):-!, 	
	create_rwt_rule(StrVars,"", StrL,StrR,"",Msg).  
  	
calc(ex("morphism",[N]),Desc):-!,
	create_new_constant(N,ex("mor",[]),Desc).

calc(ex("make_equal",[Ex1,Ex2]),ex("true",[])):- !,
          make_equal(Ex1,Ex2).
        

calc(ex("not_equal",[Ex1,Ex2]),ex("true",[])):-!, 
        make_not_equal(Ex1,Ex2).                

calc(ex("calculate",[Arg]),Res):-!,calc(Arg,Res).
calc(ex("question_end",[Arg]),Res):-!,calc(Arg,Res).
%calc(ex("question_text",[Arg]),Res):-!,calc(Arg,Res).

calc(ex("sentence_point",[X]),Y):-!, calc(X,Y). %  sentence Обработка предложений.
calc(ex("point",[]),ex(true,[])):-!. %e Обработка отдельной точки как предложения
calc(ex("text",[Sent,TextRest]),Res):-!,
			calc(Sent,_),
			calc(TextRest,Res).

calc(ex("mk_mor",[nc(F),Ob1,Ob2]),ex("true",[])):-!,
	 create_new_constant(nc(F),ex("mor",[]),DescF),
         make_morphism(Ob1, DescF,Ob2).
calc(ex("mk_mor",[F,Ob1,Ob2]),ex("true",[])):-!,
	 calc(F,DescF),
         make_morphism(Ob1, DescF,Ob2).

calc(ex("make_subobject",[Ob1,Ob2]),ex("true",[])):-!,
         %calc(ex("object",[Ob1]),ResOb1),
         make_subobject(Ob1,Ob2).

calc(ex("make_subobjects",[Ob1,Expr]),ex("true",[])):-!,
         expr_to_exprlist(Expr,Exprlist),
	 make_subobjects(Ob1,Exprlist).

calc(ex("make_element",[El,Ob]),ex("true",[])):-!,
          make_element(El,Ob).
         
calc(ex("make_elements",[Elmts,Ob]),ex("true",[])):-!,
           make_elements(Elmts,Ob).

calc(ex("is_eq_expr",[Ex1,Ex2]),ex("true_bool",[])):- /* .02.03 */
        calc(Ex1,Res1),
        calc(Ex2,Res2),
        Res1=Res2,!.                            %??????? может нужно на текущей аппроксимации
        
calc(ex("is_eq_expr",[_,_]),ex("false_bool",[])):-!. /* .02.03 */
/*
calc(ex("is_eq_ob",[Ex1,Ex2]),ex("true",[])):-
        is_equal(Ex1,Ex2),!.    				%на аппроксимации
calc(ex("is_eq_ob",[_,_]),ex("false",[])):-!.        

calc(ex("is_eq_mor",[Ex1,Ex2]),ex("true",[])):-
        is_equal(Ex1,Ex2),!.
calc(ex("is_eq_mor",[_,_]),ex("false",[])):-!.        
*/

calc(ex("is_subobject",[Ob1,Ob2]),ex("true_bool",[])):- /* .02.03 */
         is_subobject(Ob1,Ob2),!.
calc(ex("is_subobject",[_,_]),ex("false_bool",[])):-!. /* .02.03 */

calc(ex("is_element",[Ob1,Ob2]),ex("true_bool",[])):- /* .02.03 */
         is_element(Ob1,Ob2),!.
calc(ex("is_element",[_,_]),ex("false_bool",[])):-!. /* .02.03 */
calc(ex("elements_of",[Ob]),ex(X,[])):-!,
         calc(Ob,DescOb),
         findall(El,element_of_object(El,DescOb),ListEl),
         list_not_repeat(ListEl,List),
        % exprlist_to_slist(List,Slist),
        % list_to_str5(0,Slist,"\n",X).
         exprlist_to_order_str_nl(List,X).

calc(ex("max_subobjects",[Ob]),ex(X,[])):-!,
         calc(Ob,DescOb),
         findall(Sub,subobject(_,Sub,DescOb),ListSub),
         list_not_repeat(ListSub,List),
         exprlist_to_order_str_nl(List,X).

calc(ex("subobjects",[Ob]),ex(X,[])):-!,
         calc(Ob,DescOb),
         subobjects_of(DescOb,List),
         exprlist_to_order_str_nl(List,X).
calc(ex("descs",[]),ex(X,[])):-!,
         findall(Desc,descriptor(_,Desc,_),List),
         exprlist_to_order_str_nl(List,X).
calc(ex("all_inconsists",[]),ex(X,[])):-!,
         all_inconsists(X).
calc(ex("world_concept",[]),ex(X,[])):-!,
         world_concept(X).

calc(ex("loadInConcept",[nc(Concept_name)]),ex(true,[])):-!,
        get_IdConcept(Concept_name, ID),
         loadInConcept(ID).
/*        
calc(ex("parentConcept",[nc(Concept_name)]),ex(true,[])):-!,
        get_IdConcept(Concept_name, IDNewParent),
	idconcept(IdConcept),       
        %not_predok(IdNewParent,IdConcept),
         % not_predok(IdConcept,IdNewParent),
         loadInConcept(IDNewParent),
         retractall(used_concept(_,IdConcept,IdNewParent)),
         assert(used_concept(1,IdConcept,IdNewParent)).
*/

calc(ex("calc_defin",[nc(Concept_name)]),ex(msg,[])):-!,
         get_IdConcept(Concept_name, ID),
         calc_defin(ID),
         %format(Msg,"Онтология %s загружена",Concept_name),
         msg_n(did,53,[Concept_name],b_false).

calc(ex("attribute",[Ob,Attr]),ex("true",[])):-!,
         calc(ex("object",[Attr]),ResAt),
         calc(ex("disting_mor",[ResAt,Ob]),Mor),
         make_subobject(ex("attrs",[]),ex("mor",[])),
         make_element(Mor,ex("attrs",[])).         	
         
calc(ex("attributs",[Ob,Expr]),ex("true",[])):-!,
         expr_to_exprlist(Expr,Exprlist),
         attribute_list(Ob,Exprlist).
calc(ex("what_attrs",[Ob]),ex(X,[])):-!,
         what_attrs(Ob,X),!.

calc(ex("select",[Ex,Ob]),ex(Report,[])) :- !,
	select_report(Ex,Ob,Report).

%calc(ex("dic_templates",[Ex]),ex("true",[])) :- !,
	%dictionary(Ex).		

calc(ex("<",[Ex1,Ex2]),B):-
%dlg_note("SMALLER????????????"),
        calc(Ex1,Res1), Res1=r(R1),
        calc(Ex2,Res2), Res2=r(R2),
        R1<R2, 
        calc(ex("true_bool",[]),B),!
        ;
        calc(Ex1,Res1), Res1=r(R1),
        calc(Ex2,Res2), Res2=r(R2),
       not(R1<R2),
        calc(ex("false_bool",[]),B),!.

calc(ex(">",[Ex1,Ex2]),B):-
        calc(Ex1,Res1), Res1=r(R1),
        calc(Ex2,Res2), Res2=r(R2),
        R1>R2, 
        calc(ex("true_bool",[]),B),!
        ;
         calc(Ex1,Res1), Res1=r(R1),
        calc(Ex2,Res2), Res2=r(R2),
        not(R1>R2), 
        calc(ex("false_bool",[]),B),!.
  

			
/*********** Общая процедура вычисления шаблонов и категорных операций ***********/

calc(ex(Opname,Arglist),Res):-  %Opname с действием в текущей онтологии
	exec_templ(_,Opname, ListVar,Chek,Exec),!, 
	calc_list(Arglist,Reslist),!, 
	do_exec(Opname,Reslist,ListVar,Chek,Exec,Res).
calc(ex(Opname,Arglist),Res):- %Opname во внешних шаблонах
	%get_IdConceptForOp(Opname,IdConcept),
	template(IdConcept,_,_,Opname,_,_,_),
	loadInConcept(IdConcept),
	exec_templ(_,Opname, ListVar,Chek,Exec),!,
	calc_list(Arglist,Reslist),
	do_exec(Opname,Reslist,ListVar,Chek,Exec,Res).

calc(Ex,Res) :- calc_on_approx(Ex,Res),!. 
%calc(Ex,Ex):-  	descriptor(_,Ex,_), !.



calc(ex(Opname,Arglist),Res):- %Opname - имя операции без внешнего действия, но с изменением аппроксимации
	calc_list(Arglist,Reslist),
	eval(Opname,Reslist,Res),!.

calc(Term, undef):-
	expr_to_str(Term,Str),
	%dlg_Error("Не могу вычислить",Str),
	%write("\nНе могу вычислить: "), write(Term),
	msg_n(err,91, [Str], b_false),
	fail.

/*************************************************************
 calc_list - Вычисляет список выражений посредством обращения
             к calc.
**************************************************************/

calc_list([],[]):-!.
calc_list([H1|T1],[H2|T2]):-calc(H1,H2),calc_list(T1,T2).	



/**************************************************************
                 Код предиката CALC_TYPE
 *************************************************************/

/*		
calc_type_list([],[]):-!.
calc_type_list([H|T],[Htype|Ttype]):-
	calc_type(H,Htype),
	calc_type_list(T,Ttype),!.
*/

calc_type(undef,undef):-!.
calc_type(nc(X),Type):-bound(X),calc(ex("new",[]),Type),!.

calc_type(v(_,T),T):-!.

calc_type(Ex,Type):-
			calc(Ex,DescEx), !,
			type(DescEx,Type).
calc_type(_,Type):-calc(ex("term",[]),Type),!.

/*
%!!!!!!!!!! Переделать с учетом предиката   type. Предикат calc_type сначала делает calc, а потом type от результата.
calc_type(undef,undef):-!.
calc_type(nc(X),Type):-bound(X),calc(ex("new",[]),Type),!.

calc_type(v(_,T),T):-!.

calc_type(Ex,Type):-
			calc(Ex,DescEx), 
			desc_type(DescEx,Type),!.
calc_type(_,Type):-calc(ex("term",[]),Type),!.
			
desc_type(r(X),Type):-bound(X),calc(ex("real",[]),Type),!.
desc_type(Desc,Type):- element(_,Desc,Type),!.
desc_type(ex(Functor,_Arglist),Type):-
       			get_in_template(_,_,_,_,Functor,_,_List,T),
			calc_on_approx(T,Type),!.
desc_type(ex(Functor,_Arglist),Type):-
        		get_template(_,_,_,Functor,_,_List,T),
			calc_on_approx(T,Type),!.
*/	


isa_calc_type(Term,Type):-
	calc_type(Term,T1),
	T1=Type,!.
isa_calc_type(Term,Type):-
	calc_type(Term,T1),
	is_subobject(T1,Type).
/*
list_type_arg([],[]):-!.
list_type_arg([w(_)|T],Argtypelist):-!,list_type_arg(T,Argtypelist).
list_type_arg([var(Type)|T],[Type|Argtypelist]):-!,list_type_arg(T,Argtypelist).
*/

/*****************************************************************************
 Код предиката calc_on_approx
 calc_on_approx работает как calc в общей процедуре, но не меняет аппроксимации            
******************************************************************************/

calc_on_approx_list([],[]).
calc_on_approx_list([H1|T1],[H2|T2]):-calc_on_approx(H1,H2),calc_on_approx_list(T1,T2).	

/*********** Специальные случаи ***********/
/* Убрано 23.12.02
calc_on_approx(ex(IdConst,_Arg),Res):- concat("c",StrN,IdConst),str_int(StrN,_),!,
	in_template(StrN,Text,_,_,_,_),!,
	calc_on_approx(c(Text),Res).
*/
/*
calc_on_approx(Exp,_Res):-
	dlg_note("OOOOOOOOOOThis"),
	%expr_to_str(Exp,Str),
	write("\n",Exp),
	fail.
*/	
calc_on_approx(ex("()",[Expr]),Res):-!,
         calc_on_approx(Expr,Res).
calc_on_approx(ex(",",[Ex1,Ex2]),ex(",",[Res1,Res2])):-!,
         calc_on_approx(Ex1,Res1),
         calc_on_approx(Ex2,Res2).

calc_on_approx(ex("first_element",[ex(",",[Ex1,_Ex2])]),Ex1):-
	not(Ex1=ex(",",[_,_])),!.
calc_on_approx(ex("first_element",[ex(",",[Ex1,_Ex2])]),Ex):-!,
	calc_on_approx(ex("first_element",[Ex1]),Ex).
calc_on_approx(ex("first_list",[ex(",",[Ex1,_Ex2])]),Ex1):-!.
calc_on_approx(ex("first_list",[_]),ex("nil",[])):-!.
calc_on_approx(ex("second",[ex(",",[_Ex1,Ex2])]),Ex2):-!.
calc_on_approx(ex("second",[Ex]),Ex):-!.

%множества
calc_on_approx(ex("set",[]),ex("set",[])):- !.
calc_on_approx(ex("set",[H]),ex("set",[L])):- !,
	comma_expr_to_order_list(H,R),  
	exprlist_to_comma_expr(R,L).

calc_on_approx(ex("inters",[ex("set",[]),_]),ex("set",[])):- !.
calc_on_approx(ex("inters",[_,ex("set",[])]),ex("set",[])):- !.

calc_on_approx(ex("inters",[ex("set",[H]),ex("set",[K])]),Res):- !,
	comma_expr_to_order_list(H,L1),
	comma_expr_to_order_list(K,L2),
	inters_order_list(L1,L2,[],R),
	go_to_result_set(R,Res).

calc_on_approx(ex("inters",[Ex1,Ex2]),Res):- 
	calc_on_approx(Ex1,L1),L1=ex("set",_),
	calc_on_approx(Ex2,L2),L2=ex("set",_),!,
	calc_on_approx(ex("inters",[L1,L2]),Res).
	

calc_on_approx(ex("union",[ex("set",[]),ex("set",[])]),ex("set",[])):- !.
calc_on_approx(ex("union",[L,ex("set",[])]),Res):- !,calc_on_approx(L,Res).
calc_on_approx(ex("union",[ex("set",[]),L]),Res):- !,calc_on_approx(L,Res).

calc_on_approx(ex("union",[ex("set",[H]),ex("set",[K])]),ex("set",[L])):- !,
	comma_expr_to_order_list(H,L1),
	comma_expr_to_order_list(K,L2),
	union_order_list(L1,L2,[],R),
	exprlist_to_comma_expr(R,L).

calc_on_approx(ex("union",[Ex1,Ex2]),Res):- 
	calc_on_approx(Ex1,L1),L1=ex("set",_),
	calc_on_approx(Ex2,L2),L2=ex("set",_),!,
	calc_on_approx(ex("union",[L1,L2]),Res).

calc_on_approx(ex("differ",[ex("set",[]),_]),ex("set",[])):- !.
calc_on_approx(ex("differ",[L,ex("set",[])]),Res):- !,calc_on_approx(L,Res).

calc_on_approx(ex("differ",[ex("set",[H]),ex("set",[K])]),Res):- !,
	comma_expr_to_order_list(H,L1),
	comma_expr_to_order_list(K,L2),
	differ_order_list(L1,L2,[],R),
	go_to_result_set(R,Res).

calc_on_approx(ex("differ",[Ex1,Ex2]),Res):- 
	calc_on_approx(Ex1,L1),L1=ex("set",_),
	calc_on_approx(Ex2,L2),L2=ex("set",_),!,
	calc_on_approx(ex("differ",[L1,L2]),Res).
	
%calc_on_approx(ex("br_set",[L]),Res):- !,calc_on_approx(L,Res).

%числа
calc_on_approx(r(R),r(R)):-!.

calc_on_approx(v(X,T),v(X,T)):-!.


calc_on_approx(ex("r_minus1",[X]),Res):- calc_on_approx(X,Rx),Rx=r(R1),!, 

					R is -R1, 
					
					Res=r(R).
calc_on_approx(ex("r_plus",[X,Y]),Res):- calc_on_approx(X,C1),C1=r(R1), 
					calc_on_approx(Y,C2),C2=r(R2),!,

					R is R1+R2,
					
					Res=r(R).
calc_on_approx(ex("r_minus",[X,Y]),Res):- calc_on_approx(X,C1),C1=r(R1), 
					calc_on_approx(Y,C2),C2=r(R2),!,

					R is R1-R2, 
					
					Res=r(R).
calc_on_approx(ex("r_times",[X,Y]),Res):- calc_on_approx(X,C1),C1=r(R1), 
					calc_on_approx(Y,C2),C2=r(R2),!,

					R is R1*R2,
					
					Res=r(R).
calc_on_approx(ex("r_div",[X,Y]),Res):-
					 calc_on_approx(X,C1),C1=r(R1), 
					calc_on_approx(Y,C2),C2=r(R2),!,
					calc_div(R1,R2,R),Res=r(R).
calc_on_approx(ex("r_skobki",[X]),R):- calc_on_approx(X,R),!.
calc_on_approx(ex("r_mtimes",[X,Y]),Res):- calc_on_approx(X,C1),C1=r(R1), 
					calc_on_approx(Y,C2),C2=r(R2),!,

					R is R1*R2,
					
					Res=r(R).
calc_on_approx(ex("r_mdiv",[X,Y]),Res):- calc_on_approx(X,C1),C1=r(R1), 
					calc_on_approx(Y,C2),C2=r(R2),!,
					calc_div(R1,R2,R),Res=r(R).
calc_on_approx(ex("r_index",[X,Y]),Res):- calc_on_approx(X,C1),C1=r(R1), 
					calc_on_approx(Y,C2),C2=r(R2),!,
					 calc_deg(R1,R2,R),Res=r(R).


%boolean
calc_on_approx(ex("true_bool",[]),ex("true_bool",[])):- !. %?
calc_on_approx(ex("false_bool",[]),ex("false_bool",[])):- !. %?

calc_on_approx(ex("not_bool",[Ex]),ex("true_bool",[])):- 
	calc_on_approx(Ex,L1),L1=ex("false_bool",[]),!.
calc_on_approx(ex("not_bool",[Ex]),ex("false_bool",[])):- 
	calc_on_approx(Ex,L1),L1=ex("true_bool",[]),!.

calc_on_approx(ex("equiv",[Ex1,Ex2]),ex("true_bool",[])):-
	calc_on_approx(Ex1,L1),L1=ex("true_bool",[]),
	calc_on_approx(Ex2,L2),L2=L1,!.
calc_on_approx(ex("equiv",[Ex1,Ex2]),ex("true_bool",[])):-
	calc_on_approx(Ex1,L1),L1=ex("false_bool",[]),
	calc_on_approx(Ex2,L2),L2=L1,!.
calc_on_approx(ex("equiv",[Ex1,Ex2]),ex("false_bool",[])):-
	calc_on_approx(Ex1,L1),L1=ex("true_bool",[]),
	calc_on_approx(Ex2,L2),L2=ex("false_bool",[]),!.
calc_on_approx(ex("equiv",[Ex1,Ex2]),ex("false_bool",[])):-
	calc_on_approx(Ex1,L1),L1=ex("false_bool",[]),
	calc_on_approx(Ex2,L2),L2=ex("true_bool",[]),!.

calc_on_approx(ex("impl",[Ex1,Ex2]),ex("true_bool",[])):-
	calc_on_approx(Ex1,L1),L1=ex("true_bool",[]),
	calc_on_approx(Ex2,L2),L2=ex("true_bool",[]),!.
calc_on_approx(ex("impl",[Ex1,_]),ex("true_bool",[])):-
	calc_on_approx(Ex1,L1),L1=ex("false_bool",[]),!.
calc_on_approx(ex("impl",[Ex1,_]),ex("false_bool",[])):- 
	calc_on_approx(Ex1,L1),L1=ex("true_bool",[]),!.

calc_on_approx(ex("dizj",[Ex1,_]),ex("true_bool",[])):-
	calc_on_approx(Ex1,L1),L1=ex("true_bool",[]),!.
calc_on_approx(ex("dizj",[_,Ex2]),ex("true_bool",[])):-
	calc_on_approx(Ex2,L2),L2=ex("true_bool",[]),!.
calc_on_approx(ex("dizj",[Ex1,Ex2]),ex("false_bool",[])):- 
	calc_on_approx(Ex1,L1),L1=ex("false_bool",[]),
	calc_on_approx(Ex2,L2),L2=ex("false_bool",[]),!.

calc_on_approx(ex("conj",[Ex1,Ex2]),ex("true_bool",[])):-
	calc_on_approx(Ex1,L1),L1=ex("true_bool",[]),
	calc_on_approx(Ex2,L2),L2=ex("true_bool",[]),!.
calc_on_approx(ex("conj",[Ex1,_]),ex("false_bool",[])):- 
	calc_on_approx(Ex1,L1),L1=ex("false_bool",[]),!.
calc_on_approx(ex("conj",[_,Ex2]),ex("false_bool",[])):- 
	calc_on_approx(Ex2,L2),L2=ex("false_bool",[]),!.

%calc_on_approx(ex("br_bool",[L]),Res):- calc_on_approx(L,Res),!.

calc_on_approx(ex("noteq_bool",[Ex1,Ex2]),ex("false_bool",[])):-
	calc_on_approx(Ex1,Res1),
	calc_on_approx(Ex2,Res2),Res1=Res2,!.
calc_on_approx(ex("noteq_bool",[Ex1,Ex2]),ex("true_bool",[])):- 
	calc_on_approx(Ex1,Res1),
	calc_on_approx(Ex2,Res2),
	not(Res1=Res2),!.

calc_on_approx(ex("ifthenelse",[Bool,Ex1,_]),Res1):-
	calc_on_approx(Bool,L1),L1=ex("true_bool",[]),!,
	calc_on_approx(Ex1,Res1).
calc_on_approx(ex("ifthenelse",[Bool,_,Ex2]),Res2):-
	calc_on_approx(Bool,L1),L1=ex("false_bool",[]),!,
	calc_on_approx(Ex2,Res2).
calc_on_approx(ex("ifthenelse",[Bool,_Ex1,_]),_Res1):-!,
	expr_to_str(Bool,StrBool),
	%format(Str,"Не определено условие \"%\" условного оператора.",StrBool),
	msg_n(err,54,[StrBool],b_false),
	fail.

calc_on_approx(ex("is_eq_expr",[Ex1,Ex2]),ex("true_bool",[])):- /* .02.03 */
        calc_on_approx(Ex1,Res1),
        calc_on_approx(Ex2,Res2),
        Res1=Res2,!.                            
        
calc_on_approx(ex("is_eq_expr",[_,_]),ex("false_bool",[])):-!. /* .02.03 */

calc_on_approx(ex("<",[Ex1,Ex2]),B):-
        calc_on_approx(Ex1,Res1), Res1=r(R1),
        calc_on_approx(Ex2,Res2), Res2=r(R2),
        smaller(R1,R2,B),!.  
            
calc_on_approx(ex(">",[Ex1,Ex2]),B):-
        calc_on_approx(Ex1,Res1), Res1=r(R1),
        calc_on_approx(Ex2,Res2), Res2=r(R2),
        bigger(R1,R2,B), !.
        
        
/********** Общая процедура **********/	
/*
calc_on_approx(ex(Opname,Arglist),Res):-
	%template(IdConcept,_,_,Opname,_,_,_),
	%loadInConcept(IdConcept),
	exec_templ(_,Opname, ListVar,Chek,Exec),!,
	calc_on_approx_list(Arglist,Reslist),
	do_exec(Opname,Reslist,ListVar,Chek,Exec,Res).
*/

calc_on_approx(ex(Opname,Arglist),Res):-
	%write("Calc ", Opname, ":Calc "),
	%write("Arglist: ",Arglist,":Arglist"),
	calc_on_approx_list(Arglist,Desclist),
	%write("Desclist: ",Desclist,":Desclist"),
	cat_eval_approx(ex(Opname,Desclist),Res),!.

/*
calc_on_approx_or_reduce(Exp,Res):-
	calc_on_approx(Exp,Res),!.
calc_on_approx_or_reduce(Exp,Res):-!,
	reduce(0,Exp,Res),
	not(Exp=Res),!.
	%calc_on_approx(Redex,Res),
	%not(Exp=Res),!.
*/


%calc_on_approx(Ex,Ex):-!.

cat_eval_approx(Exp,Exp):-
	        descriptor(_,Exp,_),!.
cat_eval_approx(Exp,Desc):-
		resalt_op(_,Exp,Desc),!.
	
/********************* Begin eval ********************/		
%PREDICATES
%gen_term_num(integer)
%nondeterm gen_incr_ints_c(integer,integer)
%listStr_listWord(slist,list_w_v_t)


%  eval нужно переделать;  больщую часть перенести в calc и разобраться с ex(",",Arg)
eval(",",Arglist,ex(",",Arglist)):-!.
%eval(",new"Arglist,ex(",new",Arglist)):-!.

eval("objects",[ex(",",[Expr1,Expr2])],ex("true",[])):-!,
	eval("objects",[Expr1],_),
	eval("objects",[Expr2],_).
eval("objects",[nc(N)],Res):-!,calc(ex("object",[nc(N)]),Res).
/*
eval("objects",[ex(",new",[Expr1,Expr2])],ex("true",[])):-!,
	eval("objects",[Expr1],_),
	eval("objects",[Expr2],_).
eval("objects",[nc(N)],Res):-!,calc(ex("object",[nc(N)]),Res).
*/

eval("morphisms",[ex(",",[Expr1,Expr2])],ex("true",[])):-!,
	eval("morphisms",[Expr1],_Res1),
	eval("morphisms",[Expr2],_Res2).	
eval("morphisms",[nc(N)],Res):-!, calc(ex("morphism",[nc(N)]),Res).

/*
eval("morphisms",[ex(",new",[Expr1,Expr2])],ex("true",[])):-!,
	eval("morphisms",[Expr1],_Res1),
	eval("morphisms",[Expr2],_Res2).	
eval("morphisms",[nc(N)],Res):-!, calc(ex("morphism",[nc(N)]),Res).
*/

/*********** Общая процедура ***********/

eval(Opname,Arglist,ResExpr):-cat_eval(ex(Opname,Arglist),ResExpr).



/***************** Вспомогательные предикаты *************************** */

gen_term_num(N):-
				random(100000000,N),
				str_int(IdN,N),
				not(in_template(_,_,IdN,_,_,_,_,_)),!.
gen_term_num(N):-gen_term_num(N).
 
 listStr_listWord([],[]):-!.
 listStr_listWord([W|T],[w(W)|WList]):-listStr_listWord(T,WList).
 /**********************End eval*****************************************/		
		

/**********************  Recalc  *************************/
get_IdConcept(NameLower,ID):-
	name_file(Name,Fname,_,_),
	upper_lower(Name, NameLower),
	str_int(Fname,ID),!.
get_IdConcept(Name,_):-
	%format(Str,"Онтологии \"%\" нет в словаре",Name),
	%dlg_note(Str),
	msg_n(err,55,[Name],b_true),
	fail.

	
 calc_defin(ID):-
 	used_concept(_,_,ID),!.
 calc_defin(ID):-
          take_defin(ID,Defin),
          retractall(state_interpret_well),
          assert(state_interpret_well),
          recalc(Defin),
          idconcept(IdC),
          assert(used_concept(0,IdC,ID)).

get_IdConceptForOp(Opname,IdConcept):-
	  get_template(IdConcept,_,_,Opname,_,_,_),
	  not(IdConcept=1),
	  idconcept(IdTnotion),
	  not(IdConcept=IdTnotion),!.
  
take_defin(ID_concept,Defin):-
	  str_int(Fname,ID_concept),
           name_file(_Name_concept,Fname,_,Defin),!.
 take_defin(ID_concept,_Defin):-
           str_int(Fname,ID_concept),
           %format(Str,"Онтология \"%\" недоступна.", Fname),
           msg_n(err,56,[Fname],b_false),
           fail.

   
 recalc(Defin):-not(fronttoken(Defin,_,_)),!.
 recalc(Defin):-
          impose(Defin,Sentence,RestText,Expr,_Type),	% Выделение первого предложения
          msg(gr_an,Sentence),
          calc(Expr,_Result),	% Вычисление (выполнение) выражения (команды)
          %answer(Expr_type,Sentence,Result),
           recalc(RestText),!.
        