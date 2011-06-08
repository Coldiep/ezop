


:- module(make_eq, [
create_new_constant/3, %(expr Name,expr Ob,expr Resalt)   -(i,i,o) %(i,i,r(o))         %создает новый элемент в области Ob
replace_in_list/4, %(expr_list,expr_list Res,expr D1,expr D2)  -(i,o,i,i)   /*  replace_in_list(Desclist,RDesclist,D1,D2) - заменяет в expr_list   D1 ->D2 */
reduce/3, %(integer,expr,expr) - determ (i,i,o)
make_not_equal/2, %(expr,expr) - (i,i)
order_expr/4, %(expr,expr,expr,expr) - (i,i,i,i) (i,i,o,o)
make_element/2, %(expr,expr) - (i,i)
is_equal/2, %(expr,expr) - (i,i)    %на аппроксимации;  используется также  в cateval, basiq_equalities
make_equal/2, %(expr,expr) - (i,i)
is_subobject/2, %(expr,expr) - determ (i,i)
make_subobject/2, %(expr,expr) - (i,i)
make_inclusion/3, %(expr,expr,expr) - (i,i,i)
is_equal_not_message/2, %(expr,expr) - (i,i)
isomor_with_point/1, %(expr) - (i)
between/3, %(expr,expr,expr) - (i,i,o)
is_element/2, %(expr,expr) - (i,i)
element_of_object/2 %(expr,expr) - (o,i)
]).
:- style_check(+string).
:- use_module(vp52_compat).
:- use_module(editor).
:- use_module(calc).
:- use_module(impose).
:- use_module(dbctrl).
:- use_module(db).


%include "editor.pre"


 
 

append([],L,L):-!.
append([H|T1],L,[H|T2]):-append(T1,L,T2).

 
member(H,[H|_]):-!.
member(H,[_|T]):-
	member(H,T).
 
 
   /*         подстановки понятия КАТЕГОРИЯ      rul */
            
       rewrite("скобки",ex("()",[Ex]),Ex):-!.
   
   
    rewrite("поглащение справа",
    			ex("intersect",[Ob1,Ob2]),
    			Ob1):-
    			is_subobject(Ob1,Ob2),!.
    rewrite("поглащение слева",
    			ex("intersect",[Ob1,Ob2]),
    			Ob2):-
    			is_subobject(Ob2,Ob1),!.
       			
     rewrite("коммутативность пересечения",
      			ex("intersect",[Ob1,Ob2]),
      			ex("intersect",[Min,Max])) :-
				calc_on_approx(Ob1,Desc1),calc_on_approx(Ob2,Desc2),
				order_desc(Desc1,Desc2,Max,Min),Desc1=Max,!.			
    rewrite("ассоциативность пересечения",
      			ex("intersect",[ex("intersect",[Ob1,Ob2]),Ob3]),
      			ex("intersect",[Ob1,ex("intersect",[Ob2,Ob3])])) :-!.
	rewrite("упорядочение трех пересечений",
      			ex("intersect",[Ob1,ex("intersect",[Ob2,Ob3])]),
      			ex("intersect",[Min,ex("intersect",[Max,Ob3])])) :-
				calc_on_approx(Ob1,Desc1),calc_on_approx(Ob2,Desc2),
				order_desc(Desc1,Desc2,Max,Min), Desc1=Max,!.
		

      
       rewrite("область определения тождествнного",
               ex(dom,[ex(id,[T])]),
               T):- !.
       rewrite("область значений тождественного",
               ex(cod,[ex(id,[T])]),
               T):- !.
                  
       rewrite("область определения композиции",
               ex(dom,[ex(com,[_,F2])]),
               ex(dom,[F2])):- !.
       rewrite("область значений композиции",
               ex(cod,[ex(com,[F1,_])]),
               ex(cod,[F1])):- !.             
               
       rewrite("композиция с тождественным справа",
               ex(com,[ex(id,[_]),F]),
               F):- !.
       rewrite("композиция с тождественным слева",
               ex(com,[F,ex(id,[_])]),
               F):- !.
               
       rewrite("ассоциативность композиции",
               ex(com,[ex(com,[F1,F2]),F3]),
               ex(com,[F1,ex(com,[F2,F3])])):- !.
               
               
     /*  подстановки  КАТЕГОРИИ С ФИНАЛЬНЫМ  ОБЪЕКТОМ  */
     
       rewrite("область определения отображения в точку",
                ex(dom,[ex(fin,[T])]),
                T):- !.
       rewrite("область значений отображения в точку",
                ex(cod,[ex(fin,[_])]),
                ex(final,[])):- !.
       rewrite("единственность отображения в точку",
                Exmor,
                ex(fin,[ex(dom,[Exmor])])):-          
                               calc_type(Exmor,T),
                               calc(ex("mor",[]),Mor),
                               T=Mor,
                               not(Exmor = ex(fin,[_])),
                               eq_known(ex(cod,[Exmor]),ex(final,[])),
                               !.
                


     /*  подстановки  КАТЕГОРИИ С ИНИЦИАЛЬНЫМ  ОБЪЕКТОМ  */
     
       rewrite("область значений отображения из пусто",
                ex(cod,[ex(init,[T])]),
                T):- !.
       rewrite("область определения отображения из пусто",
                ex(dom,[ex(init,[_])]),
                ex(initial,[])):- !.
       rewrite("единственность отображения из пустого",
                Exmor,
                ex(init,[ex(cod,[Exmor])])):-          
                               calc_type(Exmor,T),
                               calc(ex("mor",[]),Mor),
                               T=Mor,
                               not(Exmor = ex(init,[_])),
                               not(Exmor = ex(fin,[ex(initial,[])])),
                               eq_known(ex(dom,[Exmor]),ex(initial,[])),
                               !. 



     /*  подстановки КАТЕГОРИИ С ПРОИЗВЕДЕНИЕМ      */
     
       rewrite("область определения проекции 1",
               ex(dom,[ex(proj1,[X,Y])]),
               ex(prod_ob,[X,Y])):- !.
       rewrite("область значений проекции 1",
               ex(cod,[ex(proj1,[X,_])]),
               X):- !.
               
       rewrite("область определения проекции 2",
               ex(dom,[ex(proj2,[X,Y])]),
               ex(prod_ob,[X,Y])):- !.
       rewrite("область значений проекции 2",
               ex(cod,[ex(proj2,[_,Y])]),
               Y):- !.
               
      rewrite("композиция произведения отображений с проекцией 1",
              ex(com,[ex(proj1,[X,Y]),ex(prod_mor,[F,G])]),
              F) :-
                    eq_known(X,ex(cod,[F])),        
                    eq_known(Y,ex(cod,[G])),
                    !.
      rewrite("композиция произведения отображений с проекцией 2",
              ex(com,[ex(proj2,[X,Y]),ex(prod_mor,[F,G])]),
              G) :-
                    eq_known(X,ex(cod,[F])),        
                    eq_known(Y,ex(cod,[G])),
                    !.  

                          
     rewrite("произведение проекций декартова произведения",
              ex(prod_mor,
                          [ex(com,
                                  [ex(proj1,[X,Y]),
                                  F]),
                           ex(com,
                                  [ex(proj2,[X,Y]),
                                  F])]),
              F) :-
                    eq_known(ex(prod_ob,[X,Y]),ex(cod,[F])),
                    !.
   
    rewrite(Msg,Ex,Res):-
        rwt_rule(_,_,Msg,_Vars,Lterm,Rterm,PreCond,PostCond),  %возврат к этому месту
    	unificate(Ex,Lterm,[], Pairs_Var_Term), 
  	pairlist_to_lists(Pairs_Var_Term, VarList, TermList),
  	replace_var_on_ex(TermList, VarList, PreCond, PreCondTerm),
  	calc(PreCondTerm,PreCondDesc),
  	calc_on_approx(ex("true_bool",[]), DescTrue),
  	PreCondDesc=DescTrue,
  	replace_var_on_ex(TermList, VarList, Rterm, Res),
  	replace_var_on_ex(TermList, VarList, PostCond, PostCondTerm),
  	calc(PostCondTerm,PostCondDesc),
  	PostCondDesc=DescTrue,!.
  
  
  
  unificate(Term,v(X,TypeX),AuxList,AuxList):-
  	member(v_t(v(X,TypeX),Term), AuxList),!,
  	type(Term,TypeTerm),
  	is_subobject(TypeTerm,TypeX).
  unificate(Term,v(X,TypeX),AuxList, [v_t(v(X,TypeX),Term)|AuxList]):-
  	not(member_var(v(X,TypeX), AuxList)),!,
  	type(Term,TypeTerm),
  	is_subobject(TypeTerm,TypeX).
  unificate(r(R),r(R),AuxList,AuxList):-!.
  unificate(nc(STR),nc(STR),AuxList,AuxList):-!.
  unificate(ex(Op,ArgList),ex(Op,ArgListRule),AuxList,ResList):-
  	unificate_list(ArgList,ArgListRule, AuxList, ResList).
  	
  unificate_list([],[], ResList, ResList):-!.
  unificate_list([H|T],[HRule|TRule], AuxList, ResList):-
  	unificate(H,HRule, AuxList, ResList1),
	unificate_list(T,TRule, ResList1, ResList).
 
 member_var(Var, [v_t(Var,_)|_]):-!.
 member_var(Var, [_|T]):-
	 member_var(Var, T).
 
 
 
  pairlist_to_lists([], [], []):-!.
  pairlist_to_lists([v_t(Var,Term)|T], [Var|TVarList],[Term|TTermList]):-
  	pairlist_to_lists(T, TVarList, TTermList).
  
  
  
 % dlg_note_ex(Ex):-expr_to_str(Ex,Str),dlg_note(Str).   %Используется для отладки !!!!!  можно выкинуть
  
  any_two_elements(Ob,ex(X,[]),ex(Y,[])):-!,bound(Ob),
             element_of_object(X1,Ob), X1=ex(X,[]),
             element_of_object(Y1,Ob),Y1=ex(Y,[]),
             not(X=Y),!.
  
  /* аргументы предикатов make_subobject_desc, is_subobject_desc, subobject
                 должны быть дескрипторами */
   
  /*    is_subobject работает, если отношение subobject */
  /*    не содержит равных пар и не имеет циклов        */                
   
is_subobject_desc(D,D):-!. /* если A,B - равные области */
             %D1=D2,!.
is_subobject_desc(ex("initial",[]),_):-!.   /* пусто содержится во всех */
/*             
is_subobject_desc(D1,_):-   
             calc_on_approx(ex("initial",[]),Desc),
             D1=Desc,!.
*/
is_subobject_desc(Desc1,Desc2):-
            subobject(_,Desc1,Desc2),!. /*Desc1 непосредственный подобъект Desc2*/
     %!!!!!!! Переделать с учетом циклов, как в road
is_subobject_desc(Desc1,Desc2):-
            subobject(_,Desc1,D), 
            is_subobject_desc(D,Desc2),!.
       
is_subobject(A,B):-
	is_equal(B,ex("term",[])),
	type(A,TypeA),
	is_subobject(TypeA,ex("ob",[])),!.
is_subobject(A,B):-
            calc_on_approx(A,Desc1),
            calc_on_approx(B,Desc2),
            is_subobject_desc(Desc1,Desc2),!. 
            
make_subobject(nc(Name),B):-!,
	  create_new_constant(nc(Name),ex("ob",[]),Res),
	  make_subobject(Res,B).
make_subobject(A,B):-  
             calc(A,Desc1),
             calc(B,Desc2),
            isa_calc_type(Desc1,ex("ob",[])),
            isa_calc_type(Desc2,ex("ob",[])),
            make_subobject_desc(Desc1,Desc2),!.
make_subobject(A,B):-
            not(isa_calc_type(A,ex("ob",[]))),!,
            calc_type(A,TypeA),
            retractall(fatal_error),
    	    assert(fatal_error),
    	    expr_to_str(TypeA,StrTypeA),
    	    expr_to_str(A,StrA),
    	    expr_to_str(B,StrB), 
    	  %  expr_to_str(ex("ob",[]),StrOb), 
    	   %format(Prompt,"Не могу сделать % подобластью %, так как %  имеет тип % и не является областью.",StrA,StrB,StrA,StrTypeA),
    	   msg_n(err,22,[StrA,StrB,StrA,StrTypeA],b_false),
    	   fail.
make_subobject(A,B):-
            not(isa_calc_type(B,ex("ob",[]))),!,
            retractall(fatal_error),
    	    assert(fatal_error),
    	    expr_to_str(A,StrA),
    	    expr_to_str(B,StrB), 
    	    expr_to_str(ex("ob",[]),StrOb), 
    	   %format(Prompt,"Не могу сделать % подобластью %, так как % не имеет тип %.",StrA,StrB,StrB,StrOb),
    	   msg_n(err,23,[StrA,StrB,StrB,StrOb],b_false),
    	   fail.
make_subobject(A,B):-
	    retractall(fatal_error),
    	    assert(fatal_error),
    	   expr_to_str(A,StrA),
    	   expr_to_str(B,StrB), 
    	   %format(Prompt,"Не могу сделать % подобластью %.",StrA,StrB),
    	   msg_n(err,24,[StrA,StrB],b_false),
    	   fail.
            
make_subobject_desc(A,B):-  /* уже подобласть */
             is_subobject_desc(A,B),!.
%Begin  Случаи, когда A нельзя сделать подобластью B.           
 make_subobject_desc(A,B):-    
     	    not_subobject(_,A,B),!,
	   expr_to_str(A,StrA),
    	   expr_to_str(B,StrB), 
    	   %format(Prompt,"Известно, что % не может быть подобластью области %.",StrA,StrB),
    	   msg_n(warning,25,[StrA,StrB],b_false),
    	   fail.
make_subobject_desc(A,B):-  %  В аппроксимации  у A есть подобласть, которая не может содержаться в B.
      	getSubob_NotSubob_OnApprox(A,B, Subob),!,   
    	expr_to_str(A,StrA),
    	expr_to_str(B,StrB), 
    	expr_to_str(Subob,StrSubob), 
    	%format(Prompt,"% подобласть % и не может быть подобластью области %.",StrSubob,StrA,StrB),
    	msg_n(warning,26,[StrSubob,StrA,StrB],b_false),
    	fail.
make_subobject_desc(A,B):-  %  В аппроксимации есть элемент у A, который не может принадлежать B.
      	getElement_NotIn_OnApprox(A,B, El),!,   
    	expr_to_str(A,StrA),
    	expr_to_str(B,StrB), 
    	expr_to_str(El,StrEl), 
    	%format(Prompt,"Элемент  % принадлежит % и не может принадлежать области %.",StrEl,StrA,StrB),
    	msg_n(warning,27,[StrEl,StrA,StrB],b_false),
    	fail.
%End  Случаи, когда Aнельзя сделать подобластью.
    
make_subobject_desc(A,B):-   /*  B - подобласть A. Следовательно A и B равны. */
             is_subobject(B,A),!,
             make_equal(A,B).     
make_subobject_desc(A,B):-  /*  в аппроксимации есть пересечение A и B. */
            calc_on_approx(ex("intersect",[A,B]),Dintersekt),!,
             make_equal(A,Dintersekt).
make_subobject_desc(Desc1,Desc2):-  /* основной случай */
            idconcept(Id),
            assert(subobject(Id,Desc1,Desc2)),
             expr_to_str(Desc1,StrDesc1),
             expr_to_str(Desc2,StrDesc2),
            %format(Prompt, "Область % стала  подобластью области %.", StrDesc1, StrDesc2),
            msg_n(approx,39,[StrDesc1, StrDesc2],b_false),
             modi_subobject,
             !.
             
     modi_subobject:-
            subobject(_,Desc1,Desc2),
            idconcept(Id),
            retractall(subobject(Id,Desc1,Desc2)),
            not(is_subobject_desc(Desc1,Desc2)),
            idconcept(Id),
            asserta(subobject(Id,Desc1,Desc2)),
            fail.
     modi_subobject.
        
     make_inclusion(A,J,B):-
           make_subobject(A,B),
           calc(ex("inclusion",[A,B]),I),
           make_equal(J,I). 

getSubob_NotSubob_OnApprox(A,B, Subob):-
             not_subobject(_,Subob,B),
             is_subobject_desc(Subob,A),!.
             
getElement_NotIn_OnApprox(A,B, El):-
	not_element(_,El, B),
	is_element(El,A),!.
/***************     Программа make_elements, is_element   ***************/
/* в программе используется предикат БД  element(DescElement,DescObject),*/
/* означающий непосредственую принадлежность (без промежуточных областей),*/
/* и not_element(DescElement,DescObject), запрещающий быть элементом области .*/
/*************************************************************************/
               
  %согласование типов Type и Ob         
accord(Type,Ob,Type):-is_subobject(Ob,Type),!.
accord(Type,Ob,SupOb):-
		subobject(_,Type,X),
		accord(Ob,X,SupOb),!.
		  
ask_do_element(DescEl,TypeEl, DescB):-
	    expr_to_str(TypeEl,StTEl),
            expr_to_str(DescEl,StEl),
            expr_to_str(DescB,StB),
            format(Prompt1,"Выражение \"%\"  типа  % делается элементом области %.",StEl,StTEl,StB),
	    msg_n(warning,28,[StEl,StTEl,StB],b_false),
	    dlg_ask_do_element("on"),!,
	    msg_text(28,Prompt1),!,
	    format(Prompt,"%<BR>Продолжать?",Prompt1),
	    dlgAsk_call(Prompt, Bool),
	    Bool = "true".
	    %resp_default= dlg_Ask(Prompt,["&Да","&Нет"]).
ask_do_element(_,_,_).
  		 
%create_new_constant(Name,Ob)             создает новый элемент в области Ob
create_new_constant(El,B,_Res):-
	    not(isa_calc_type(B,ex("ob",[]))),!,
	    calc_type(B,Type),
	    retractall(fatal_error),
    	    assert(fatal_error),
    	   expr_to_str(Type,StrType),
    	   expr_to_str(B,StrB), 
    	   expr_to_str(El,StrEl),
    	   %format(Prompt,"Не могу сделать % элементом терма \"%\" типа %, \nтак как % не область.",StrEl,StrB,StrType,StrB),
    	   msg_n(err,29,[StrEl,StrB,StrType,StrB],b_false),
    	   fail.
create_new_constant(El,B,Res):- % El уже построен в области B.
	  not(El=nc(_)),
	  isa_calc_type(El,B),!,
	  calc(El,Res).
create_new_constant(nc(Name),_Ob,_Res):-
        not(fronttoken(Name,_,_)),!,
        %msg(err,"Имя элемента не может быть пустым."),
        msg_n(err,30,[],b_false),
        fail.
create_new_constant(nc(Name),Ob,_Res):-
         impose_all_not_message(Name,Term,Type),
         accord(Type,Ob,SupOb),  %согласование типов Type и Ob
         bound(Term),!,
	 expr_to_str(Ob,StrOb),
	 expr_to_str(SupOb,StrSupOb),
         %format(Str, "Выражение \"%\" уже есть в области \"%\", которая содержит область \"%\".",Name,StrSupOb,StrOb),
          msg_n(err,31,[Name,StrSupOb,StrOb],b_false),
          fail.
create_new_constant(nc(Name),Type,ex(Op,[])):-!,
	calc(Type,T),
	upper_lower(Name, LowerName),
	scan(LowerName,List),
	listStr_listWord(List,WList),
	idconcept(IdC),
	str_int(SIdC,IdC),
	gen_term_num(IdN),
	str_int(Id,IdN),
	concat("c",SIdC,S1),
	concat(S1,".",S2),
	concat(S2,Id,Op),
	expr_to_str(T,StrT),
	format(Str,"Элемент типа %",StrT),
	assert(in_template(1,IdC,Id,Name,Op,Str,WList,T)),
	assert(descriptor(IdC,ex(Op,[]),1)).

	
           
make_element(El,B):-is_element(El,B),!. % Уже элемент.
make_element(El,B):-
	    not(isa_calc_type(B,ex("ob",[]))),!,
	    calc_type(B,Type),
	    retractall(fatal_error),
    	    assert(fatal_error),
    	     expr_to_str(Type,StrType),
    	   expr_to_str(El,StrEl),
    	   expr_to_str(B,StrB), 
    	   %format(Prompt,"Не могу сделать \"%\" элементом терма \"%\" типа %, \nтак как % не область.",StrEl,StrB,StrType,StrB),
    	   msg_n(err,32,[StrEl,StrB,StrType,StrB],b_false),
    	   fail.
 make_element(Elmt,Ob):-     /* Ob пуста. Невозможно сделать */
         is_equal(Ob,ex("initial",[])),!,
         retractall(fatal_error),
    	 assert(fatal_error),
        expr_to_str(Elmt,St1),
        expr_to_str(Ob,St2),
         %format(Prompt,"Попытка сделать % элементом пустой области \"%\".",St1,St2),
         msg_n(err,33,[St1,St2],b_false),
         fail.
make_element(nc(Name),Ob):-!,    %новый элемент.
	create_new_constant(nc(Name),Ob,Res),
	calc(Ob,DescOb),
	make_element_desc(Res,DescOb).
make_element(El,B):-   %Область B - подобласть области типа элемента El. Делать!
            calc(El,DescEl),
            calc(B,DescB),
            calc_type(DescEl,TypeEl),
            is_subobject(DescB, TypeEl),
            make_element_desc(DescEl,DescB),!.
make_element(El,B):- %Сомнительный случай. По умолчанию пытается выполнить.
            calc(El,DescEl),
            calc(B,DescB),
            calc_type(DescEl,TypeEl),
            /****
            ask_do_element(DescEl,TypeEl,DescB),
            ****/
            calc(ex("intersect",[TypeEl,DescB]),B1),
            make_element(DescEl,B1),!.
%Здесь могут быть случаи, когда делать не надо. Например, ограничения по мощности. 
%В этом случае: отсечение не ставить, писать предупреждения, заканчивать fail. 
make_element(El,B):-
	    retractall(fatal_error),
    	    assert(fatal_error),
    	   expr_to_str(El,StrEl),
    	   expr_to_str(B,StrB), 
    	   %format(Prompt,"Не могу сделать % элементом области %.",StrEl,StrB),
    	   msg_n(err,34,[StrEl,StrB],b_false),
    	   fail.
    	   
    	   
make_element_desc(DescEl,DescOb):-  %Добавление элемента к области. Элемент может быть в нескольких областях. 
           idconcept(Id),
           assert(element(Id,DescEl,DescOb)),!,
           expr_to_str(DescEl,StrDescEl),
           expr_to_str(DescOb,StrDescOb),
           %format(Prompt, "% стал элементом области %.", StrDescEl, StrDescOb),
           msg_n(approx,35,[StrDescEl, StrDescOb],b_false),
           clear_in_bigger(DescEl,DescOb).
 
 clear_in_bigger(El,Ob):-
           subobject(_,Ob,BigOb),
           retractall(element(_,El,BigOb)),
           fail.
 clear_in_bigger(_,_).

/***** isomor_with_point(Ob) должен проверять существование изоморфизма с
       точкой. Требуется доделка      *********/
     isomor_with_point(Ob):- /* не все случаи рассмотрены */
           is_equal_not_message(Ob,ex("final",[])),!.
     isomor_with_point(Ob):-
            calc(Ob,Desc),
            element(_,Desc,_),!.
     isomor_with_point(ex(image,[_,Ob])):-
            isomor_with_point(Ob),!.
             
                   
    is_element(Elmt,Ob):-
           calc(Elmt,DescEl),
           calc(Ob,DescOb),
           is_element_desc(DescEl,DescOb),!.
   is_element(Elmt,Ob):-
   	isa_calc_type(Elmt,Ob).

     is_element_desc(DescEl,DescOb):-
           immed_element(DescEl,DescOb),!.        
     is_element_desc(DescEl,Ob):-
         % immed_element(DescEl,Ob1),
         %  member(Ob1,ListOb).
           subobjects_of(Ob,ListOb),
           immed_element_list(DescEl,ListOb),!.
    
     immed_element(El,Ob):-
           element(_,El,Ob).           
     immed_element(r(X),Ob):-
     	   bound(X),
           calc_on_approx(ex("real",[]),Desc),
           Desc=Ob,!.
     immed_element(nc(X),Ob):-       %immed_element(ex(X,[]),Ob):-
           calc_on_approx(ex("concepts",[]),Desc),
           Desc=Ob,!,
           name_file(X1,_,_,_),
           upper_lower(X1, X).
    immed_element(X,Ob):-
           calc_on_approx(ex("terms",[]),Desc),
           Desc=Ob,!,
           descriptor(_,X,1).
     immed_element(X,Ob):-
           calc_on_approx(ex("objcts",[]),Desc),
           Desc=Ob,!,
           descriptor(_,X,1),
           isa_calc_type(X,ex("ob",[])).
     immed_element(X,Ob):-
           calc_on_approx(ex("maps",[]),Desc),
           Desc=Ob,!,
           descriptor(_,X,1),
           isa_calc_type(X,ex("mor",[])).      
   
   /*  
     element_of_object(DescEl,DescOb):- bound(DescOb),     
           isomor_with_point(DescOb),!,
           DescEl=DescOb.
    */
     element_of_object(DescEl,DescOb):- 
           subobjects_of(DescOb,ListOb),
           immed_element_list(DescEl,[DescOb|ListOb]).
           
     immed_element_list(DescEl,[H|_]):-
           immed_element(DescEl,H).
     immed_element_list(DescEl,[_|T]):-
           immed_element_list(DescEl,T).




  eq_known(Ex1,Ex2):- calc_on_approx(Ex1,Res1), %должен использоваться на аппроксимации без вывода
  											%и не расширять ее, в отличии от is_equal_not_message
                      calc_on_approx(Ex2,Res2),
                      Res1=Res2.
                      
  is_equal(Ex1,Ex2):- % не отличается от eq_known. Ранее предполагалось, что этот предикат диалоговый. Проверить использование! 
          calc_on_approx(Ex1,Res1),
          calc_on_approx(Ex2,Res2),
   %expr_to_str(Res1,S1), expr_to_str(Res2,S2), format(Str,"Res1= %,  Res2= %",S1,S2 ), dlg_note("Проверка равенства",Str),
          % calc(Ex1,Res1),
          %calc(Ex2,Res2),
          Res1=Res2,!.
   /*  
        % dlg_note("\n Равенство "),
         %dlg_note_ex(Ex1), 
          %dlg_note(" и "),
          %dlg_note_ex(Ex2),
          %dlg_note(" известно, так как оба выражения равны "),
         % dlg_note_ex(Res1),
          %dlg_note(".").
                            
  is_equal(Ex1,Ex2):- 
          dlg_note("Проверка равенства"),
          calc_on_approx(Ex1,Res1),
          calc_on_approx(Ex2,Res2),
          Res1=c(X),       
          Res2=c(Y),
          element(c(X),_),
          element(c(Y),_),
          not(X=Y),!,
         
          dlg_note("\n Известно, что выражения "),
          dlg_note_ex(Ex1), dlg_note(" и "),
          dlg_note_ex(Ex2),
          dlg_note(" не равны."),
          fail.  
                  
    is_equal(Ex1,Ex2):- 
          dlg_note("Проверка равенства"),
          calc_on_approx(Ex1,Res1),
          calc_on_approx(Ex2,Res2),
          order_desc(Res1,Res2,Dmax,Dmin),
          not_eq_desc(Dmax,Dmin),!,
          dlg_note("\n Известно, что выражения "),
          dlg_note_ex(Ex1), dlg_note(" и "),
          dlg_note_ex(Ex2),
          dlg_note(" не равны."),
          fail.          
  is_equal(Ex1,Ex2):-
          dlg_note("Проверка равенства"),
          dlg_note("\n Не знаю, равны ли "),
          dlg_note_ex(Ex1),dlg_note(" и "),
          dlg_note_ex(Ex2),dlg_note("."),
          dlg_note("\n Теперь известно,что выражения  "),
          dlg_note_ex(Ex1),dlg_note(" и "),
          dlg_note_ex(Ex2),dlg_note(" равны.").
          
  is_equal(Ex1,Ex2):-
          dlg_note("Проверка равенства"),!,
          dlg_note("\n Выражения  "),
          dlg_note_ex(Ex1),dlg_note(" и "),
          dlg_note_ex(Ex2),dlg_note(" не равны."),          
          fail.
 
  is_equal(Ex1,Ex2):-
          is_equal_not_message(Ex1,Ex2).
 */         
  is_equal_not_message(Ex1,Ex2):-                          
          eq_known(Ex1,Ex2),!.
  is_equal_not_message(Ex1,Ex2):-
          calc_on_approx(Ex1,Res1),
          calc_on_approx(Ex2,Res2),
          order_desc(Res1,Res2,Dmax,Dmin),
          not_eq_desc(_,Dmax,Dmin),!,
          fail.          
  is_equal_not_message(Ex1,Ex2):-
          calc_on_approx(Ex1,Res1),
          calc_on_approx(Ex2,Res2),
          Res1=ex(X,[]),       
          Res2=ex(Y,[]),
          element(_,ex(X,[]),_),
          element(_,ex(Y,[]),_),
          not(X=Y),!,  
          fail.                  
  is_equal_not_message(Ex1,Ex2):-          
          deduce_on_approx("on"),
          deduce_eq(Ex1,Ex2).   
                
/*
  menudeduce(Ex1,Ex2,StartChoice):-
  	vmenu(4,36,defclr,
    	" Укажите способ продолжения работы ",
       [v(" Попытаться доказать это равенство",13),
        v(" Положить равными эти выражения ",11),
        v(" Положить эти выражения неравными",25),
        v(" Показать ход доказательства равенства",2),
        v(" Вы сами будете доказывать его мне",5)],StartChoice,CHOICE),
         case(CHOICE,Ex1,Ex2).
         
         */
         /*
         
  case(1,Ex1,Ex2):- deduce_eq(Ex1,Ex2),!.
  case(1,Ex1,Ex2):- !, shiftwindow(4),
                    write("\n Вывод равенства не получается."),
                    write("\n Не знаю, равны ли "),                    
                    dlg_note_ex(Ex1),write(" и "),
                    dlg_note_ex(Ex2),write("."),                    
                    menudeduce(Ex1,Ex2,1).
  case(2,Ex1,Ex2):- 
            calc_type(Ex1,Ex1_type),Ex1_type="mor",
            expr_to_str(ex(make_equal_mor,[Ex1,Ex2]),Str), 
            make_equal(Ex1,Ex2),!,
            update_description(Str),
            message_make_inconsist. 
  case(2,Ex1,Ex2):- 
            calc_type(Ex1,Ex1_type),Ex1_type="ob",
            expr_to_str(ex(make_equal_ob,[Ex1,Ex2]),Str), 
            make_equal(Ex1,Ex2),!,
            update_description(Str),
            message_make_inconsist. 
  case(2,Ex1,Ex2):-!,
            shiftwindow(4),
            write("\n Не могу положить равными "),
            dlg_note_ex(Ex1),write(" и "),
            dlg_note_ex(Ex2),write(",так как эти выражения разных типов."),
            fail.
            
  case(3,Ex1,Ex2):- 
            expr_to_str(ex(not_equal,[Ex1,Ex2]),Str), 
            make_not_equal(Ex1,Ex2),!,
            update_description(Str),
            message_make_inconsist,
            fail.                          
  case(3,Ex1,Ex2):-!, 
            write("\n Не могу сделать неравными "),                    
            dlg_note_ex(Ex1),write(" и "),
            dlg_note_ex(Ex2),write("."),                    
            menudeduce(Ex1,Ex2,3).
            
  case(4,Ex1,Ex2):- 
            makewindow(6,main_clr,main_clr,"Доказательство",2,0,22,80), 
            write("\n    Доказательство равенства:"),
            nl, dlg_note_ex(Ex1), write(" = "),
            dlg_note_ex(Ex2), write(" ."),
            waitkey_esc(Key),not(Key=esc),
            calc(Ex1,Res1),
            calc(Ex2,Res2),
            nl,
            write("\n Известно,что "),
            dlg_note_ex(Ex1),
            write(" = "),
            dlg_note_ex(Res1),
            write(" и "),
            dlg_note_ex(Ex2),
            write(" = "),
            dlg_note_ex(Res2),
            write("."),
            waitkey_esc(RKey),not(RKey=esc),
            prove_eq(Ex1,Ex2),!,
            removewindow(6,1).
  case(4,Ex1,Ex2):- !,
            removewindow(6,1),
            shiftwindow(4),  
            write("\n Доказать не удалось."),
            write("\n Не знаю, равны ли "),                    
            dlg_note_ex(Ex1),write(" и "),
            dlg_note_ex(Ex2),write("."),
            shiftwindow(2),                    
            menudeduce(Ex1,Ex2,4).            
  case(5,Ex1,Ex2):- !, 
                    shiftwindow(4),  
                    write("\n Это возможность пока не работает. "),
                    write("\n Не знаю, равны ли "),                    
                    dlg_note_ex(Ex1),write(" и "),
                    dlg_note_ex(Ex2),write("."),
                    menudeduce(Ex1,Ex2,5).      
  */
  
/**********************************************/  
/*    Программа  вывода равенства категорных  */
/*    ex-выражений   (deduce_eq)              */
/**********************************************/

   /*reduce(N,Ex,REx)  приводит категорное выражение
            к каноническому виду , если нет циклов*/ 

 
  deduce_eq(Ex1,Ex2):-
     message_deduce(Ex1,Ex2),
     reduce(0,Ex1,REx1), reduce(0,Ex2,REx2),
     calc(REx1,Res1),  calc(REx2,Res2),
     Res1=Res2,!,
     make_equal(Ex1,Res1),
     make_equal(Ex2,Res1).           

  message_deduce(Ex1,Ex2):-
    %dlg_note("Вывод"),
        expr_to_str(Ex1,Str1),
        expr_to_str(Ex2,Str2),
  	%format(String,"Делается вывод равенства % = %.", Str1,Str2),
	 msg_n(calc,36,[Str1,Str2],b_false).
	 % dlg_note(String).    
  message_deduce(_,_).
 

maxlong(N):-maxlong_deduce(N),!.							
maxlong(100).        
    

reduce(N,Ex,Ex):- maxlong(M),N>=M,!,
                  %format (String,"Длина доказательства превышает максимальную длину,\n равную %.",M),
                  %dlg_note(String),
                  str_int(Mstr,M),
                 msg_n(warning,37,[Mstr],b_true).
                                  
  reduce(N,Ex,Res):-  maxlong(M),N<M,
                  rewrite(Msg,Ex,Res1),!,
              	  msg(rwrt,Msg),
                  N1=N+1,   
                  reduce(N1,Res1,Res).                 
  reduce(N,ex(Op,Arg),Res):-
                  maxlong(M),N<M,
                  reduce_list(N,Arg,[],Reslist),
                  not(Arg=Reslist),!,
                  N1=N+1,
                  reduce(N1,ex(Op,Reslist),Res).
reduce(_,Ex,Ex).             
      
        

  reduce_list(_,[],LEx,Res):-!,
  	turn_list(LEx,[],Res).
  reduce_list(N,[H|T],List,Res):- reduce(N,H,ResH),
                                     reduce_list(N,T,[ResH|List],Res).

turn_list([],Res,Res):-!.
turn_list([H|T],L,Res):-
	turn_list(T,[H|L],Res).


 
    /************ КОНЕЦ ПРОГРАММЫ ПОКАЗА ДОКАЗАТЕЛЬСТВА ***********/     
 
 
minob_of_element(El,Minob):-
 	calc(El,DescEl),
 	findall(Ob,element(_,DescEl,Ob),ListOb),
 	calc_type(El,Type),
 	intersect_list([Type|ListOb],ex("initial",[]),Minob).
 	
intersect_list([Ob],ex("initial",[]),Ob):-!.
intersect_list([],Res,Res):-!.
intersect_list([H|T],B,Res):-!,
		calc(ex("intersect",[H,B]),B1),
		intersect_list(T,B1,Res).   
     
make_equal(nc(Name),Ex2):-!,
		minob_of_element(Ex2,Type),
		create_new_constant(nc(Name),Type,Res),
		make_equal(Res,Ex2).
make_equal(Ex1,nc(Name)):-!,
		make_equal(nc(Name),Ex1).
make_equal(Ex1,Ex2):-
	calc(Ex1,D1),
	calc(Ex2,D2),!,
	make_equal_1(Ex1,Ex2,D1,D2).		
make_equal(Ex1,Ex2):-
           not(calc(Ex1,_)),!,
           expr_to_str(Ex1,StrEx1),
           expr_to_str(Ex2,StrEx2),
           %format(Str,"Не вычисляется выражение \"%\".\nНе могу приравнять выражение \"%\" и \"%\".",StrEx1,StrEx1,StrEx2),
	   msg_n(diagn,38,[StrEx1,StrEx1,StrEx2],b_false),	
	   fail.
make_equal(Ex1,Ex2):-
           not(calc(Ex2,_)),!,
           expr_to_str(Ex1,StrEx1),
           expr_to_str(Ex2,StrEx2),
           %format(Str,"Не вычисляется выражение \"%\".\nНе могу приравнять выражение \"%\" и \"%\".",StrEx2,StrEx1,StrEx2),
	    msg_n(diagn,40,[StrEx2,StrEx1,StrEx2],b_false),
	    fail.
 
make_equal_1(_,_,D1,D2):-
              D1=D2,!.             

make_equal_1(Ex1,Ex2,D1,D2):- % Тип первого терма является подтипом второго
             calc_type(D1,T1),
             calc_type(D2,T2),
             is_subobject(T1,T2),!,
             make_equal_desc(D1,D2),
             message_make_equal(Ex1,Ex2).
make_equal_1(Ex1,Ex2,D1,D2):- % Тип второго терма является подтипом первого
             calc_type(D1,T1),
             calc_type(D2,T2),
             is_subobject(T2,T1),!,
             make_equal_desc(D1,D2),
             message_make_equal(Ex1,Ex2).

make_equal_1(Ex1,Ex2,D1,D2):- % Существует пересечение типов  термов
             calc_type(D1,T1),
             calc_type(D2,T2),
            calc_on_approx(ex("intersect",[T1,T2]),_),!,
             make_equal_desc(D1,D2),
             message_make_equal(Ex1,Ex2).

make_equal_1(Ex1,Ex2,_,_):- % Типы термов несовместны.
             calc_type(Ex1,T1),
             calc_type(Ex2,T2),
             expr_to_str(Ex1,StrEx1),
             expr_to_str(Ex2,StrEx2),
             expr_to_str(T1,Typ1),
             expr_to_str(T2,Typ2), 
             %format(Str,"Несовместны типы  \"%\" и \"%\" термов % и \%.\nНе могу приравнять эти термы.\nВозможно предварительно нужно построить пересечение их типов.",Typ1,Typ2,StrEx1,StrEx2),
		msg_n(err,41,[Typ1,Typ2,StrEx1,StrEx2],b_false),
		retractall(fatal_error),
		assert(fatal_error),
		fail.           
     
/* message_make_equal - выдает сообщения о новом равенстве, 
                        если это потребовано в меню Сообщений*/
 
message_make_equal(Ex1,Ex2):-
             expr_to_str(Ex1,Str1),
             expr_to_str(Ex2,Str2),!,
             %format(Str,"Построение равенства % = % выполнено.",Str1,Str2),
             msg_n(calc,42,[Str1,Str2],b_false).            
    
/* make_equal_desc - приравнивает два дескриптора  */
 
make_equal_desc(D1,D2):-
             order_desc(D1,D2,Dmax,Dmin),
             retractall(eq_desc(_,_)),
             retractall(modification_resalt_op(_,_)),
             assert(eq_desc(Dmax,Dmin)),
             factor_set_desc,
             not(fatal_error).  

/********************************************************************/             
/* factor_set_desc - максимально факторизует множество дескрипторов */
/*     по отношнию эквивалентности, порожденному отношением eq_desc */
/*     и отношением равенства результатов на равных аргументах для  */
/*     действия операций из resalt_op                               */
/********************************************************************/
factor_set_desc:-
             retract(eq_desc(D1,D2)),
             replace_in_not_eq_desc(D1,D2),             
             replace_in_resalt_op(D1,D2),
             replace_in_subobject(D1,D2),
             replace_in_element(D1,D2),
             replace_in_eq_desc(D1,D2),
             replace_in_descriptor(D1,D2),
             new_equalities,
             fail.

factor_set_desc.
  
/*  replace_in_not_eq_desc(D1,D2) заменяет D1->D2 в not_eq_desc и проверяет
    на противоречивость */
replace_in_not_eq_desc(El,Nil):- /*  */
             calc_on_approx(ex("initial",[]),Desc),
             Desc=Nil,
             is_not_nil(El),!,
             retractall(fatal_error),
            assert(fatal_error),
            message_inconsist(El,ex("initial",[]),"Приравниваются непустой  к пусто."),
             replace_not_message(El,Nil).
             
replace_in_not_eq_desc(Nil,El):- 
             calc_on_approx(ex("initial",[]),Desc),
             Desc=Nil,
             is_not_nil(El),!,
             retractall(fatal_error),
            assert(fatal_error),
             message_inconsist(El,ex("initial",[]),"Приравниваются непустой  к пусто."),
             replace_not_message(El,Nil). 
  
replace_in_not_eq_desc(ex(Op1,[]),ex(Op2,[])):- 
             descriptor(_,ex(Op1,[]),1),
             descriptor(_,ex(Op2,[]),1),
             not(Op1=Op2),
             ask_synonym_desc(ex(Op1,[]),ex(Op2,[])),!,
  	     get_in_template(IdDostup1,ID_Concept1, Id_template1,Text1, Op1,Comment1,L1,T1),
             get_in_template(_IdDostup2,_ID_Concept2, _Id_template2,Text2, Op2,_Comment2,_L2,_T2),
             !,
  	     idconcept(IdConc),
	     retractall(in_template(IdDostup1,ID_Concept1, Id_template1,Text1, Op1,Comment1,L1,T1)),
	     format(Comment,"%\n\tСиноним термина %.",Comment1, Text2),
	     assert(in_template(IdDostup1,IdConc, Id_template1,Text1, Op2,Comment,L1,T1)),
	     replace_not_message(ex(Op1,[]),ex(Op2,[])).
	     %;
	     %dialog_answer("no"),
	     %write("no"),
	     %exit.

replace_in_not_eq_desc(Desc1,Desc2):- 
             descriptor(_,Desc1,1),
             descriptor(_,Desc2,1),
             
             write("3 clause\n", Desc1),
             
             is_not_eq_desc(Desc1,Desc2),!,
             retractall(not_eq_desc(_,Desc1,Desc2)),
             message_inconsist(Desc1,Desc2,"Приравниваются  константы."),
             replace_not_message(Desc1,Desc2). 
  	%приравнивается множество с константой
  replace_in_not_eq_desc(Desc1,Desc2):- 
             descriptor(_,Desc1,1),
             Desc2=ex("set",_),
             not(Desc1=Desc2),!,
             retractall(not_eq_desc(_,Desc1,Desc2)),
             message_inconsist(Desc1,Desc2,"Приравниваются  константа и множество."),
             replace_not_message(Desc1,Desc2). 
  replace_in_not_eq_desc(Desc1,Desc2):- 
             Desc1=ex("set",_),
             descriptor(_,Desc2,1),
             not(Desc1=Desc2),!,
             retractall(not_eq_desc(_,Desc1,Desc2)),
             message_inconsist(Desc1,Desc2,"Приравниваются множество и константа."),
             replace_not_message(Desc1,Desc2). 
  replace_in_not_eq_desc(Desc1,Desc2):- 
              Desc1=ex("set",A),
              Desc2=ex("set",B),
             not(A=B),!,
             retractall(not_eq_desc(_,Desc1,Desc2)),
             message_inconsist(Desc1,Desc2,"Приравниваются множества."),
             replace_not_message(Desc1,Desc2). 
   %Случай множества и числа не рассмотрен. Они разного типа.
    replace_in_not_eq_desc(Desc1,Desc2):- 
             not(Desc1=ex(_,_)),
             not(Desc2=ex(_,_)),
             is_not_eq_desc(Desc1,Desc2),!,
             retractall(not_eq_desc(_,Desc1,Desc2)),
             message_inconsist(Desc1,Desc2,"Приравниваются  константы."),
             replace_not_message(Desc1,Desc2). 
                         
  replace_in_not_eq_desc(D1,D2):-  
             not_eq_desc(_,D1,D2),!,
             retractall(not_eq_desc(_,D1,D2)),
             message_inconsist(D1,D2, ""),
             replace_not_message(D1,D2).
  replace_in_not_eq_desc(D1,D2):-
             replace_not_message(D1,D2). 
          
 ask_synonym_desc(ex(Op1,[]),ex(Op2,[])):-
  	     synonym_desc(ex(Op1,[]),ex(Op2,[])),!.
 ask_synonym_desc(ex(Op1,[]),ex(Op2,[])):-
  	     expr_to_str(ex(Op1,[]),St1),
             expr_to_str(ex(Op2,[]),St2),
             format(Prompt1,"Делаются равными термины <BR> %  и  %, которые предполагались неравными.",St1,St2),
	     msg_n(warning,75,[St1,St2],b_false),
	     format(Prompt,"%\<BR>Сделать их синонимами?",Prompt1 ),
	     %not (dialog_answer("no")),
	     dlgAsk_call_eq(Prompt),
	     %dialog_answer(A),
	     %write(A),
	     %retractall (dialog_answer(_)),
	     %Bool = "true",
	     %resp_default= dlg_Ask("Противоречие",Prompt,["&Да","&Нет"]),
	     %tnotion_window(WinTn,"tnotion",_),
	     expr_to_str(ex("synonym",[ex(Op2,[]),ex(Op1,[])]),Text),
	     format(_SText,"<BR>%.",Text),
	    % edit_AppendStr(WinTn,SText),
	     /***
	     retractall(tnotion_window(WinTn,"tnotion",_)),
	  
	NewText=edit_GetText(WinTn),
	assert(tnotion_window(WinTn,"tnotion",NewText)),
	retractall(edit_window(WinTn,_,_)),
	assert(edit_window(WinTn,"tnotion",NewText)),
	   ***/
	        /***TO SUBSTITUTE THE STRING BELOW ***/
	        NewText = "Абракадарба",
	put_defin1(NewText),
	     !.
	     
	             
          
  replace_not_message(D1,D2):-
             not_eq_desc(_,D1,D),
             retractall(not_eq_desc(_,D1,D)),
             order_desc(D2,D,Dmax,Dmin),
             retractall(not_eq_desc(_,Dmax,Dmin)),
             idconcept(Id),
             assert(not_eq_desc(Id,Dmax,Dmin)),
             fail.
  replace_not_message(D1,D2):-
             not_eq_desc(_,D,D1),
             retractall(not_eq_desc(_,D,D1)),
             order_desc(D2,D,Dmax,Dmin),
             retractall(not_eq_desc(_,Dmax,Dmin)),
             idconcept(Id),
             assert(not_eq_desc(Id,Dmax,Dmin)),
             fail.
  replace_not_message(_,_).                        

  message_inconsist(D1,D2,Str):-
 		expr_to_str(D1,St1),
          	expr_to_str(D2,St2),
             	format(Prompt1,"Делаются равными \n %  и  %, которые предполагались неравными.",St1,St2),
	     	msg(warning,Prompt1),
	     	format(Prompt,"%\n %\nПродолжать?",Prompt1,Str),
	     	%resp_default= dlg_Ask("Противоречие",Prompt,["&Да","&Нет"]),
	     	dlgAsk_call_eq(Prompt),!,
	     	%Bool = "true", !,
	     	idconcept(Id),
	     	assert(inconsist(Id,D1,D2)),
             	retractall(current_inconsist(_)),
             	assert(current_inconsist(Id)).
  message_inconsist(_,_,_):-
  		retractall(fatal_error),
  		assert(fatal_error),
  		fail. 
   
      
  		
  is_not_eq_desc(r(X),r(Y)):-!,
  		Z=X-Y,
  		W=abs(Z),
  		W > (0.1e-100).
  is_not_eq_desc(Desc1,Desc2):-
  	not(Desc1=Desc2).		             
  		             
  is_not_nil(Ob):- isomor_with_point(Ob),!.
  is_not_nil(Ob):- element_of_object(_,Ob),!.
             
             
/*  replace_in_resalt_op(D1,D2)  заменяет в resalt_op(Ex,D)    D1->D2 */ 
  replace_in_resalt_op(D1,D2):-
  		retractall(descriptor(_,D1,_)),
               %not(calc_on_approx(D1,_)), %может вызвать зацикливание
               idconcept(Id),
               assert(resalt_op(Id,D1,D2)),
  		fail.
  replace_in_resalt_op(D1,D2):-
  	     resalt_op(_,ex(Op,Arg),D),
  	     memberEx(D1,[D|Arg]),
             retractall(resalt_op(_,ex(Op,Arg),_)),
             replace_in_list(Arg,ArgNew,D1,D2),
             replace_in_list([D],ListDnew,D1,D2),
             ListDnew=[Dnew],
             not(Dnew=ex(Op,ArgNew)),
             assert(modification_resalt_op(ex(Op,ArgNew),Dnew)),
             not(calc_on_approx(ex(Op,ArgNew),_)),
             idconcept(Id),
             assert(resalt_op(Id,ex(Op,ArgNew),Dnew)),
             fail.
 replace_in_resalt_op(_,_).

  replace_in_descriptor(D1,D2):-
  	     descriptor(_,ex(Op,Arg),_),
  	     memberEx(D1,Arg),
             replace_in_list(Arg,ArgNew,D1,D2),
             retractall(descriptor(_,ex(Op,Arg),_)),
             assert(modification_resalt_op(ex(Op,ArgNew),ex(Op,ArgNew))),
             not(calc_on_approx(ex(Op,ArgNew),_)),
             weigh(ex(Op,ArgNew),W),
             idconcept(Id),
             assert(descriptor(Id,ex(Op,ArgNew),W)),
              fail.
  replace_in_descriptor(_,_).

 
memberEx(D,[D|_]):-!.
memberEx(D,[ex(_,Arg)|_]):-memberEx(D,Arg),!.
memberEx(D,[_|T]):-memberEx(D,T),!.

/************** программа  replace_in_subobject(D1,D2) ********/ 
/*  находит все равные дескрипторы Desc1 и Desc2, т.е. такие, */
/*  что Desc1 включает Desc2 и наоборот Desc2 включает Desc1  */
/*  при дополнительном равенстве  D1 = D2. Далее она вводит   */
/*  эти равенства в eq_desc и заменяет равные дескрипторы в   */
/*  subobject на минимальный десриптор.                       */
/*         Ограничения: is_subobject работает, если отношение */
/*         subobject не содержит равных пар и не имеет циклов.*/
/**************************************************************/
    /* replace_in_subobject(D1,_):-  замена D1->D2 при действии make_equal 
             calc_type(D1,T1),
             not(is_subobject(T1,ex("ob",[]))),!.   */
     replace_in_subobject(D1,_):- 
             not(subobject(_,D1,_)),
             not(subobject(_,_,D1)),!.             
     replace_in_subobject(D1,D2):- 
             find_all_equal_desc(D1,D2,List_desc),
             min_desc(List_desc,Dmin),
             replace_in_list(List_desc,List_without_D1,D1,D2),
             assert_list_in_eq_desc(List_without_D1,Dmin),
             replace_list_in_subobject(List_desc,Dmin).
            % delete_element(D2,Dmin). % если D2 или Dmin - элементы,Dmin 
                                     % вычищается из subobject как подобъект 
     replace_in_subobject(_,_).
     
     find_all_equal_desc(D1,D2,[D1,D2|List_desc]):-
             findall(Desc1,between(D1,D2,Desc1),List1),
             findall(Desc2,between(D2,D1,Desc2),List2),
             append(List1,List2,List_desc).


:- redefine_system_predicate(between/3).

     /*between(D1,D2,D) не выдает D- элементD2, кото,рый D1<D */         
     between(D1,D2,_):-      
            not(is_subobject(D1,D2)),!,             
            fail.
     between(_,D2,D2).
     between(D1,D2,Desc):-    /* может не выдать Desc ,равный D2   */
            subobject(_,Desc,_),
            is_subobject(D1,Desc),
            is_subobject(Desc,D2).
     
     replace_list_in_subobject(List_desc,Dmin):-  /* если Dmin  пусто */
            is_equal_not_message(Dmin,ex(initial,[])),
            members(Desc,List_desc),
            retractall(subobject(_,Desc,_)),
            retractall(subobject(_,_,Desc)),
            fail.
     replace_list_in_subobject(List_desc,Dmin):-  /* замена арг1 в subobject */
            members(Desc1,List_desc),
            subobject(_,Desc1,Desc2),
            retractall(subobject(_,Desc1,Desc2)),
            retractall(subobject(_,Dmin,Desc2)),
            not(Dmin=Desc2),
            idconcept(Id),
            asserta(subobject(Id,Dmin,Desc2)),  
            fail.
     replace_list_in_subobject(List_desc,Dmin):-  /* замена арг2 в subobject */
            members(Desc2,List_desc),
            subobject(_,Desc1,Desc2),
            retractall(subobject(_,Desc1,Desc2)),
            retractall(subobject(_,Desc1,Dmin)),
            not(Desc1=Dmin),
            idconcept(Id),
            asserta(subobject(Id,Desc1,Dmin)),  
            fail.   
     replace_list_in_subobject(_,_):- 
            modi_subobject.   /* убирает лишние по транзитивности пары */

/*
     delete_element(_,X):-  % удаление элемента из subobject,
                                 %если D2 или Dmin - элементы       
            element(_,X,_),!,
            retractall(subobject(_,X,_)).
     delete_element(X,Dmin):-  
            element(_,X,_),!,
            retractall(subobject(Dmin,_)).
     delete_element(_,_).
   */  
      members(X,[X|_]).
members(X,[_|L]):-members(X,L).   
           
          /*********** конец программы replace_in_subobject ********/          


/****    замена D1 -> D2  в предикате БД element (Element,Object)     ****/    
/* в программе используется предикат БД  element(DescElement,DescObject),*/
/* означающий непосредственую принадлежность (без промежуточных областей)*/
/*      Для правильной работы программы недопустимо существовоние пар:   */
/*             element(X,a) и element(a,Y)  одновременно                 */
/*************************************************************************/

 replace_in_element(D1,D2):- /* если области D1 и D2 изоморфны точке */
             isomor_with_point(D1),
             isomor_with_point(D2),!,
             repl_arg1_element(D1,D2).
 replace_in_element(D1,D2):- /* если область D2 изоморфна точке,то новые равен */
             isomor_with_point(D2),!,
             inconsist_if_two_elements_in(D1),
             findall(DescEl,element_of_object(DescEl,D1),List1),
             list_not_repeat([D2|List1],ListDesc),
             retractall(element(_,_,D1)),
             repl_arg1_element(D1,D2),
             min_desc(ListDesc,Dmin),
             assert_list_in_eq_desc(ListDesc,Dmin),
             message_eq_with_point(D1,ListDesc,Dmin).
 replace_in_element(D1,D2):- /* если область D1 изоморфна точке,то новые равен */             
             isomor_with_point(D1),!,
             inconsist_if_two_elements_in(D2),
             findall(DescEl,element_of_object(DescEl,D2),List1),
             list_not_repeat([D2|List1],ListDesc),
             retractall(element(_,_,D2)),
             repl_arg1_element(D1,D2),
             min_desc(ListDesc,Dmin),
             assert_list_in_eq_desc(ListDesc,Dmin),
             message_eq_with_point(D1,ListDesc,Dmin).
 replace_in_element(D1,D2):- /* замена области D1 на D2 во втором аргументе */
             element(_,D,D1),
             retractall(element(_,D,D1)),
             retractall(element(_,D,D2)),
             idconcept(Id),
             assert(element(Id,D,D2)),
             fail.
 replace_in_element(_,_).

 repl_arg1_element(D1,D2):- /* замена элемента D1 на D2 в первом аргументе */
             element(_,D1,D),
             retractall(element(_,D1,D)),
             retractall(element(_,D2,D)),
             idconcept(Id),
             asserta(element(Id,D2,D)),
             retractall(element(_,D,D2)),
             fail. 
 repl_arg1_element(_,_).
 
 
             
 inconsist_if_two_elements_in(Ob):-
             any_two_elements(Ob,X,Y),!,
           %  element_of_object(X,Ob),X=c(_),
           %  element_of_object(Y,Ob),Y=c(_),
           %  not(X=Y),!,
              message_inconsist(X,Y,"Содержащая эти элементы область делается одноэлементной.").                
 inconsist_if_two_elements_in(_).     
 
 


  message_eq_with_point(_,_,_).  
    /********  конец программы   replace_in_element  ********/

/* replace_in_eq_desc(D1,D2) - заменяет в eq_desc D1->D2,не вводя eq_desc(D2,D2)*/
  replace_in_eq_desc(D1,D2):-
             eq_desc(D1,D),
             retractall(eq_desc(D1,D)),
             order_desc(D2,D,Dmax,Dmin),
             assert_in_eq_if_not(Dmax,Dmin),
             fail.
  replace_in_eq_desc(D1,D2):-
             eq_desc(D,D1),
             retractall(eq_desc(D,D1)),
             order_desc(D2,D,Dmax,Dmin),
             assert_in_eq_if_not(Dmax,Dmin),             
             fail.
  replace_in_eq_desc(_,_).
    
/*  replace_in_list(Desclist,RDesclist,D1,D2) - заменяет в Desclist   D1 ->D2 */
  replace_in_list([],[],_,_):- !.
  replace_in_list([D1|T1],[D2|T2],D1,D2):-!,
              replace_in_list(T1,T2,D1,D2).
 replace_in_list([ex(Op,Larg)|T1],[ex(Op,LargD2)|T2],D1,D2):-!,
              replace_in_list(Larg,LargD2,D1,D2),
              replace_in_list(T1,T2,D1,D2).  
  replace_in_list([H|T1],[H|T2],D1,D2):-
              replace_in_list(T1,T2,D1,D2).

/* new_equalities - последовательно выбрасывает факты из modification_resalt_op и,
                  если находит факты с теми же аргументами в resalt_op и modification_resalt_op,
                  то расщиряет отношение eq_desc  */
  new_equalities:-
              retract(modification_resalt_op(Exp,D)),
              findall(Res,modification_resalt_op(Exp,Res),Desclist1),
              findall(Desc,resalt_op(_,Exp,Desc),Desclist2),   
              append(Desclist1,Desclist2,Desclist),
              retractall(modification_resalt_op(Exp,_)),
              calc_on_approx(Exp,D1),
              min_desc([D1,D|Desclist],Dmin), 
              assert_list_in_eq_desc([D1,D|Desclist],Dmin), 
              not(Desclist2=[]),
              retractall(resalt_op(_,Exp,_)),
              idconcept(Id),
              assert(resalt_op(Id,Exp,Dmin)),
              fail.
  new_equalities.            
  
 /* assert_list_in_eq_desc(Desclist,D) - вставляет в eq_desc 
          строгоупорядоченные пары (D1,D),где D1 из Desclist */
  assert_list_in_eq_desc([],_):-!.
  assert_list_in_eq_desc([D|T],D):-!,
              assert_list_in_eq_desc(T,D).
  assert_list_in_eq_desc([D1|T],D):-
              order_desc(D1,D,Dmax,Dmin),
              assert_in_eq_if_not(Dmax,Dmin),
              assert_list_in_eq_desc(T,D).
              
  assert_in_eq_if_not(D1,D2):-
              retractall(eq_desc(D1,D2)),
              assert(eq_desc(D1,D2)).            

/* min_desc(Desclist,Dmin) находит минимальный дескриптор 
                           в непустом списке Desclist */
 min_desc([D],D):-!.
 min_desc([D,D],D):-!.
 min_desc([D1,D2],Dmin):-
              order_desc(D1,D2,_,Dmin),!.
  min_desc([D1,D2],Dmin):-!,
              order_expr(D1,D2,_,Dmin).
 min_desc([D1|T],Dmin):-
              min_desc(T,D2),
              min_desc([D1,D2],Dmin).          

/* order_desc(D1,D2,Dmax,Dmin) упорядочивает дескрипторы; на равных - fail */
 order_desc(D1,D2,D1,D2):-
              descriptor(_,D1,W1),
              descriptor(_,D2,W2),

              W1 @> W2,
              
              !.

 order_desc(D1,D2,D2,D1):-
              descriptor(_,D1,W1),
              descriptor(_,D2,W2),

              W1 @< W2,
              
               !.
               
 order_desc(D1,D2,Dmax,Dmin):-
              descriptor(_,D1,_),
              descriptor(_,D2,_),
              order_expr(D1,D2,Dmax,Dmin),!.
              
order_desc(r(X),r(X),_,_):-!,fail.
order_desc(r(X),D,D,r(X)):-!.
order_desc(D,r(X),D,r(X)):-!.

order_desc(ex("set",Arg),ex("set",Arg),_,_):-!,fail.
order_desc(D1,ex("set",Arg),D1,ex("set",Arg)):-!.
order_desc(ex("set",Arg),D2,D2,ex("set",Arg)):-!.

/*
order_desc(ex("emptyset",[]),ex("emptyset",[]),_,_):-!,fail.
order_desc(D1,ex("emptyset",[]),D1,ex("emptyset",[])):-!.
order_desc(ex("emptyset",[]),D2,D2,ex("emptyset",[])):-!.
 */             


/*  order_expr(Ex1,Ex2,Exmax,Exmin)  лексикографически упорядочивает 
                                   два ex-выражения; на равных - fail */                            
/* Убрано 23.12.02
 order_expr(ex(Op,Arg),c(A),c(A),ex(Op,Arg)):-
 				const(A,c("var")),!.
 order_expr(c(A),ex(Op,Arg),c(A),ex(Op,Arg)):-
 				const(A,c("var")),!.

 order_expr(c(A),ex(Op,[]),c(A),ex(Op,[])):-!. 
 order_expr(ex(Op,[]),c(A),c(A),ex(Op,[])):-!.

 order_expr(c(A),ex(Op,Arg),ex(Op,Arg),c(A)):-!. 
 order_expr(ex(Op,Arg),c(A),ex(Op,Arg),c(A)):-!.

 order_expr(c(B),c(A),c(B),c(A)):-
              B>A,!. 
 order_expr(c(A),c(B),c(B),c(A)):-
              B>A,!. 
*/
 order_expr(r(A),r(A),_,_):-!,fail.
/* Эаменила из-за множеств 20.01.03 
 order_expr(r(A),D,D,r(A)):-!. 
 order_expr(D,r(A),D,r(A)):-!.*/
% Добавила 20.01.03 замену из-за множеств
 order_expr(r(A),r(B),r(A),r(B)) :- 

 	A @> B,
 	
 	!.
 	
 order_expr(r(A),r(B),r(B),r(A)) :- 

  	B @> A,
 	
  	!.
 
 order_expr(ex(Op,Arg),r(A),ex(Op,Arg),r(A)) :- !. 
 order_expr(r(A),ex(Op,Arg),ex(Op,Arg),r(A)) :- !. 
 
 order_expr(ex(Op,Arg),nc(A),ex(Op,Arg),nc(A)) :- !.
 order_expr(nc(A),ex(Op,Arg),ex(Op,Arg),nc(A)) :- !. 
 
 order_expr(nc(B),nc(A),nc(B),nc(A)) :- 

  	B @> A,
 	
 	!. 
 	
 order_expr(nc(A),nc(B),nc(B),nc(A)) :- 

  	B @> A,
 	
 	!.               

 order_expr(nc(B),r(A),nc(B),r(A)) :- !.
 
 order_expr(r(A),nc(B),nc(B),r(A)) :- !.               
 
%Конец замены         

 order_expr(Ex1,Ex2,Ex1,Ex2):-
 	expr_to_str(Ex1,StrEx1),
 	expr_to_str(Ex2,StrEx2),    

        StrEx1 @> StrEx2,
 	
        !. 
              
 order_expr(Ex1,Ex2,Ex2,Ex1):-
 	expr_to_str(Ex1,StrEx1),
 	expr_to_str(Ex2,StrEx2),           

        StrEx1 @< StrEx2,
 	
        !. 
 
 /*
 order_expr(ex(Op1,Arg1),ex(Op2,Arg2),ex(Op1,Arg1),ex(Op2,Arg2)):-
              Op1>Op2,!. 
 order_expr(ex(Op2,Arg2),ex(Op1,Arg1),ex(Op1,Arg1),ex(Op2,Arg2)):-
              Op1>Op2,!. 
 order_expr(ex(Op,Arg1),ex(Op,Arg2),ex(Op,Argmax),ex(Op,Argmin)):-
              order_expr_list(Arg1,Arg2,Argmax,Argmin).
 */             
              
 /*************** конец программы factor_set_desc ***************/
 
/**************************************************************************/   
/*      Программа положить неравными два выражения make_not_equal         */
/*                  имеется проверка на противоречивость                  */ 
/**************************************************************************/
  make_not_equal(Ex1,Ex2):-
            not(deduce_eq(Ex1,Ex2)),!,
            calc(Ex1,Res1),
            calc(Ex2,Res2),
            order_desc(Res1,Res2,Dmax,Dmin),
            idconcept(Id),
            assert(not_eq_desc(Id,Dmax,Dmin)). /* запоминается неравенство*/

/*
  make_not_equal(Ex1,Ex2):-
            makewindow(26,74,74,"",6,10,7,40),
            write("\n Можно доказать равенство выражений\n"),
            dlg_note_ex(Ex1),write(" и "),
            dlg_note_ex(Ex2),write(",\nпоэтому не могу сделать их неравными."),
            !,           
            waitkey,
            removewindow(26,1),
            fail.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
