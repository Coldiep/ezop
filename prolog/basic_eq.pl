

:- module(basic_eq, [
basic_equalities/1
]).
:- style_check(+string).

:- use_module(vp52_compat).
:- use_module(editor).
:- use_module(calc).
:- use_module(make_eq).
:- use_module(db).

%include "editor.pre"


  
	
	basic_equalities(ex("appl",[F,El])) :-
		calc_on_approx(El,DescEl),
		calc_type(DescEl,Ob),
		calc_on_approx(ex("image",[F,Ob]), Im),
		make_element(ex("appl",[F,El]),Im),
		fail.
	/*
	basic_equalities(ex("appl",[F,El])) :-
		calc_on_approx(El,DescEl),
		not(element(_, DescEl,_)),
		make_element(ex("appl",[F,El]),ex("cod",[F])),
		fail.
	*/
	basic_equalities(ex("intersect",[Ob1,Ob2])) :-
		is_equal(Ob1,ex("initial",[])),
		make_equal(ex("intersect",[Ob1,Ob2]),
					ex("initial",[])
					),fail.
	basic_equalities(ex("intersect",[Ob1,Ob2])) :-
		is_equal(Ob2,ex("initial",[])),
		make_equal(ex("intersect",[Ob1,Ob2]),
					ex("initial",[])
				
					),fail.
	basic_equalities(ex("intersect",[Ob1,Ob2])) :-
		is_subobject(Ob1,Ob2),
		make_equal(ex("intersect",[Ob1,Ob2]),
					Ob1
					),fail.
	basic_equalities(ex("intersect",[Ob1,Ob2])) :-
		is_subobject(Ob2,Ob1),
		make_equal(ex("intersect",[Ob1,Ob2]),
					Ob2
					),fail.
	basic_equalities(ex("intersect",[Ob1,Ob2])) :-
		make_subobject(ex("intersect",[Ob1,Ob2]),Ob1),
		make_subobject(ex("intersect",[Ob1,Ob2]),Ob2).
	/*
	basic_equalities(ex("intersect",[Ob1,ex("intrsect",[Ob2,Ob3])])) :-
		make_subobject(ex("intersect",[Ob1,ex("intrsect",[Ob2,Ob3])]),Ob2).
		*/
	
	basic_equalities(ex("com",[F1,F2])) :-
		make_equal(
		ex("dom",[ex("com",[F1,F2])]),
		ex("dom",[F2])
		),fail.
	basic_equalities(ex("com",[F1,F2])) :-
		make_equal(
		ex("cod",[ex("com",[F1,F2])]) , 
		ex("cod",[F1])
		),fail.
		
	basic_equalities(ex("id",[A]))  :-
		make_equal(ex("dom",[ex("id",[A])]),A),fail.
	basic_equalities(ex("id",[A]))  :-
		make_equal(ex("cod",[ex("id",[A])]),A),fail.
	
	basic_equalities(ex("fin",[A])):-
	 	make_equal(ex("dom",[ex("fin",[A])]),A),fail.
	basic_equalities(ex("fin",[A])):-
	 	make_equal(ex("cod",[ex("fin",[A])]),ex("final",[])),fail.

	basic_equalities(ex("init",[A])):-
	 	make_equal(ex("dom",[ex("init",[A])]),ex("initial",[])),fail.
	basic_equalities(ex("init",[A])):-
	 	make_equal(ex("cod",[ex("init",[A])]), A ),fail.
	 	
	basic_equalities(ex("inclusion",[A,B])):-	 	
	        make_morphism(A,
                              ex("inclusion",[A,B]),
                              B),fail.

basic_equalities(ex("proj1",[X,Y])):-
        make_equal(ex("cod",[ex("proj1",[X,Y])]),X),fail.
basic_equalities(ex("proj1",[X,Y])):-		
	make_equal(ex("cod",[ex("proj2",[X,Y])]),Y),fail.
basic_equalities(ex("proj1",[X,Y])):-		
	make_equal(ex("dom",[ex("proj1",[X,Y])]),ex("prod_ob",[X,Y])),fail.
basic_equalities(ex("proj1",[X,Y])):-		
	make_equal(ex("dom",[ex("proj2",[X,Y])]),ex("prod_ob",[X,Y])),fail.


basic_equalities(ex("proj2",[X,Y])):-
        make_equal(ex("cod",[ex("proj1",[X,Y])]),X),fail.
basic_equalities(ex("proj2",[X,Y])):-
	make_equal(ex("cod",[ex("proj2",[X,Y])]),Y),fail.
basic_equalities(ex("proj2",[X,Y])):-
	make_equal(ex("dom",[ex("proj1",[X,Y])]),ex("prod_ob",[X,Y])),fail.
basic_equalities(ex("proj2",[X,Y])):-
	make_equal(ex("dom",[ex("proj2",[X,Y])]),ex("prod_ob",[X,Y])),fail.

basic_equalities(ex("prod_el",[E1,E2])):-
				calc_type(E1,Ob1),
				calc_type(E2,Ob2),
				make_element(ex("prod_el",[E1,E2]),ex("prod_ob",[Ob1,Ob2])),
				fail.
basic_equalities(ex("prod_el",[E1,E2])):-
				calc_type(E1,Ob1),
				element(_,E2,Ob2),
				make_element(ex("prod_el",[E1,E2]),ex("prod_ob",[Ob1,Ob2])),
				fail.
basic_equalities(ex("prod_el",[E1,E2])):-
				element(_,E1,Ob1),
				calc_type(E2,Ob2),
				make_element(ex("prod_el",[E1,E2]),ex("prod_ob",[Ob1,Ob2])),
				fail.
basic_equalities(ex("prod_el",[E1,E2])):-
				element(_,E1,Ob1),
				element(_,E2,Ob2),
				make_element(ex("prod_el",[E1,E2]),ex("prod_ob",[Ob1,Ob2])),
				fail.

basic_equalities(ex("prod_mor",[F,G])):-
        make_equal(
        ex("dom",[ex("prod_mor",[F,G])]),ex("dom",[F])
        ),fail.
basic_equalities(ex("prod_mor",[F,G])):-
	make_equal(
	ex(prod_ob,[ex("cod",[F]),ex("cod",[G])]),
	ex("cod",[ex("prod_mor",[F,G])])
	),fail.
basic_equalities(ex("prod_mor",[F,G])):-
	make_equal(
    ex("com",[ex("proj1",[ex("cod",[F]),ex("cod",[G])]),ex("prod_mor",[F,G])]),
    F
	),fail.
basic_equalities(ex("prod_mor",[F,G])):-
	make_equal(
    ex("com",[ex("proj2",[ex("cod",[F]),ex("cod",[G])]),ex("prod_mor",[F,G])]),
    G
	),fail.


basic_equalities(ex("prod_ob",[X,Y])):-
        make_equal(ex("cod",[ex("proj1",[X,Y])]),X),fail.
basic_equalities(ex("prod_ob",[X,Y])):-
	make_equal(ex("cod",[ex("proj2",[X,Y])]),Y),fail.
basic_equalities(ex("prod_ob",[X,Y])):-
	make_equal(ex("dom",[ex("proj1",[X,Y])]),ex("prod_ob",[X,Y])),fail.
basic_equalities(ex("prod_ob",[X,Y])):-
	make_equal(ex("dom",[ex("proj2",[X,Y])]),ex("prod_ob",[X,Y])),fail.


	basic_equalities(ex(coproj1,[X,Y])):-
	        make_equal(ex(dom,[ex(coproj1,[X,Y])]),X),fail.
	basic_equalities(ex(coproj1,[X,Y])):-
		make_equal(ex(dom,[ex(coproj2,[X,Y])]),Y),fail.
	basic_equalities(ex(coproj1,[X,Y])):-
		make_equal(ex(cod,[ex(coproj1,[X,Y])]),ex(coprod_ob,[X,Y])),fail.
	basic_equalities(ex(coproj1,[X,Y])):-
		make_equal(ex(cod,[ex(coproj2,[X,Y])]),ex(coprod_ob,[X,Y])),fail.


	basic_equalities(ex(coproj2,[X,Y])):-
	        make_equal(ex(dom,[ex(coproj1,[X,Y])]),X),fail.
	basic_equalities(ex(coproj2,[X,Y])):-
		make_equal(ex(dom,[ex(coproj2,[X,Y])]),Y),fail.
	basic_equalities(ex(coproj2,[X,Y])):-
		make_equal(ex(cod,[ex(coproj1,[X,Y])]),ex(coprod_ob,[X,Y])),fail.
	basic_equalities(ex(coproj2,[X,Y])):-
		make_equal(ex(cod,[ex(coproj2,[X,Y])]),ex(coprod_ob,[X,Y])),fail.


	basic_equalities(ex(coprod_mor,[F,G])):-
	        make_equal(
	        ex(cod,[ex(coprod_mor,[F,G])]),ex(cod,[F])
	        ),fail.
	basic_equalities(ex(coprod_mor,[F,G])):-		
		make_equal(
		ex(coprod_ob,[ex(dom,[F]),ex(dom,[G])]),ex(dom,[ex(coprod_mor,[F,G])])
		),fail.
	basic_equalities(ex(coprod_mor,[F,G])):-		
		make_equal(
		ex(com,[ex(coprod_mor,[F,G]),ex(coproj1,[ex(dom,[F]),ex(dom,[G])])]),F
		),fail.
	basic_equalities(ex(coprod_mor,[F,G])):-		
		make_equal(
		ex(com,[ex(coprod_mor,[F,G]),ex(coproj2,[ex(dom,[F]),ex(dom,[G])])]),F
		),fail.


	basic_equalities(ex(coprod_ob,[X,Y])):-
	        make_equal(ex(dom,[ex(coproj1,[X,Y])]),X),fail.
	basic_equalities(ex(coprod_ob,[X,Y])):-
		make_equal(ex(dom,[ex(coproj2,[X,Y])]),Y),fail.
	basic_equalities(ex(coprod_ob,[X,Y])):-
		make_equal(ex(cod,[ex(coproj1,[X,Y])]),ex(coprod_ob,[X,Y])),fail.
	basic_equalities(ex(coprod_ob,[X,Y])):-
		make_equal(ex(cod,[ex(coproj2,[X,Y])]),ex(coprod_ob,[X,Y])),fail.

		
	basic_equalities(ex(in_eq,[F1,F2,F])):-
	         make_equal(
	         ex(dom,[ex(in_eq,[F1,F2,F])]),
	         ex(dom,[F])
	         ),fail.
	basic_equalities(ex(in_eq,[F1,F2,F])):-	
	         make_equal(
	         ex(cod,[ex(in_eq,[F1,F2,F])]),
	         ex(eq,[F1,F2])
	         ),fail.
	basic_equalities(ex(in_eq,[F1,F2,F])):-	
	         make_equal(
	         ex(com,[ex(mor_eq,[F1,F2]),
	         ex(in_eq,[F1,F2,F])]),F
	         ),fail.	
			

	basic_equalities(ex(eq,[F1,F2])):-
	         calc(ex(mor_eq,[F1,F2]),_),fail.


	basic_equalities(ex(mor_eq,[F1,F2])):-
	         make_equal(ex(dom,[ex(mor_eq,[F1,F2])]),ex(eq,[F1,F2])),fail.
	basic_equalities(ex(mor_eq,[F1,F2])):-
	         make_equal(ex(cod,[ex(mor_eq,[F1,F2])]),ex(dom,[F1])),fail.
	basic_equalities(ex(mor_eq,[F1,F2])):-
	         make_equal(
	                    ex(com,[F1,ex(mor_eq,[F1,F2])]),
	                    ex(com,[F2,ex(mor_eq,[F1,F2])])
	                    ),fail.

	basic_equalities(ex(ob_mor,[T1,T2])):-
	         calc(ex(ev,[T1,T2]),_),fail.


	basic_equalities(ex(ev,[T1,T2])):-
		make_equal(
		ex(dom,[ex(ev,[T1,T2])]),ex(prod_ob,[ex(ob_mor,[T1,T2]),T1])
		),fail.
	basic_equalities(ex(ev,[T1,T2])):-
		make_equal(ex(cod,[ex(ev,[T1,T2])]),T2),fail.


	basic_equalities(ex(lambda,[G,T,T1])):-
		make_equal(
		ex(dom,[ex(lambda,[G,T,T1])]),T
		),fail.
	basic_equalities(ex(lambda,[G,T,T1])):-
		make_equal(
		ex(cod,[ex(lambda,[G,T,T1])]),ex(ob_mor,[T1,ex(cod,[G])])
		),fail.
	basic_equalities(ex(lambda,[G,T,T1])):-
		make_equal(
		ex(com,[ex(ev,[T1,ex(cod,[G])]),ex(prod_mor,[ex(com,[ex(lambda,[G,T,T1]),ex(proj1,[T,T1])]),ex(proj2,[T,T1])])]),G
		),fail.
	
 

basic_equalities(ex(omega,[])):-
                calc(ex(true,[]),_),fail.


basic_equalities(ex(true,[])):-
                make_morphism(
                ex(final,[]),ex(true,[]),ex(omega,[])
                ),fail.


basic_equalities(ex(char_f,[F])):-
                make_morphism(
                ex(cod,[F]),ex(char_f,[F]),ex(omega,[])
                ),fail.
basic_equalities(ex(char_f,[F])):-
                make_equal(
                    ex(com,[ex(true,[]),ex(fin,[ex(dom,[F])])]),
		    ex(com,[ex(char_f,[F]),F])
		),fail.
basic_equalities(ex(char_f,[F])):-
                calc(ex(ver_dom,[ex(char_f,[F])]),_),fail.

		   
basic_equalities(ex(ver_dom,[G])):-
                make_equal(
                ex(ver_dom,[G]),
                ex(eq,[G,ex(com,[ex(true,[]),ex(fin,[ex(dom,[G])])])])
                ),fail.
basic_equalities(ex(ver_dom,[G])):-
                make_inclusion(
                ex(ver_dom,[G]),
                ex(mor_eq,[G,ex(com,[ex(true,[]),ex(fin,[ex(dom,[G])])])]),
                ex(dom,[G])
                ),fail.
basic_equalities(ex(ver_dom,[G])):-
                make_equal(
                ex(char_f,[ex(inclusion,[ex(ver_dom,[G]),ex(cod,[G])])])
                ,
                G
                ),fail.
basic_equalities(ex(ver_dom,[G])):-
                make_equal(
                ex(isom_ver_dom,[ex(inclusion,[ex(ver_dom,[G]),ex(cod,[G])])]),
                ex(id,[ex(ver_dom,[G])])
                ),fail.


basic_equalities(ex(isom_ver_dom,[F])):-
                make_morphism(
  /* вместо     ex(ver_dom,[ex(char_f,[F])]),   */
                ex(eq,[ex(char_f,[F]),ex(com,[ex(true,[]),ex(fin,[ex(dom,[ex(char_f,[F])])])])]),
  /*    эта  строка по определению ex(ver-dom,[G]), где G = ex(char_f,[F]) */
                ex(isom_ver_dom,[F]),
                ex(dom,[F])
                ),fail.
basic_equalities(ex(isom_ver_dom,[F])):-
                make_equal(
                ex(com,[ex(fin,[ex(cod,[F])]),F])
                ,
                ex(fin,[ex(dom,[F])])
                ),fail.
basic_equalities(ex(isom_ver_dom,[F])):-
                make_equal(
		ex(com,[ex(in_eq,[ex(char_f,[F]),ex(com,[ex(true,[]),ex(fin,[ex(cod,[F])])]),F]),ex(isom_ver_dom,[F])])
		,
		ex(id,[ex(ver_dom,[ex(char_f,[F])])])
		),fail.
basic_equalities(ex(isom_ver_dom,[F])):-
                make_equal(
		ex(com,[ex(isom_ver_dom,[F]),ex(in_eq,[ex(char_f,[F]),ex(com,[ex(true,[]),ex(fin,[ex(cod,[F])])]),F])])
		,
		ex(id,[ex(dom,[F])])),fail.
		
basic_equalities(ex(inclusion,[A,B])):-
                make_equal(
                ex(dom,[ex(inclusion,[A,B])]),
                A),fail.
basic_equalities(ex(inclusion,[A,B])):-
                make_equal(
                ex(cod,[ex(inclusion,[A,B])]),
                B),fail. 

basic_equalities(ex(image,[F,B])):- /*образ пустого - пусто */
                is_equal_not_message(B,ex(initial,[])),!,
                make_equal(
                ex(image,[F,B]),
                ex(initial,[])
                ).
basic_equalities(ex(image,[F,B])):- /*образ тождественного морфизма*/
                is_equal_not_message(F,ex(id,[ex(dom,[F])])),!,
                make_equal(
                ex(image,[F,B]),
                B).
basic_equalities(ex(image,[F,B])):- /*образ  вложения*/
                is_equal_not_message(F,
                          ex(inclusion,[ex(dom,[F]),ex(cod,[F])])),!,
                make_equal(
                ex(image,[F,B]),
                B).                
basic_equalities(ex(image,[F,B])):- /*образ - подобъект области значений*/
             make_subobject(ex(image,[F,B]),ex(cod,[F])),
             fail.
basic_equalities(ex(image,[F,Elm])):- /*образ элемента - 
                                                  элемент области значений*/
             isomor_with_point(Elm),!,
             make_image_element(F,Elm).             
basic_equalities(ex(image,[F,B])):- /*образ больших - больше */
                calc_on_approx(ex(image,[F,B]),ImB),
                calc(ex(dom,[F]),Desc_domF),
                calc(B,DescB),
                findall(Sub,between(DescB,Desc_domF,Sub),List),
                list_not_repeat(List,ListSub),
                assert_img_big(F,ImB,ListSub),
                fail.
basic_equalities(ex(image,[F,B])):- /*образ меньших - меньше */ 
                calc_on_approx(ex(image,[F,B]),ImB),
                calc_on_approx(B,DescB),
                calc(ex(initial,[]),Nil),
                findall(Sub,between(Nil,DescB,Sub),List),
                list_not_repeat(List,ListSub),
                assert_img_litl(F,ImB,ListSub),
                fail.

basic_equalities(ex(name_ob,[Ob])):-
                calc(Ob,DescOb),
                resalt_op(_,ex(denote_ob,[X]),DescOb),
                make_equal(X,ex(name_ob,[DescOb])),
                fail.
basic_equalities(ex(name_ob,[Ob])):-
                make_element(ex(name_ob,[Ob]),ex(names_of_objects,[])),
                fail.
basic_equalities(ex(name_mor,[Mor])):-
                calc(Mor,DescMor),
                resalt_op(_,ex(denote_mor,[X]),DescMor),
                make_equal(X,ex(name_mor,[DescMor])),
                fail.
basic_equalities(ex(name_mor,[Mor])):-
                make_element(ex(name_mor,[Mor]),ex(names_of_morphisms,[])),
                fail.
basic_equalities(ex(denote_ob,[Name])):-
                calc(Name,DescName),
                resalt_op(_,ex(name_ob,[X]),DescName),
                make_equal(X,ex(denote_ob,[Name])),
                fail.
basic_equalities(ex(denote_mor,[Name])):-
                calc(Name,DescName),
                resalt_op(_,ex(name_mor,[X]),DescName),
                make_equal(X,ex(denote_mor,[Name])),
                fail.

basic_equalities(ex(disting_mor,[Attr,Entite])):-
                make_equal(ex(dom,[ex(disting_mor,[Attr,Entite])]),Entite),
                fail.
basic_equalities(ex(disting_mor,[Attr,Entite])):-
                make_equal(ex(cod,[ex(disting_mor,[Attr,Entite])]),Attr),
                fail.

basic_equalities(_).

 make_image_element(F,Elm):-
             calc(Elm,DescElm),
             element(_,DescElm,Ob),
             calc_on_approx(ex(image,[F,Ob]),ImOb),
             make_element(ex(image,[F,DescElm]),ImOb),
             fail.
 make_image_element(_,_).
 
 assert_img_big(_,_,[]):-!.
 assert_img_big(F,ImB,[Sub|T]):-
             if_in_approx_msub_big(F,Sub,ImB),
             assert_img_big(F,ImB,T).

 if_in_approx_msub_big(F,Sub,ImB):-
             calc_on_approx(ex(image,[F,Sub]),ImSub),
             make_subobject(ImB,ImSub),!.
 if_in_approx_msub_big(_,_,_).
 
 assert_img_litl(_,_,[]):-!.
 assert_img_litl(F,ImB,[Sub|T]):-
             if_in_approx_msub_litl(F,Sub,ImB),
             assert_img_litl(F,ImB,T).
            
 if_in_approx_msub_litl(F,Sub,ImB):-
             calc_on_approx(ex(image,[F,Sub]),ImSub),
             make_subobject(ImSub,ImB),!.
 if_in_approx_msub_litl(_,_,_).             
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%555 
