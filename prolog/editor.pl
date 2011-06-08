/*****************************************************************************

     Copyright (c)  2010  RSUH, 
                 Cafedra of Mathematics, Logic and Intellectual Systems
      

 Project:  WinEzop
 FileName: EDITOR.PRO
 Purpose: Editor
 Written by:  Eugeny Beniaminov 
 Comments: EZOP  is the system for  knowledge represntation 
******************************************************************************/

:- module(editor,[
    get_in_template/8
  , get_template/7
  , msg_n/4
  , msg/2
  , dlgAsk_call/2
  , dlgAsk_call_eq/1
  , get_current_environment_file/1
  , get_current_concept_name/1
  , get_current_environment_name/1
  , error_handler/2
  , write_added_templates/0
  , change/2
  , send_templates_list/2
  , run/1
  , notion_base/1
  , environment/1
  , send_to_debug/1
  , get_paramVal/2
]).

:- style_check(+string).
:- set_prolog_flag(unknown, fail).

:- use_module(vp52_compat).
:- use_module(htmlpages).
:- use_module(form).
:- use_module(calc).
:- use_module(dbctrl).
:- use_module(dic_tree).
:- use_module(make_eq).
:- use_module(db).

:- op(700, xfx, user:(=)).

:- dynamic 
   notion_base/1,
   environment/1.

append([],L,L):-!.
append([H|T1],L,[H|T2]):-append(T1,L,T2).

dlgAsk_call_eq(_):-
  dialog_answer("yes"),!.
dlgAsk_call_eq(_):-
  dialog_answer("no"),!,
  write("NO"),
  fail.
dlgAsk_call_eq(Str):-!,
  dlgAsk_eq(Str),
  exit.

dlgAsk_call(_,"true"):-
  dialog_answer("yes"),!.
dlgAsk_call(Str, "false"):-!,
  dlgAsk(Str),
  exit.

get_list_concept_env([1,ID_env|T]):-
  get_current_environment_id(ID_env),
  findall(IdUsedConcept ,use(_OforUseOr1forParent,ID_env,IdUsedConcept),T).

get_dic_info(Type,Text,Comment,List,Type2):-
  free(Type),
  get_template(_,_,Text,_Opname,Comment,List,Type1),
  calc_on_approx(Type1,Type2),
  Type=Type2.
get_dic_info(Type,Text,Comment,List,Type2):-
  bound(Type),
  get_template(_,_,Text,_Opname,Comment,List,Type1),
  is_subobject(Type1,Type),
  calc_on_approx(Type1,Type2).
get_dic_info(r(1),Text,Comment,List,Type):-
  send_to_debug("Вызван get_dic_info c r(1)"),
  %write("3"),
     get_template(_,_,Text,_Opname,Comment,List,Type1),
     calc_on_approx(Type1,Type).
get_dic_info(r(2),Text,Comment,List,Type):-
  send_to_debug("Вызван get_dic_info c r(2)"),
  %write("4"),
     get_list_concept_env(ListConc),
     %write(ListConc),
     get_in_template(_,Id_Conc,_Descr,Text,_Opname,Comment,List,Type1),
     not(member(Id_Conc,ListConc)),
     %write("Template Text: ", Text),
     %write("Type1: ", Type1), 21.05.08
     calc_on_approx(Type1,Type).
get_dic_info(r(3),Text,Comment,List,ex("ob",[])):-
  %write("5"),
    get_template(_,_Descr,Text,_Opname,Comment,List,ex("ob",[])).
get_dic_info(r(3),Text,Comment,List,ex("mor",[])):-
  %write("6"),
    get_template(_,_Descr,Text,_Opname,Comment,List,ex("mor",[])).
get_dic_info(r(4),Text,Comment,List,Type):-
  %write("7"),
     get_in_template(_,_,_Descr,Text,_Opname,Comment,List,Type1),
    calc_on_approx(Type1,Type).
get_dic_info(r(5),Text,Comment,List,ex("ob",[])):-
  %write("8"),
     get_in_template(_,_,_Descr,Text,_Opname,Comment,List,ex("ob",[])).
  
   
list_to_invert_str([],""):-!.
list_to_invert_str([H|T],StrDir):-
  concat("\\",H,StrH),
  list_to_invert_str(T,StrT),
  concat(StrT,StrH,StrDir).

list_w_v_to_str_var([], ""):-!.
list_w_v_to_str_var([w(_)|T], StrArg):-!,
  list_w_v_to_str_var(T, StrArg).
list_w_v_to_str_var([var(Expr)|T], StrArg):-
  list_w_v_to_str_var(T, StrT),
  StrT="",!,
  expr_to_str(Expr, StrArg).  
list_w_v_to_str_var([var(Expr)|T], StrArg):-!,
  expr_to_str(Expr, StrEx),
  list_w_v_to_str_var(T, StrT),
  format(StrArg,"%, %",StrEx, StrT).

get_in_template( IdDostup_0_1_2,  ID_Concept,Id_template, Text,  Opname, Comment, List, TypeRes):-
  in_template( IdDostup_0_1_2,  ID_Concept,Id_template, Text,  Opname, Comment, List, TypeRes),
  /*
  write("In Template\n"),
  write("IdDostup_0_1_2: ",IdDostup_0_1_2,"\n"),
  write("ID_Concept: ",ID_Concept,"\n"),
  write("Id_template: ",Id_template,"\n"),
  write("Text:",Text,"\n"),
  write("End In Template\n"),
  */
  not(del_template(_,Opname, List, TypeRes)).

get_template( ID_Concept,Id_template, Text,  Opname, Comment, List, TypeRes):-
  template( ID_Concept,Id_template, Text,  Opname, Comment, List, TypeRes),
  concept_visible(ID_Concept),
  %write("template: ", Text),
  not(del_template(_,Opname, List, TypeRes)).

del_element(_,[],[]):-!.
del_element(S,[S|R],R):-!.
del_element(S,[H|R1],[H|R]):-
  del_element(S,R1,R).

member(H,[H|_]):-!.
member(H,[_|T]):-
  member(H,T).

concept_visible(ID_Concept):-
  idconcept(ID_Concept),!.
concept_visible(ID_Concept):-
  used_concept(_,_,ID_Concept),!.
concept_visible(ID_Concept):-
  str_int(Str_ID_Concept,ID_Concept),
  get_environment_id(Str_ID_Concept,Str_Id_Env),
  Str_Id_Env="1",!.
concept_visible(ID_Concept):-
  str_int(Str_ID_Concept,ID_Concept),
  get_environment_id(Str_ID_Concept,Str_Id_Env),
  str_int(Str_Id_Env,Id_Env),
  used_concept(_,_,Id_Env),!.
  
get_environment_id(ID_Concept,Id_Env):-
  name_file(_,ID_Concept,Id_Env, _),!.
  
  
get_envFile_forConcept("Новая онтология",File):-
  environment(File),!.
get_envFile_forConcept("Новая онтология","1"):-!. % Ядро онтологий из системной папки
get_envFile_forConcept(Name,Env):-
  name_file(Name,_,Env,_),!.

get_current_environment_file(S):-environment(S),!.
get_current_environment_file("1").


get_current_environment_name("Ядро системы"):-
    get_current_environment_file(File),
    upper_lower(File,"1"),!.
get_current_environment_name(S):-
    get_current_environment_file(File),
    name_file(S,File,_,_),!.
get_current_environment_name("Ядро системы").


get_current_concept_name(S):-
  current_concept_name(S),!.
get_current_concept_name("Новая онтология").


load_concept_view(Func,UserName):-
     text_to_execute("tnotion", Defin1),!,
     text_to_execute("rest", Defin2),!,
    text_to_execute("command",Cmd),!,
    curCnptPage(Defin1, Defin2, Cmd,Func,UserName).  
    
load_concept_readonly(Func,UserName):-
     text_to_execute("tnotion", Defin1),!,
     text_to_execute("rest", Defin2),!,
    text_to_execute("command",Cmd),!,
    curCnptReadonly(Defin1, Defin2, Cmd,Func,UserName).  
%load_draftform(Defin1,Defin2,Cmd,Func):-
  %save_draft_form(Defin1, Defin2, Cmd,Func). 


load_concept(UserName):-
  %write("2"),
     retractall(fatal_error),
     %Очистить все окна текущей онтологии, но не имя онтологии. 
     %Изменить текст в tnotion_window и edit_window
     get_current_concept_name(Name),
     name_file(Name,File,_,_),
     not(upper_lower(File,"1")),
     notion_base(CatalogFile),
     filenamepath(CatalogFile,  Path, _Name),
     filenamepath(PathFile,  Path,File),
     not(existfile(PathFile)),!,
      %write("File doesn't exist"),  
  format(Msg,"Не найден файл \"%\", онтологии \"%\".",PathFile,Name),
  fatalErrorMsg(Msg),
     %msg_n(err,63,[PathFile,Name,Name],b_true),
     retractall(fatal_error),
  assert(fatal_error).
    %load_env_concept.        
     %test_inconsistence.
 load_concept(UserName):-
   %write("3"),
     get_current_concept_name(Name),
     name_file(Name,File,_,_),
     not(upper_lower(File,"1")),
     notion_base(CatalogFile),
  filenamepath(CatalogFile,  Path, _Name),
  filenamepath(PathFile,  Path,File),!,
  %write(_PathFile),
  consult(PathFile,concept),!,
     %trap(consult(PathFile,concept),Err, error_msg(PathFile,Err)),
     !.   
 load_concept(UserName):-        % - загрузка ядра!
  %write("4"),
     get_current_concept_name(Name),
     name_file(Name,File,_,_),
     upper_lower(File,"1"),
     consult("1",concept),!,
     %trap(consult("1",concept),Err, error_msg("1",Err)),
  load_concept_view("",UserName).   
 load_concept(UserName):-
   %write("5"),
   fatal_error.
   %msg_n(warning,64,[],b_true).
 /*
 
 test_inconsistence:- %написать предикаты set_concept_w..._error и восстановить
  inconsist(_,_,_),!,
  set_concept_with_error.
test_inconsistence:-
  set_concept_without_error.
*/    
  
set_tnotion(Name,UserName) :-
  %UNCOMMENT
  %check_concept_change, 
  % verifying if there were changes in the concept, commands, etc.
  get_envFile_forConcept(Name,Env),  % setting environment name for a new concept
  retractall(environment(_)),
  assert(environment(Env)),  % asserting new environment file
  get_current_environment_name(NameEnv),      % getting new environment name
  set_environment_name(NameEnv),     %changing windows' titles
   set_current_concept_name(Name), 
   load_concept(UserName).
  
  
get_name_file(Name, File) :-
  name_file(Name,File, _, _),!.  
  
modified_set_tnotion(Name,UserName):-
  get_envFile_forConcept(Name,Env),% setting environment name for a new concept
  retractall(environment(_)),
  assert(environment(Env)),% asserting new environment file
  get_current_environment_name(NameEnv), % getting new environment name
  set_environment_name(NameEnv), %changing windows' titles
   set_current_concept_name(Name),
   modified_load_concept(UserName).
 /*  
 modified_load_concept(UserName):-
  get_current_concept_name(Name),
  Name="Новая онтология",!,
  % case of a new concept
   %clear_concept,  %Очистить все окна текущей онтологии, но не имя онтологии. %Изменить текст в tnotion_window и edit_window
    load_env_concept, 
    %send_to_debug("load_env_concept"),        
    get_newIdConcept(Id),
    %send_to_debug("get_newIdConcept"),
    retractall(idconcept(_)),
    %send_to_debug("retractall"),
    assert(idconcept(Id)),
    send_to_debug("assert"),
   %ID среды в использованные онтологии
    get_current_environment_id(N),
    send_to_debug("get_current_environment_id"),
   assert(used_concept(0,Id,N)).
   %curCnptPage("","").
     %test_inconsistence.
 */

 
 modified_load_concept(UserName):-
   retractall(fatal_error),
     %clear_concept, %Очистить все окна текущей онтологии, но не имя онтологии. %Изменить текст в tnotion_window и edit_window
     get_current_concept_name(Name),
     %name_file(Name,File,_,_),
     get_name_file(Name,File),
     not(upper_lower(File,"1")),
     notion_base(CatalogFile),
     filenamepath(CatalogFile,  Path, _Name),
     filenamepath(PathFile,  Path,File),
     not(existfile(PathFile)),!,
      write("File doesn't exist"),  
   %format(Msg,"Не найден файл \"%\", онтологии \"%\".\nСтроится пустая онтология c именем \"%\".",PathFile,Name,Name),
     %msg_n(err,63,[PathFile,Name,Name],b_true),
     retractall(fatal_error),
  assert(fatal_error),
    load_env_concept.        
     %test_inconsistence.


 modified_load_concept(UserName):-
   get_current_concept_name(Name),
     name_file(Name,File,_,_),!,
     %not(upper_lower(File,"init.ntn")),
     not(upper_lower(File,"1")),
     notion_base(CatalogFile),
     filenamepath(CatalogFile,  Path, _Name),
  filenamepath(PathFile,  Path,File),!,
  consult(PathFile,concept),!.
     %trap(consult(PathFile,concept),Err, error_msg(PathFile,Err)).
  %load_concept_view,!.   

 modified_load_concept(UserName):-
   get_current_concept_name(Name),
     name_file(Name,File,_,_),
     upper_lower(File,"1"),
     consult("1",concept),!,
     %trap(consult("1",concept),Err, error_msg("1",Err)),
  load_concept_view("",UserName).   

 modified_load_concept(UserName):-
   send_to_debug("modified_load_concept - 5 "),
   fatal_error.
   
   
my_format(S,S,[]):-!.
my_format(S_res,S_in,[H|T]):-
  searchstring(S_in,"%",N),!,

  N1 is  N - 1,
  
  frontstr(N1,S_in,Begin,End1),!,
  frontstr(1,End1,_,End2),!,
  my_format(End,End2,T),
  concat(Begin,H,BeginH),
  concat(BeginH,End,S_res).
my_format(S,S,_).

get_msg_text(N,Msg_text):-
  msg_text(N,Msg_text),!.
get_msg_text(N,_):-
  str_int(Str_N,N),
  format(Str,"Сообщение номер % отсутствует!", Str_N),
  fatalErrorMsg(Str), 
  fail.
/*  
next_filename(FileName) :-
  next_file_name("F","ntn",FileName).



next_file_name(Pre,Post,FileName) :-  
  retract(count(N,Pre,Post)),
  N < 10000000,!,
  %disk(Path),
  format(FileName0,"%s%u.%s",Pre,N,Post),
ifndef iso_prolog  
  N1 = N + 1,
elsedef
  N1 is N + 1,
enddef  
  assert(count(N1,Pre,Post)),
  next_filename(FileName0,FileName).
  next_file_name(Pre,Post,FileName) :-
  assert(count(1,Pre,Post)),
  next_file_name(Pre,Post,FileName).

  next_filename(FileName0,FileName1) :-
  existfile(FileName0),!,
  next_filename(FileName1).
  next_filename(FileName,FileName).
*/  
 

msg_n(_Type,N,Vars,b_true):-
  get_msg_text(N,Text),
  my_format(Str,Text,Vars),
  %*******FATAL ERROR************
  assert(my_message(Str)),
  fail.
msg_n(Type,_N,_,_):-
  tmessage(Type,_,"off"),!.
  
msg_n(Type,N,Vars,_):-
  tmessage(Type,Text,"on"),
  get_msg_text(N,Msg_text),
  my_format(Str,Msg_text,Vars),!,
  format(Prompt,"% >> %\n",Text,Str),
  assert(my_message(Prompt)).
  
msg_n(Type,N,Vars,_):-
  not(tmessage(Type,_,_)),
  get_msg_text(N,Msg_text),
  my_format(Str,Msg_text,Vars),!,
  concat(Str,"\n",Prompt),
  assert(my_message(Prompt)).
msg_n(_Type,_N,_Vars,_).



msg(Type,_):-
  tmessage(Type,_,"off"),!.
msg(Type,Str):-
  tmessage(Type,Text,"on"),!,
  format(Prompt,"% >> %\n",Text,Str),
  assert(my_message(Prompt)).
msg(_Type,Str):-
  concat(Str,"\n",Prompt),
  assert(my_message(Prompt)).
  

change_templates:-
  retractall(_,dic_templates),
  get_dicTemplates,
  idconcept(IdConcept),
  retractall(dic(template(IdConcept,_,_,_,_,_,_))), 
  insert_templates(IdConcept),
  save_dicTemplates,
  retractall(_,dic_templates).

changeIdInConcept(IdOld,IdNew):-
  str_int(Old,IdOld),
  str_int(New,IdNew),
  changeStrInConcept(Old,New),
  fail.
changeIdInConcept(_,IdNew):-
  not(idconcept(IdNew)),
  fatalErrorMsg( "Внутреннее представление онтологии испорчено!!!<BR>Постройте онтологию заново по тексту."),
  retractall(fatal_error),
  assert(fatal_error),
  assert(idconcept(IdNew)),
  fail.  
changeIdInConcept(_,_).

changeStrInConcept(_,_):-
  retractall(_,tmp_concept),
  fail.
changeStrInConcept(Old,New):-
  retract(Term,concept),
  term_str(concept, Term,StrTerm),
  replaceStr_all(Old,New,StrTerm,NewStrTerm),
  term_str(concept, NewTerm, NewStrTerm),
  %trap(term_str(concept, NewTerm, NewStrTerm),_,
%    error_msg( "Ошибка при замене во внутреннем представлении онтологии!!!\nПостройте онтологию заново по тексту.",0)),
  assert(t(NewTerm), tmp_concept),
  fail.
changeStrInConcept(_,_):-
  retract(t(Term),tmp_concept),
  assert(Term,concept),
  fail.  
changeStrInConcept(_,_).

ifdel_ver(Fname):-
  inset(Inset),
  send_to_debug(Inset),
  not(inset("delete_draft")),!,
  send_to_debug("inset not deletedraft"),
  del_ver(Fname).
ifdel_ver(_Fname):-
  send_to_debug("inset deletedraft"),!.
  
del_ver(Fname):-
  ver(Fname,V),
  not(Fname=V),!,
  fatalErrorMsg("Эта онтология является главной версией. Для удаления этой онтологии необходимо выбрать другую главную версию."),fail.
  %retractall(fatal_error),
  %assert(fatal_error).
del_ver(Fname):-
  retractall(ver(_,Fname)),
  retractall(ver(Fname,_)).
    

delete_notion_for_id(Fname):-
  str_int(Fname,Id),
  not(use(_,_,Id)),!,
  name_file(Name,Fname,_,_),!,
  str_int(Fname,Id),
  retractall(name_file(_,Fname,_,_)),
  retractall(use(_,Id,_)),
  retractall(dir_c(_,Name)),
  ifdel_ver(Fname),!,
  notion_base(CatalogFile),
  filenamepath(CatalogFile,  Path, _Name),
  filenamepath(PathFile,  Path,Fname),  
  deletefile(PathFile),
  save_names,
  delete_templates(Id),
  send_to_debug("Go to send_afterdel"),
  send_afterdel.  
  %format(Msg,"Удаление  онтологии \"%\" произведено.",Name),
  %dlgMsg(Msg).%Это нужно сообщать после выполнения в друпал
delete_notion_for_id(_):-
  fatalErrorMsg("Онтология используется. Удаление невозможно!!!"),
  fail.
    
  
  
 delete_templates(Id):-
  retractall(_,dic_templates),
  get_dicTemplates,
  retract(dic(template(Id,_,_,_,_,_,_))),
  fail
  ;
  save_dicTemplates,
  retractall(_,dic_templates).
 

get_dicTemplates:-
  notion_base(CatalogFile),
  concat(PathAndName,".ezp",CatalogFile),
  concat(PathAndName,".tmt",File),
    not(existfile(File)),
    copyfile("nilfile.tmt",File),
     format(Str,"Отсутствовал файл \"%\", содержащий шаблоны базы онтологий. Создан пустой  файл",File),
     fatalErrorMsg(Str),
     msg_n(warning,67,[File],b_true),
   fail.
get_dicTemplates:-
     notion_base(CatalogFile),
  concat(PathAndName,".ezp",CatalogFile),
  concat(PathAndName,".tmt",FileTemplates),
     format(Prompt1," Невозможно открыть файл \"% \".",FileTemplates),
     %openread(input,FileTemplates),
     trap(openread(input,FileTemplates),Err1,error_handler(Prompt1,Err1)),
     readdevice(input),
     readterm_repeat(input),
     format(Prompt2," Невозможно прочитать терм в файле  \"% \".",FileTemplates),
     %readterm(templates,Term),
     trap(readterm(templates,Term),Err2,error_handler(Prompt2,Err2)),
     assert(dic(Term)),
     fail.
get_dicTemplates:-
  readdevice(keyboard),
  closefile(input).

  
 get_newFileConcept(File):-
   idconcept(IdOld),
   get_newIdConcept(IdNew),
   str_int(File,IdNew),
   not(existfile(File)),!,
   changeIdInConcept(IdOld,IdNew).
 get_newFileConcept(File):-
   get_newFileConcept(File).
   
      
   
   
 get_newFileConceptNewOnt(File):-
   get_newIdConcept(IdNew),
   str_int(File,IdNew),
   not(existfile(File)),!,
  retractall(idconcept(_)),
   assert(idconcept(IdNew)).
 get_newFileConceptNewOnt(File):-
   get_newFileConcept(File).  
   
   
 
insert_templates(IdConcept):-
  in_template(2, IdConcept, Id_template, Text,Opname,  Comment, List_templ, TypeRes),
  assert(dic(template( IdConcept, Id_template, Text,Opname,  Comment, List_templ, TypeRes))),
  %str_concat(IdConcept, Id_template, Text,Opname,  Comment, List_templ,TypeRes,OutPut),
  fail.
insert_templates(_).

%str_concat(IdConcept, Id_template, Text,Opname,  Comment, Список_шаблона,TypeRes,OutPut):-
  %Id_template_str=toString(TypeRes).


readterm_repeat(_).
readterm_repeat(File):-not(eof(File)),readterm_repeat(File).

save_dicTemplates:-
  notion_base(CatalogFile),
  concat(PathAndName,".ezp",CatalogFile),
  concat(PathAndName,".tmt",File),
    not(existfile(File)),
    copyfile("nilfile.tmt",File),
     format(Str," Отсутствовал файл \"%\", содержащий шаблоны базы онтологий. Создан пустой  файл",File),
     fatalErrorMsg(Str),
     msg_n(warning,67,[File],b_true),
     fail.
save_dicTemplates:-
     notion_base(CatalogFile),
  concat(PathAndName,".ezp",CatalogFile),
  concat(PathAndName,".tmt",FileTemplates),
  format(Prompt1," Невозможно открыть файл \"% \".",FileTemplates),
     trap(openwrite(input,FileTemplates),Err1,error_handler(Prompt1,Err1)),
     writedevice(input),
     dic(Term),
     not(Term=template(1,_,_,_,_,_,_)), %не сохраняются шаблоны ядра
     not(Term=dir_tt(1,_,_,_)), % убрал папки в словарях 14.03.2010
     not(Term=dir_dtt(1,_,_)),  % убрал папки в словарях 14.03.2010
     
     
     write_term(Term, [quoted(true)]), write("."),
     
     nl,      %- 15.5.08
     %templ_str(Term, InStr, OutStr),
      fail.
save_dicTemplates:-readdevice(keyboard),closefile(input).


/*templ_str(Term, InStr, OutStr):-
  Str=toString(Term),
  concat(InStr,Str,InStr),
  concat(InStr,",",OutStr),!.*/
  
check_concept_name_to_save(S):-
     not(fronttoken(S,_,_)),!,
     fatalErrorMsg("Имя онтологии не может быть пустым."),fail.
check_concept_name_to_save(S):-
  str_to_list(S,ListS),
  ListS=["новая","онтология"],!,
     fatalErrorMsg("Онтология с таким именем не может быть сохранена. Измените имя."),fail.
check_concept_name_to_save(S):-
    str_to_list(S,ListS),
  ListS=["ядро","системы"],!,
     fatalErrorMsg("Это имя используется системой."),fail.
check_concept_name_to_save(Name):-
  str_to_list(Name,ListName),
  name_file(NameF,_,_,_),
  upper_lower(NameF, LowerNameF),
  str_to_list(LowerNameF,ListNameF),
  ListName=ListNameF,!,
  fatalErrorMsg("Онтология с таким именем уже существует."),fail.
check_concept_name_to_save(_).  



str_to_list(Str,[]):-
  not(fronttoken(Str,_,_)),!.
str_to_list(Str,[H|T]):-
  fronttoken(Str,H,Rest),
  str_to_list(Rest,T).
  
  
save_concept:-
  curid(File),
     get_current_concept_name(ErrName),
     concat("ERROR**",Name,ErrName),!, %если в онтологии было противоречие, то имя начиналось с ERROR** 
      retract(name_file(Name,File,FEnv,_)),!,
      get_defin1(Text),
      assert(name_file(Name,File,FEnv,Text)),
     save_concept_in_file(Name,File),
     %% The three strings below were added.
     %set_tnotion(Name),
     %load_concept_view,
  changeInUse,!,
  save_names,!.
  %save_version,!.
save_concept:-
  curid(File),
     get_current_concept_name(Name),
      retract(name_file(Name,File,FEnv,_)),!,
      get_defin1(Text),
      send_to_debug(Text),
      assert(name_file(Name,File,FEnv,Text)),
     save_concept_in_file(Name,File),
     %% The three strings below were added.
     %%set_tnotion(Name),
     %load_concept_view,
  changeInUse,!,
  save_names,!.
  %save_version,!.

 
%save_version:- 
   
     
save_concept_zip(File):-
  notion_base(CatalogFile),
  filenamepath(CatalogFile,  Path, _Name),
  filenamepath(PathFile,  Path,File),
  save(PathFile,concept).
  %trap(save(PathFile,concept),_,save_concept_err(PathFile)).
  
changeInUse:-
   idconcept(Id),
   retractall(use(_,Id,_)),
   fail.
changeInUse:-
  idconcept(Id),
  used_concept(T,Id,IdUsed),
  IdUsed<>1,
  assert(use(T,Id,IdUsed)),
  fail.
changeInUse.

save_concept_in_file(Name,File):-
    change_templates,
    save_concept_zip(File),
      retractall(concept_changed),
      !,
   msg_n(did,73,[Name],b_false).

  
 checkForCurId(File):-
   curid(OldId),!,
     retractall(oldid(_)),
    assert(oldid(OldId)),
    retractall(curid(_)),
    assert(curid(File)).
 checkForCurId(File):-!,
  retractall(oldid(_)),
    assert(oldid(File)),
    retractall(curid(_)),
    assert(curid(File)).
      

   
rename_concept:-
  get_current_concept_name(Name),
     get_defin1(Text), 
     curid(Id),!,
     get_current_environment_file(FileEnv),
     
     new_cnpt_name(NewName),
     upper_lower(NewName, LowerName),
        check_concept_name_to_save(LowerName),!,       
     set_current_concept_name(NewName),    
     retractall(name_file(Name,_,_,_)),
     assert(name_file(NewName,Id,FileEnv,Text)),
     %assert(dir_c("Папки понятий",NewName)),
     %changeInUse,!,
      save_names.
  %checkForCurId(File),
      %load_concept_view.
rename_concept:-
  msg_n(err,74,[],b_true),
  fatalErrorMsg("Онтология не сохранена.").



save_concept_asNewOnt(File):-
     get_current_concept_name(Name),
     get_defin1(Text),
     send_to_debug("get_defin1"),
     send_to_debug(Text), 
     get_newFileConceptNewOnt(File),
     not(fatal_error),!,
     save_concept_in_file(Name,File),
     get_current_environment_file(FileEnv),
     assert(name_file(Name,File,FileEnv,Text)),
     changeInUse,!,
      save_names.
  %checkForCurId(File)
  /*,%comment from this in my version
  retractall(text_to_execute(_,_)),
   assert(text_to_execute("tnotion","")),
   assert(text_to_execute("command","")),
   assert(text_to_execute("rest","")),  
      load_concept_view*/
save_concept_asNewOnt(""):-
  msg_n(err,74,[],b_true),
  fatalErrorMsg("Онтология не сохранена.").




save_concept_as(UserName):-
     get_current_concept_name(Name),
     get_defin1(Text),
     get_newFileConcept(File),
     not(fatal_error),!,
     save_concept_in_file(Name,File),
     get_current_environment_file(FileEnv),
     assert(name_file(Name,File,FileEnv,Text)),
     assert(dir_c("Папки понятий",Name)),
     changeInUse,!,
      save_names,
      %***************************** 10.05.2008 - CHANGED!
      curid(OldId),
    retractall(oldid(_)),
    assert(oldid(OldId)),
    retractall(curid(_)),
    assert(curid(File)),
      %*****************************
      get_defin1(Text1),
   get_defin2(Text2),
   retractall(text_to_execute("tnotion",_)),
   retractall(text_to_execute("rest",_)),
   assert(text_to_execute("tnotion",Text1)),
   assert(text_to_execute("command","")),
   assert(text_to_execute("rest",Text2)),
      load_concept_view("",UserName).
save_concept_as(UserName):-
  msg_n(err,74,[],b_true),
  fatalErrorMsg("Понятие не сохранено.").

save_names:-
  %create_xml_file,
  notion_base(File),
% problem to port to unix. temporarily commented  
%  not(File="kernel.ezp"),
  %write(File),
  deletefile(File),
  save(File,concept_names),!,
  save("concept.ezp",concept). % это ОШИБКА, сохранил для отладки, что бы видеть концепт
  %trap(save(File,concept_names),_,save_names_err),!,  
save_names.  

state_convert("0","off"):-!.
state_convert("1","on"):-!.

convert_list([HList|TList],[StH|StT]):-
  state_convert(StH,HList),
  convert_list(TList,StT).
convert_list([],[]):-!.

save_config_list([TypeH|TypeT], [TextH|TextT], [StateH|StateT]):-
  state_convert(StateH, State),
  assert(tmessage(TypeH, TextH, State)),
  save_config_list(TypeT, TextT, StateT).
save_config_list([], [], []).

save_config:-
  retractall(_, config),
  typelist(TypeList),
  textlist(TextList),
  statelist(StateList),
  save_config_list(TypeList, TextList, StateList),
  save("config.ez", config).
/*************************************************************************/

load_catalog:-

   retractall(_,concept_names),
   %send_to_debug("1"),
   notion_base(File),
   %send_to_debug(File),
   concat("Ошибка в загрузке файла ",File,Str),
   trap(consult(File,concept_names),Err,error_handler(Str,Err)),!.
load_catalog:- 
  msg_n(warning,72,[],b_false).

  error_handler(S,E) :-
  format(STR,"% - Error code = % ",S,E),
  !,
  fatalErrorMsg(STR).
/***************************************************************************
  TO OPEN NOTION BASE  
***************************************************************************/  
convert_name("BaseExample2","BaseExample2\\Examples.ezp"):-!.
convert_name(ShortName, FullName):-!,
  concat(ShortName,"\\", Dir),
  concat(Dir,ShortName, PrePath),
  concat(PrePath,"s.ezp",FullName).
/*
open_notion_base(_FileName):-   
   concept_changed,!,
   check_concept_change,
   fail.
   */
open_notion_base(FileName):-
     assert(notion_base(FileName)),
     load_catalog.
     
     
 
/***************************************************************************
  
***************************************************************************/
assert_cmd_values_intocur :-
  get_paramVal("curcnpt_id",CurConceptId),
  get_paramVal("old_id",OldId),
  name_file(CurCnptName,CurConceptId,_,_),!, % найден CurConceptId онтологии
  retractall(oldid(_)),
   assert(oldid(OldId)),
  retractall(curid(_)),
   assert(curid(CurConceptId)),
   retractall(current_concept_name(_)),
   assert(current_concept_name(CurCnptName)),
   get_environmentName_forConceptName(CurCnptName,NameEnv), %-исправить для id!!!
  set_environment_name(NameEnv). % этот предикат загружает и шаблоны    
/*  
assert_cmd_values_intocur([_,parm("old_id",OldId),parm("curcnpt_name",CurCnptName_utf)|Rest2],Rest2) :- 
  %utf_8_to_ansi(CurCnptName_utf,CurCnptName),
  %cgi_DecodeString2(CurCnptName_utf, CurCnptName),
  name_file(CurCnptName,CurConceptId,_,_),!, % найдено имя онтологии, а CurConceptId не найден
  send_to_debug("Вызывается assert_cmd_values_intocur with CurCnptName"),
  retractall(oldid(_)),
   assert(oldid(OldId)),
  retractall(curid(_)),
   assert(curid(CurConceptId)),
   retractall(current_concept_name(_)),
   assert(current_concept_name(CurCnptName)),
   get_environmentName_forConceptName(CurCnptName,NameEnv), %-исправить для id!!!
  set_environment_name(NameEnv). % этот предикат загружает и шаблоны
*/  
assert_cmd_values_intocur :- !,
  get_paramVal("curcnpt_id",CurConceptId),
  get_paramVal("curcnpt_name",CurCnptName),
  format(Msg,"File % with ontology % not exists.", CurConceptId, CurCnptName),
  fatalErrorMsg(Msg),
  fail.
  

assert_cmd_values :-
  get_paramVal("curcnpt_id",CurConceptId),
        not(name_file(_,CurConceptId,_,_)),!,
  concat("Concept id file not exists ", CurConceptId, Msg),
  fatalErrorMsg(Msg),
  fail.  
assert_cmd_values :-
   get_paramVal("curcnpt_id",CurConceptId),
   get_paramVal("old_id",OldId),
   get_paramVal("curcnpt_name",_Name),
        send_to_debug(_Name),
        get_paramVal("env_id",_NameEnvId),
   get_paramVal("curcnpt_text",CurCnptText),
   get_paramVal("rest",RestT),
   get_paramVal("usedIds",_UsedIdsList),
   get_paramVal("inset", Inset),
   get_paramVal("cmd",Cmd),
   retractall(inset(_)),
   assert(inset(Inset)),
  %add_readonly(Mode),  
  %delete_r_char(CurCnptText, CurCnptTextR),
  %delete_r_char(Cmd, CmdR),
  %delete_r_char(RestT, RestTR),  
  retractall(oldid(_)),
   assert(oldid(OldId)),
  retractall(curid(_)),
   assert(curid(CurConceptId)),
   load_catalog,
   name_file(CurCnptName,CurConceptId,_,_),!, 
   retractall(current_concept_name(_)),
   assert(current_concept_name(CurCnptName)),
   get_environmentName_forConceptName(CurCnptName,NameEnv), %-исправить для id!!!
  set_environment_name(NameEnv),  % этот предикат изагружает шаблоны
  retractall(text_to_execute(_,_)),
  assert(text_to_execute("tnotion",CurCnptText)),
  assert(text_to_execute("command",Cmd)),
   assert(text_to_execute("rest",RestT)).

assert_save_values :-
  assert_cmd_values,
  get_paramVal("user",UserName),
   get_paramVal("new_cnpt_name", NewCnptName),
   get_paramVal("version",Ver),
   check_ver(Ver),  
   assert(new_cnpt_name(NewCnptName)),
   get_current_concept_name(CurCnptName),
    modified_set_tnotion(CurCnptName,UserName).
/*
check_name(OName, Name):-
   concat(" - [o] ",Name,OName),!.
check_name("","Новая онтология"):-!.
check_name(Name, Name):-!.
*/
check_ver(""):-!.
check_ver(Ver):-
  oldid(Old),
  curid(New),
  not(Old=New),
  ver(MainVer,Old),!,
  assert(ver(MainVer,New))
  ;
  oldid(Old),
  curid(New),
  not(Old=New),!,
  assert(ver(Old,New)).
check_ver(_).

stringToList("",[]):-!.
 stringToList(String, [StringArg|RestList]):-
  frontchar(String, Char, RestString),
  str_char(StringArg, Char),!,
  stringToList(RestString, RestList).  
 
/*******************************************************/ 
 get_dialog_answer :-
   parm("dlg_ask",Answer),!,
   retractall(dialog_answer(_)),
   assert(dialog_answer(Answer)).
 get_dialog_answer.
 
 msg_to_display :-
   parm("msg_to_display",MsgString),
   stringToList(MsgString, StateList),
   StateList = [_H|_],
   !,
   assert(statelist(StateList)),
   save_config.   
 msg_to_display :-
   parm("msg_to_display",_MsgString),!,
   findall(X, tmessage(_,_,X), List),
   convert_list(List, ListString),
   assert(statelist(ListString)).
 msg_to_display.
 
 choose_menu_item :-
   parm("menu_item",Item),!,
   filenamepath(Path, "BaseExample", "BaseExamples.ezp"),
  open_notion_base(Path),  
  assert(kbname(Path)),
  assert(location("http://localhost/drupal/exe/editor.exe")),
  
  %ERROR:***  location нужно задавать в goal, иначе этот текст потеряется и 
  %СИСТЕМА НЕ БУДЕТ РАБОТАТЬ при установке в другую папку или 
  %НА ВНЕШНЕМ СЕРВЕРЕ.  
  send_to_debug(Item),  
  main_menu(Item).
 choose_menu_item :-!,
        fatalErrorMsg("Не указан параметр меню.").
     
   
  
/*
 main_menu("KB_folders",ParmList, ParmList):-!,
  set_dir_con,
  set_dir_papki,
  refresh_concept_list.
 main_menu("KB_cnpt_tree",ParmList, ParmList):-!,    
  create_xml_file,
  treeCnptPage.
 main_menu("KB_open_another",ParmList, ParmList):-!,
   indexPage.
   %dlgAsk_sgl("<p>Все изменения, которые не были сохранены, будут утеряны! </p>  Вы уверены, что хотите закончить работу с этой Базой онтологий и открыть новую?</p> ").
*/
 main_menu("CC_disp"):-!,
   assert_cmd_values,
   get_paramVal("user",UserName),
   /*
   get_current_concept_name(CurCnptName),
    modified_set_tnotion(CurCnptName),
   idconcept(ID),
   text_to_execute("tnotion", Defin1),
    text_to_execute("rest", Defin2),
   retractall(defin1(_,_)),
   retractall(defin2(_,_)),
   assert(defin1(ID, Defin1)), 
   assert(defin2(ID, Defin2)), 
   */
   %get_current_concept_name(CurCnptName),
   load_concept(UserName).
 main_menu("CC_disp"):-!,
   fatalErrorMsg("Текущая онтология не установлена.").
 
 
 
 /**********************************************/ 
 
main_menu("cnpt_intoenv"):- % Делает онтологию средой
   get_paramVal("env_id", EnvId),
   get_paramVal("inset",Inset),
   retractall(inset(_)),
   assert(inset(Inset)),
   retractall(current_concept_name(_)),
   assert(current_concept_name("Новая онтология")),
   load_catalog,
  name_file(NameEnv,EnvId,_,_),!,             
   set_environment_name(NameEnv),  % этот предикат загружает и шаблоны
  assert(readonly_concept("false")),
   set_current_concept_name("Новая онтология"),
     get_paramVal("user",UserName),
     retractall(curid(_)),
   assert(curid("")),
   retractall(oldid(_)),
   assert(oldid("")),
   assert(text_to_execute("tnotion","")),
   assert(text_to_execute("command","")),
   assert(text_to_execute("rest","")),
   load_concept_view("",UserName).
     
main_menu("cnpt_intoenv"):-  !, % Выдает сообшение об ошибке
  fatalErrorMsg("Онтология среды не найдена").

  
 /*main_menu("cnpt_intocur", ParmList, RestParmList):-!, % Делает онтологию текущей
   assert_cmd_values_intocur(ParmList, RestParmList), % Устанавливает имя онтологии CurCnptName
                 % и загружает среду с шаблонами,
                 % и выдает сообщение об ошибке
   get_current_concept_name(CurCnptName),
   modified_set_tnotion(CurCnptName),
   %get_paramVal("user",User,ParmList),
     %retractall(user(_)),
   %assert(user(User)),
   get_defin1(Text1),
   get_defin2(Text2),
   retractall(text_to_execute(_,_)),
   assert(text_to_execute("tnotion",Text1)),
   assert(text_to_execute("command","")),
   assert(text_to_execute("rest",Text2)),
   load_concept_view. % онтология показывается в форме по значению в text_to_execute: Text1, Text2
 */
  main_menu("delete_ontology"):-
  get_paramVal("curcnpt_id",CurConceptId),
  CurConceptId="",!,
  get_paramVal("inset",Inset),
  retractall(inset(_)),
   assert(inset(Inset)), 
   send_afterdel.
  %fatalErrorMsg("Id пусто, онтология не найдена. Удаление НЕ произведено").  
  main_menu("delete_ontology"):-
  get_paramVal("curcnpt_id",CurConceptId),
  load_catalog,
  not(name_file(_,CurConceptId,_,_)),!,
  get_paramVal("inset",Inset),
  retractall(inset(_)),
   assert(inset(Inset)), 
   send_afterdel.
  %dlgMsg("Файл онтологии не существовал. Удаление онтологии произведено.").%Это нужно сообщать после выполнения в друпал
  main_menu("delete_ontology"):-!,
    get_paramVal("curcnpt_id",CurConceptId),
  get_paramVal("inset",Inset),
   retractall(inset(_)),
   assert(inset(Inset)),
   delete_notion_for_id(CurConceptId).
  
 /* main_menu("CC_edit", ParmList, RestParmList):-!,
   assert_cmd_values(ParmList, RestParmList),
   get_current_concept_name(CurCnptName),
   %get_paramVal("user",User,ParmList),
     %retractall(user(_)),
   %assert(user(User)),
   modified_set_tnotion(CurCnptName),    
     save_concept,
     %с проверкой непонятно понятно
     load_concept_view.*/

 main_menu("CC_editdraft"):-!,
   assert_cmd_values,
   get_current_concept_name(CurCnptName),
   split_rest,
   get_paramVal("user",UserName),
   %retractall(user(_)),
   %assert(user(User)),
   modified_set_tnotion(CurCnptName,UserName),  
   add_readonly("editing"),
   %text_to_execute("tnotion",Text1)),
  %text_to_execute("command",""),
   %text_to_execute("rest",Text2),  
     %save_concept,
     %с проверкой непонятно понятно
     load_concept_view("",UserName).  
     
     
  main_menu("CC_editNewVer"):-!,
    %send_to_debug("newver"),
   assert_cmd_values,
   get_paramVal("user",UserName),
   get_current_concept_name(CurCnptName),
   modified_set_tnotion(CurCnptName,UserName),
   makeTempNameMain(CurCnptName,1,ResName),         
  upper_lower(ResName, LowerName),
        check_concept_name_to_save(LowerName),
     set_current_concept_name(ResName),    
     save_concept_as(UserName).
     
      

 
/*************************************************/ 
   
 main_menu("CC_build_all"):-!,
    
    assert_cmd_values,% в предикате устанавливаются все параметры для выполнения
         % предиката execute - это CurCnptName и тексты text_to_execute
    get_paramVal("user",UserName),
    get_current_concept_name(CurCnptName),
    split_rest,
    modified_set_tnotion(CurCnptName,UserName),
    add_readonly("editing"),
   execute("tnotion"),
    %idconcept(File),!!!!!!!!!!!!!!
    get_paramVal("curcnpt_id",File),
    save_concept_zip(File),
    save_concept,
    % Нужно сохранить онтологию в своем файле с признаком ред,
    get_defin1(Text1),
    get_defin2(Text2),
    retractall(text_to_execute(_,_)),
   assert(text_to_execute("tnotion",Text1)),
  assert(text_to_execute("command","")),
   assert(text_to_execute("rest",Text2)),
   %get_paramVal("user",UserName),
   %retractall(user(_)),
   %assert(user(UserName)),
   %assert(ifbuilt_concept("true")),
   get_paramVal("version",Ver),
   retractall(version(_)),
   assert(version(Ver)),
   check_build(Text2),
   load_concept_view("",UserName).% Нужно  передать на сервер весь текст онтологии (черновик),
         % в форме отобразить режим редактирования и "построена", если строка 
         % в get_defin2(Text2) пустая
         

   
main_menu("CC_build_complete"):-!,
   main_menu("CC_build_all"). 

 
  main_menu("CC_save"):-!,
    savepredicate.
    
    
 
  

 main_menu("CC_rename"):-!,
   assert_save_values,
   get_paramVal("user",UserName),
   rename_concept,
   load_concept_view("",UserName).

 
 main_menu("CC_new_c_env"):-!,
   assert_cmd_values,
   get_paramVal("user",UserName),  
   set_tnotion("Новая онтология",UserName).
     
 main_menu("CC_new_c_krnl"):-!,
   assert_cmd_values,
   get_paramVal("user",UserName),
   set_tnotion("Новая онтология",UserName). 


main_menu("CC_newver"):-!,
   %assert_cmd_values(ParmList, RestParmList),
   get_paramVal("curcnpt_id",CurConceptId),   
   name_file(CurCnptName,CurConceptId,_,_),!, % найден CurConceptId онтологии
  retractall(current_concept_name(_)),
   assert(current_concept_name(CurCnptName)),
   get_environmentName_forConceptName(CurCnptName,NameEnv), %-исправить для id!!!
  set_environment_name(NameEnv), 
   %get_current_concept_name(CurCnptName),   
   get_paramVal("user",UserName),
   get_paramVal("inset",Inset),
   retractall(inset(_)),
   assert(inset(Inset)),
   modified_set_tnotion(CurCnptName,UserName),
   %curid(OldId),
    retractall(oldid(_)),
    %assert(oldid(OldId)),
    assert(oldid(CurConceptId)),
    retractall(curid(_)),
    assert(curid("")),
    curid(ID),
    send_to_debug(ID),
   %get_paramVal("curcnpt_id",CnptId),
   %str_int(CnptId,ConceptIdInt),
   %get_version(ConceptIdInt,Ver),
   %str_int(VerStr,Ver),
   %retractall(curid(_)),
   %assert(curid("")),
   add_readonly("editing"),
   retractall(version(_)),
   assert(version("")),
   get_defin1(Text1),
   retractall(text_to_execute(_,_)),
   assert(text_to_execute("tnotion",Text1)),
   assert(text_to_execute("command","")),
   assert(text_to_execute("rest","")),
   send_to_debug("before"),
     load_concept_view("",UserName).  


%main_menu("Main_ver", ParmList, ParmList):-!,
  %get_paramVal("curcnpt_id",CurConceptId),
   %retractall(curid(_)),
   %assert(curid(CurConceptId)),
   /*name_file(CurCnptName,CurConceptId,_,_),!, % найден CurConceptId онтологии
  retractall(current_concept_name(_)),
   assert(current_concept_name(CurCnptName)),
   get_environmentName_forConceptName(CurCnptName,NameEnv), 
  set_environment_name(NameEnv),*/ 
   
   
   /*get_paramVal("user",UserName),
   modified_set_tnotion(CurCnptName,UserName),*/
   
    %ver(Main,CurConceptId,Ver,_),
    %retractall(oldid(_)),
    %assert(oldid(Main)),
    %retractall(mainver(_)),
    %assert(mainver("1")),
    %get_paramVal("version",Ver),
   %check_ver(Ver),  
   %save_names,!.
   
main_menu("Main_ver"):-!,
  get_paramVal("curcnpt_id",CurConceptId),
   retractall(curid(_)),
   assert(curid(CurConceptId)), 
  get_paramVal("main_id",MainId),
  change_main(CurConceptId,MainId),  
  %retract(ver(MainId,ConceptId,N1,"1")),
  %assert(ver(MainId,ConceptId,N1,"0")),
  %retract(ver(MainId,CurConceptId,N2,_)),
  %assert(ver(MainId,CurConceptId,N2,"1")),
  save_names,!,
  after_changever.
/**/
/**********************************************/ 
 main_menu("CMD_run"):-!,
   assert_cmd_values,
   get_paramVal("user",UserName),
   get_current_concept_name(CurCnptName),
   retractall(text_to_execute("tnotion",_)),
   assert(text_to_execute("tnotion","")),
   assert(mode_answer),
   assert(answer_text_all("")),
   modified_set_tnotion(CurCnptName,UserName),
   execute("command"),
   %set_text_to_show,
   %findall(Names,get_dic_info(r(4),Names,_,_,_),A),
  findall(Names,get_template(_,_,Names,_,_,_,_),A),
   %get_paramVal("user",UserName),
   sort_temp(A,SortList),
   commandPage(SortList,UserName),!.
   %load_concept_view,!,
   %write_answer,!,
   %save_If_Editing.

 
   
 main_menu("CMD_last"):-!,
   assert_cmd_values,
   get_current_concept_name(CurCnptName),
   get_paramVal("user",UserName),
    modified_set_tnotion(CurCnptName,UserName),
    get_last_cmd(Str),
   retractall(text_to_execute("command",_)),
   assert(text_to_execute("command",Str)),
   load_concept_view("",UserName),
   idconcept(I),
   write(I).
   
   
 /**********************************************/     


 main_menu("MSG_set"):-!,
   assert_cmd_values,
   get_paramVal("user",UserName),
    get_current_concept_name(CurCnptName),
    modified_set_tnotion(CurCnptName,UserName),
    save_concept,
    text_to_execute("tnotion", Defin1),!,
    text_to_execute("rest", Defin2),!,
    text_to_execute("command",CmdR),!,   
   msgSetup(Defin1,Defin2,CmdR).  
   
   
 /**********************************************/
 main_menu("DIC_all_ext_templ"):-!,  
   assert_cmd_values,
   get_paramVal("user",UserName),
    get_current_concept_name(CurCnptName),
    modified_set_tnotion(CurCnptName,UserName),    
 /*     У Ильи БЫЛО
   refresh_template_list.
   где
 refresh_template_list.  
  findall_tmpt_list(A),
  %send_templates_list("DIC_all_ext_templ", A),
  findall_dirt_list(B),  
  get_dir_con_t(DirCon), 
  concat("Словарь внешних шаблонов: ..\\ ",DirCon,LeftPath), 
  set_text_papki_t(RightPath),  %!.    
  tmplExplorerPage(A,B,LeftPath,RightPath, "").
*/  
  %findall(Names,get_template(_,_,Names,_,_,_,_),A),
  findall(Names,get_template(_,_,Names,_,_,_,_),A),
   %get_paramVal("user",UserName),
   sort_temp(A,SortList),
   tmplExplorerPage(SortList,[],"Словарь внешних шаблонов:","","").
   
 main_menu("DIC_all_cur_c"):-!,
  assert_cmd_values,
  get_paramVal("user",UserName),
    get_current_concept_name(CurCnptName),
    modified_set_tnotion(CurCnptName,UserName),    
  %findall(Names,get_dic_info(r(2),Names,_,_,_),A),
  findall(Names,get_template(_,_,Names,_,_,_,_),A),
   %get_paramVal("user",UserName),
   sort_temp(A,SortList),
  tmplExplorerPage(SortList,[],"Все шаблоны текущей онтологии","","").
  
 main_menu("DIC_all_cur_env") :- !,
  assert_cmd_values,
  get_paramVal("user",UserName),
    get_current_concept_name(CurCnptName),
    modified_set_tnotion(CurCnptName,UserName),    
  %findall(Names,get_dic_info(r(4),Names,_,_,_),A), 
  findall(Names,get_template(_,_,Names,_,_,_,_),A),
   %get_paramVal("user",UserName),
   sort_temp(A,SortList),
        tmplExplorerPage(SortList,[],"Все шаблоны текущей онтологии и среды","","").
        
 main_menu("DIC_rules") :- !,
  assert_cmd_values,
  get_paramVal("user",UserName),
    get_current_concept_name(CurCnptName),
    modified_set_tnotion(CurCnptName,UserName),      
  %findall(Names,get_dic_info(r(5),Names,_,_,_),A),
  findall(Names,get_template(_,_,Names,_,_,_,_),A),
   %get_paramVal("user",UserName),
   sort_temp(A,SortList),
  tmplExplorerPage(SortList,[],"Нетерминалы","","").

  
   
   
 main_menu("tmpl_left_display") :-! ,
  assert_cmd_values,
  %get_paramVal("user",UserName),
    get_current_concept_name(CurCnptName),
    modified_set_tnotion(CurCnptName,""),  
    get_paramVal("explorer_selected_left", Item),
    %write(Item),
    %delete_r_char(Item, PrItem),
   get_dic_info(_Type,Item,Comment,ListArg,TypeRes),
  list_w_v_to_str_var(ListArg, StrArg),
  expr_to_str(TypeRes,STypeRes),
  format(Text,"Текст:  %\nТипы аргументов: %\nТип результата: %\nПояснение: %",Item,StrArg,STypeRes,Comment),!,
   text_into_html_page_readonly(Text, "").
   
 
 
 main_menu("new_command") :- !,
   assert_cmd_values_intocur,
   get_paramVal("user",UserName),
   get_current_concept_name(CurCnptName),
   modified_set_tnotion(CurCnptName,UserName),
   get_defin1(Text1),
   get_defin2(Text2),
   retractall(text_to_execute(_,_)),
   assert(text_to_execute("tnotion",Text1)),
   assert(text_to_execute("command","")),
   assert(text_to_execute("rest",Text2)),
   %findall(Names,get_dic_info(r(4),Names,_,_,_),A),
   findall(Names,get_template(_,_,Names,_,_,_,_),A),
   sort_temp(A,SortList),
  %assert_cmd_values(ParmList, Rest),
    %get_current_concept_name(CurCnptName),
    %modified_set_tnotion(CurCnptName),    
  %findall(Names,get_dic_info(r(4),Names,_,_,_),A), 
        commandPage(SortList,UserName). 
   
 

 
 
   /*
main_menu ("toolbar", ParmList, ParmList):-!,
   %ParmList = [parm("curcnpt_name",_CurCnptName)|Rest],
   %Rest = [parm("curcnpt_text",_CurCnptText)|Rest1],
   %Rest1 = [parm("env",_Env)|Rest2],
   5Rest2 = [parm("cmd",_Cmd)|Rest3],
   %Rest3 = [parm("rest",_RestT)|Rest4],
  ParmList = [parm("toolbar_button", ToolbarButton)|Rest7],
   toolbar_button_action (ToolbarButton,Rest7, ParmList).  
   */
/*****************************************************************************************/

get_paramVal(Param, Value):-
  parm(Param,Value),!.
get_paramVal(_Param,"").

   
   
   
change_main(CurConceptId,MainId):-
  retract(ver(MainId,ConcId)),
  assert(ver(CurConceptId,ConcId)),
  fail.
change_main(CurConceptId,MainId):-
  assert(ver(CurConceptId,MainId)).     

  
  
  
savepredicate :-
    get_paramVal("curcnpt_id",Id),
   not(Id=""),!,
  send_to_debug("1"),
  assert_save_values,  
  save_concept,!,
    get_defin1(Text1),
   get_defin2(Text2),
   retractall(text_to_execute(_,_)),
  assert(text_to_execute("tnotion",Text1)),
   assert(text_to_execute("command","")),
   assert(text_to_execute("rest",Text2)),
   get_paramVal("user",UserName),
   get_paramVal("version",Ver),
   retractall(version(_)),
   assert(version(Ver)),
   load_concept_readonly("",UserName),!.
    
  savepredicate :- !,  
    get_paramVal("env_id", EnvId),
    get_paramVal("inset",Inset),
   retractall(inset(_)),
   assert(inset(Inset)),
   get_paramVal("old_id",OldId),
   retractall(oldid(_)),
   assert(oldid(OldId)),
   get_paramVal("curcnpt_name",Name),
   retractall(current_concept_name(_)),
   assert(current_concept_name(Name)),
   get_paramVal("version",Ver),
   check_name(Ver,Name),!,
  %upper_lower(Name, LowerName),
        %check_concept_name_to_save(LowerName),!,   
   load_catalog,
   send_to_debug(EnvId),
  name_file(NameEnv,EnvId,_,_),!,             
   set_environment_name(NameEnv),
     % этот предикат загружает и шаблоны
   assert(readonly_concept("false")),
   save_concept_asNewOnt(File),
   retractall(curid(_)),
   assert(curid(File)),
     send_to_debug(File),
     get_paramVal("curcnpt_text",Text1),
    retractall(text_to_execute(_,_)),
  assert(text_to_execute("tnotion",Text1)),
   assert(text_to_execute("command","")),
   assert(text_to_execute("rest","")),   
   retractall(version(_)),
   assert(version(Ver)),
   get_paramVal("user",UserName),
   choose_func(Inset,Func),
   load_concept_view(Func,UserName).   


check_name("",Name):-
upper_lower(Name, LowerName),
        check_concept_name_to_save(LowerName),!.
check_name(_,_).        

 choose_func("SaveOntology","Save_Draft()"):-!.%which func to call in curCnptPage body onLoad
 choose_func("CC","Build_All()"):-!.
   
 check_build(""):-assert(ifbuilt_concept("true")),!.
 check_build(_Text):-assert(ifbuilt_concept("false")),!.  
 
 
 
 split_rest :-
   get_paramVal("curcnpt_text",Text),
   %delete_r_char(CurCnptText, CurCnptTextR),
  searchstring(Text,"*split_rest*",Position),!, %Было "*split_here*"
  cut_rest(Text,Position,S,E),
/*ifndef iso_prolog  
  NewPos=Position-1,
elsedef
  NewPos is Position-1,
enddef
  
  frontstr(NewPos, Text,StrStart,StrEnd),
  frontstr(12, StrEnd,StrStart1,StrEnd1),*/
  retractall(text_to_execute("tnotion",_)),
  retractall(text_to_execute("rest",_)),
  assert(text_to_execute("tnotion",S)),
   assert(text_to_execute("rest",E)).  
 split_rest :- !.  
 
 cut_rest(Text,0,S,E):-!,
   S="",
   frontstr(12, Text,_StrStart1,E).
   
 cut_rest(Text,Position,S,E):-!,
 

  NewPos is Position-1,

  
  frontstr(NewPos, Text,S,StrEnd),
  frontstr(12, StrEnd,_StrStart1,E),
  send_to_debug(StrEnd),
  send_to_debug(E).
   
sort_temp([], []).%сортировка списка шаблонов
sort_temp([H|Tail], S) :-
  split(H, Tail, Small, Big),
  sort_temp(Small, Small1),
  sort_temp(Big, Big1),
  append(Small1, [H|Big1], S).


order(X, Y) :- X @=< Y.



split(H, [A|Tail], [A|Small], Big) :-
  order(A, H), !,
  split(H, Tail, Small, Big).
split(H, [A|Tail], Small, [A|Big]) :-
  split(H, Tail, Small, Big).
  split(_, [], [], []). 
  
  
/*get_version(ConceptId,NewConceptVer):-
  findall(ConceptId1,get_verlist(ConceptId,ConceptId1),A),
  countlist(A,Num),
ifndef iso_prolog  
  Num=TailLength + 1,
elsedef
  Num is TailLength + 1.
enddef
  NewConceptVer=Num+1.
*/    

  
%get_verlist(ConceptId,ConceptId1).
  %ver(ConceptId,ConceptId1).

countlist([],0):-!.

countlist([_|T],Num):-

  countlist(T,TailLength),

  Num is TailLength + 1.

  
  
   

    

  
    
/*****************************************************************************************/ 

 send_templates_list("DIC_all_ext_templ", Res):-
   send_to_debug("Вызван send_templates_list(DIC_all_ext_templ"),
   findall(Names,get_template(_,_,Names,_,_,_,_),A),
  get_templ_textMain(A,[],Res),!.
  
 send_templates_list(_,[]):-!.


 get_templ_textMain([],Res,Res):-!. 
 
 get_templ_textMain([H|T],ResList,Res):-
   get_templ_text(H,TemplText),
  get_templ_textMain(T,[[H,TemplText]|ResList],Res).    


 get_templ_text(Item, Text):-
   %check_name(Item, Item1),
    %delete_r_char(Item, PrItem),
   get_dic_info(_Type,Item,Comment,ListArg,TypeRes),!,
  list_w_v_to_str_var(ListArg, StrArg),
  expr_to_str(TypeRes,STypeRes),
  format(Text,"Текст:  %\nТипы аргументов: %\nТип результата: %\nПояснение: %",Item,StrArg,STypeRes,Comment),!.
  
 get_templ_text(_Item,"").
 

  
 
 



/**********************************************************/


/*Toolbar of the TreeCnpt. 
Possible variants: to Show, to Make current, to Make Environment*/


/***************************************************************************
    
***************************************************************************/           

load_message_ezp:- not(existfile("message.ezp")),!,
    fatalErrorMsg("Файл \"message.ezp\" отсутствует!!!" ).
    %write("File\"message.ezp\" is absent!!!","\n").
load_message_ezp:-
  consult("message.ezp",message_ezp).
/************************************************  MY PART    ***********************************************/
  
get_environmentName_forConceptName(ProperName,NameEnv):-
  name_file(ProperName, _,EnvFile,_),
  name_file(NameEnv,EnvFile,_,_),!.
    
  
send_to_debug(ParameterForDebug):-
  %retractall(parToCheck(_)),
  %consult("debugging_file.txt",fordebugging),
  assert(parToCheck(ParameterForDebug)),
  save("debugging_file.txt",fordebugging).
  



addNewVersion(NamePart,VersionPart,VersionCounter,NewConceptName):-
   str_len(VersionPart,VersionPartLength),

   DotPos is VersionPartLength-2,
   
   subchar(VersionPart,DotPos,Dot),
   Dot='.',!,
   frontstr(DotPos,VersionPart,LeftPart,RightPart),
   frontstr(1,RightPart,VersionNumberStr,_RightBracket),
   str_int(VersionNumberStr,VersionNumberInt),

   NewVersionNumberInt is VersionNumberInt + VersionCounter,
   
   str_int(NewVersionNumber,NewVersionNumberInt),      
   concat(LeftPart, NewVersionNumber, LPAndNewVersionNumber),   
   concat(LPAndNewVersionNumber, ")", NewVersionPart),   
   concat(NamePart, NewVersionPart, NewConceptName).   
 
addNewVersion(NamePart,VersionPart,VersionCounter,NewConceptName):-
   str_len(VersionPart,VersionPartLength),

   DotPos is VersionPartLength-3,
   
   subchar(VersionPart,DotPos,Dot),
   Dot='.',!,
   frontstr(DotPos,VersionPart,LeftPart,RightPart),
   frontstr(2,RightPart,VersionNumberStr,_RightBracket),
   str_int(VersionNumberStr,VersionNumberInt),

   NewVersionNumberInt is VersionNumberInt + VersionCounter,
   
   str_int(NewVersionNumber,NewVersionNumberInt),      
   concat(LeftPart, NewVersionNumber, LPAndNewVersionNumber),   
   concat(LPAndNewVersionNumber, ")", NewVersionPart),   
   concat(NamePart, NewVersionPart, NewConceptName).


makeTempName(ConceptName,VersionCounter,NewConceptName):-
   searchstring(ConceptName, "(v.", Pos),

   LenOfSplit is Pos-1,
   
   frontstr(LenOfSplit, ConceptName, NamePart, VersionPart),
   addNewVersion(NamePart,VersionPart,VersionCounter,NewConceptName),!.   
makeTempName(ConceptName,VersionCounterInt, ResName):-
  concat(ConceptName,"(v.",Name),
  str_int(VersionCounter,VersionCounterInt),
  concat(Name,VersionCounter,Name2),
  concat(Name2,")",ResName).
      

makeTempNameMain(ConceptName,VersionCounter,NewConceptName):-
  makeTempName(ConceptName,VersionCounter,NewConceptName),
  upper_lower(NewConceptName, LowerName),
        check_concept_new_name_to_save(LowerName),!.
    
makeTempNameMain(ConceptName,VersionCounter,NewConceptName):-

  NewVersionCounter is VersionCounter+1,
   
  makeTempNameMain(ConceptName,NewVersionCounter,NewConceptName).  



addNewVersion2(NamePart,VersionPart,VersionCounter,NewConceptName):-
   str_len(VersionPart,VersionPartLength),
   VersionPartLength=3,!,
   frontstr(1,VersionPart,LeftPart,RightPart),
   frontstr(1,RightPart,VersionNumberStr,_RightBracket),
   str_int(VersionNumberStr,VersionNumberInt),

   NewVersionNumberInt is VersionNumberInt + VersionCounter,
   
   str_int(NewVersionNumber,NewVersionNumberInt),      
   concat(LeftPart, NewVersionNumber, LPAndNewVersionNumber),   
   concat(LPAndNewVersionNumber, ")", NewVersionPart),   
   concat(NamePart, NewVersionPart, NewConceptName).   
 
addNewVersion2(NamePart,VersionPart,VersionCounter,NewConceptName):-
   str_len(VersionPart,VersionPartLength),
   VersionPartLength=4,!,
   frontstr(1,VersionPart,LeftPart,RightPart),
   frontstr(2,RightPart,VersionNumberStr,_RightBracket),
   str_int(VersionNumberStr,VersionNumberInt),

   NewVersionNumberInt is VersionNumberInt + VersionCounter,
   
   str_int(NewVersionNumber,NewVersionNumberInt),      
   concat(LeftPart, NewVersionNumber, LPAndNewVersionNumber),   
   concat(LPAndNewVersionNumber, ")", NewVersionPart),   
   concat(NamePart, NewVersionPart, NewConceptName).


makeTempName2(ConceptName,VersionCounter,NewConceptName):-
   searchstring(ConceptName, "(", Pos),

   LenOfSplit is Pos-1,
   
   frontstr(LenOfSplit, ConceptName, NamePart, VersionPart),
   addNewVersion2(NamePart,VersionPart,VersionCounter,NewConceptName),!.   
makeTempName2(ConceptName,VersionCounterInt, ResName):-
  concat(ConceptName,"(",Name),
  str_int(VersionCounter,VersionCounterInt),
  concat(Name,VersionCounter,Name2),
  concat(Name2,")",ResName).
  



makeNameNewOntMain(ConceptName,VersionCounter,NewConceptName):-
  makeTempName2(ConceptName,VersionCounter,NewConceptName),
  upper_lower(NewConceptName, LowerName),
        check_concept_new_name_to_save(LowerName),!.
    
makeNameNewOntMain(ConceptName,VersionCounter,NewConceptName):-

  NewVersionCounter is VersionCounter + 1,
   
  makeNameNewOntMain(ConceptName,NewVersionCounter,NewConceptName).  


existName(NewConceptName):- 
  name_file(NewConceptName,_,_,_),!. 



check_concept_new_name_to_save(Name):-
  str_to_list(Name,ListName),
  name_file(NameF,_,_,_),
  upper_lower(NameF, LowerNameF),
  str_to_list(LowerNameF,ListNameF),
  ListName=ListNameF,!,fail.
check_concept_new_name_to_save(_).  


  

add_readonly("editing"):-
  assert(readonly_concept("false")),!.
add_readonly(_):-  
  assert(readonly_concept("true")),!.
  
  
set_text_to_show:-
  not(execute_command_error),!,
   get_defin1(Text1),
   get_defin2(Text2),
   retractall(text_to_execute(_,_)),
  assert(text_to_execute("tnotion",Text1)),
   assert(text_to_execute("command","")),
   assert(text_to_execute("rest",Text2)).
   
set_text_to_show:-  
   execute_command_error,!,
   get_defin1(Text1),
   get_defin2(Text2),
   retractall(text_to_execute("tnotion",_)),
   retractall(text_to_execute("rest",_)),
  assert(text_to_execute("tnotion",Text1)),
   assert(text_to_execute("rest",Text2)).

save_If_Editing:-
  readonly_concept("true"),!,save_concept.
save_If_Editing.  
   
  
change(InText, OutText):- aux_change(InText,"",OutText).

aux_change("",OutText,OutText):-!.

aux_change(UnChangedText,ChangedText,Res):-
      frontchar(UnChangedText,Char,UnChangedText1),
      Char='>',!,
      concat(ChangedText,"&gt;",ChangedText1),
      aux_change(UnChangedText1,ChangedText1,Res).
      
aux_change(UnChangedText,ChangedText,Res):-
      frontchar(UnChangedText,Char,UnChangedText1),
      Char='<',!,
      concat(ChangedText,"&lt;",ChangedText1),
      aux_change(UnChangedText1,ChangedText1,Res).
      
 aux_change(UnChangedText,ChangedText,Res):-
      frontchar(UnChangedText,Char,UnChangedText1),
      Char='"',!,
      concat(ChangedText,"&quot;",ChangedText1),
      aux_change(UnChangedText1,ChangedText1,Res).
      
  /*    aux_change(UnChangedText,ChangedText,Res):-
      frontchar(UnChangedText,Char,UnChangedText1),
      Char=' ',!,
      concat(ChangedText,"&nbsp;",ChangedText1),
      aux_change(UnChangedText1,ChangedText1,Res).
    */ 
      
aux_change(UnChangedText,ChangedText,Res):-
     frontchar(UnChangedText,Char,UnChangedText1),!,
     str_char(CharS,Char),
      concat(ChangedText,CharS,ChangedText1),
      aux_change(UnChangedText1,ChangedText1,Res).  
aux_change(_,OutText,OutText):-!.   
  
write_added_templates:-
      idconcept(IdConcept),%str_int(IdConceptSTR,IdConcept), send_to_debug("_IdConceptSTR_"),
      in_template(2, IdConcept, Id_template, TextTempl, _Opname,  Comment, ListArg,  TypeRes),%send_to_debug("_in_templ_"),
      %write(IdConcept),write(","),
      write(Id_template),write(","),
      %write(TextTempl),write(","),
      %write(Opname),write(","),
      %write(Comment),write(","),
      list_w_v_to_str_var(ListArg, StrArg),
      %write(StrArg),write(","),
      expr_to_str(TypeRes,STypeRes),
      %write(STypeRes),write(","),
      %format(Text,"Текст:  %\nТипы аргументов: %\nТип результата: %\nПояснение: %",TextTempl,StrArg,STypeRes,Comment),
  format(Text,"%,%,%,%",TextTempl,StrArg,STypeRes,Comment),  
      write(Text),write("###"),
      fail.
write_added_templates.   







cmd_options('tests') :-
   run_parser_tests,
   exit.

cmd_options(X) :- !.





examine_cmd_line :-
  comline(X),
  cmd_options(X).
  

saveParamList([]).

saveParamList([cgiparm(Name,Value)|ParamListRest]) :-
    assert(parm(Name, Value), cgiParms),
    saveParamList(ParamListRest).




check_opt([]).

check_opt([H|T]) :-
  cmd_options(H),
  check_opt(T).


examine_cmd_line_iso([]) :- !.

examine_cmd_line_iso(Opt) :-
  check_opt(Opt).
  


/***************************************************************************
      Main Goal Project
***************************************************************************/


run(Opt) :-  
examine_cmd_line_iso(Opt),
assert(parm("encoding", "UTF-8")),

load_message_ezp,
%open_msg_file,
consult("kernel.tmt",templates),
cgi_GetParmList(ParmList),
saveParamList(ParmList),
%term_str(parmList, ParmList, S),
%msg(diagn,S),
get_dialog_answer,
msg_to_display,
choose_menu_item, 
retractall(dialog_answer(_))
% TODO: temporoarily commentes, uncomment
%save_names.
.

%close_msg_file.

/**************************************************************************/  





notion_base("kernel.ezp").
environment("1").
