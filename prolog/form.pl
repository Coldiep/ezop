% FORM.PRO



:- module(form, [
execute/1
]).
:- style_check(+string).

:- use_module(vp52_compat).
:- use_module(editor).
:- use_module(htmlpages).
:- use_module(calc).
:- use_module(impose).
:- use_module(dbctrl).
:- use_module(db).


%include "editor.pre"






%add_to_defin(ex("question",[]),_):-!.
add_to_defin(ex("message",[]),_):-!. %обработка сообщения системы
%add_to_defin(_,_):- mode_answer,!.
add_to_defin(_,Command_string):-
		update_description_html(Command_string).
 
answer(_,_Command_string,_):-
	not(mode_answer),!.
answer(ex("message",[]),_,_):-!,
	assertz(answer_text("",nil)).
answer(Type,Command_string,Result):-
	Type =ex("question",[]),!,
	assertz(answer_text(Command_string,ex("answ",[ex("nterm",[Result])]))).
answer(_,Command_string,_):-
	assertz(answer_text(Command_string,nil)).


% Предикат execute_command при запуске устанавливает флаг
% execute_command_error который в случае возникновения ошибки 
% должен гасить вызывающий предикат.

message_make_inconsist:-
	not(current_inconsist(_)),!. 
message_make_inconsist:-
        retractall(current_inconsist(_)),
        %dlg_note("ВАША ОНТОЛОГИЯ ПРОТИВОРЕЧИВА"),
        expr_to_str(ex("msg",
             		[nc("ПРЕДЫДУЩЕЕ  ВЫРАЖЕНИЕ  ВЫЗВАЛО ПРОТИВОРЕЧИЕ")]),Txt),
        concat("\n",Txt,Msg),
        update_description_html(Msg),
        set_concept_name_with_error,!.            
message_make_inconsist.

/*
execute(_):-
	retract(dic_window(_)),
	%win_Destroy(Win),
	fail.
	%commented by A.K.
*/

execute("tnotion"):-
	text_to_execute("tnotion",Text1),
	text_to_execute("rest", Text2),
	concat(Text1, Text2, Text),
  	not(fronttoken(Text,_,_)),!,
	retractall(execute_command_error),
  	assert(execute_command_error),
  	msg_n(err,79,[],b_true).%commented by A.K.
  	
execute("tnotion"):-
	idconcept(IdC),
	%send_to_debug(),
	retractall(_,concept),
	load_env_concept,
	get_current_environment_id(IdEnv),
	assert(idconcept(IdC)),
	assert(used_concept(0,IdC,IdEnv)),
	%text_to_execute("tnotion", NewText),%
	retract(text_to_execute("tnotion", NewText)),!,
	set_concept_name_without_error,
	retract(text_to_execute("rest", Text2)),!,
	concat(NewText, Text2, NewRest),
	assert(text_to_execute("rest", NewRest)),
	assert(text_to_execute("tnotion", "")),
	execute("rest"),
% EDWIN - окно rest, Rest Text - текст, оставшийся неразобранным
	%retract(text_to_execute("rest", _)),
	text_to_execute("rest", RestText),!,
	%msg_n(err,81,[RestText],b_true),
	put_defin2(RestText).

execute(T_notion_win):-
	%  T_notion_win - одно из окон:  команда, необработанная часть
	% , command, rest
	text_to_execute(T_notion_win,Cmd),
  	not(fronttoken(Cmd,_,_)),!,
	retractall(execute_command_error),
  	assert(execute_command_error),
  	msg_n(err,79,[],b_true).

execute("rest"):-
	%msg_n(err,81,["no error before execute repeat"],b_true),
	execute_repeat("rest"),
	%msg_n(err,81,["no error"],b_true),
        not(mode_answer),
        not(execute_command_error),!.
        %msg_n(err,81,["no error"],b_true),
execute("command"):-
	%msg_n(err,81,["no error before execute repeat"],b_true),
	execute_repeat("command"),
	%msg_n(err,81,["no error"],b_true),
        %not(mode_answer),
        not(execute_command_error),!.
        %msg_n(err,81,["no error"],b_true),        
execute(_).

execute_repeat(T_notion_win):-
	%retract(text_to_execute(T_notion_win,Cmd)),
	text_to_execute(T_notion_win,Cmd),
  	not(fronttoken(Cmd,_,_)),!.
  	%assert(text_to_execute(T_notion_win,"")).
  
execute_repeat(T_notion_win):-	
  	text_to_execute(T_notion_win,Cmd),
  	put_last_cmd(Cmd),
       	retractall(_,execute_state),
     	execute_command(RestText),
	not(execute_command_error),!,
	msg_n(calc,80,[],b_false),
	set_concept_change_flag,
	retractall(text_to_execute(T_notion_win, _)),
	assert(text_to_execute(T_notion_win, RestText)),
	put_defin2(RestText),
	execute_repeat(T_notion_win),!.
execute_repeat(_) :-
	msg_n(err,81,[],b_true),
	fail.


execute_command(RestText) :-
   	save_concept_dubl,
   	load_concept_dubl, %Эту строку наверно можно удалить
   	get_last_cmd(Command_string),   	% Команда из базы данных
 	retractall(_,execute_state), 
   	assert(execute_command_error),
   	
   	getbacktrack(Btop),
   	
   	assert(done(text_scanned)),
   	impose(Command_string,Sentence,RestText,Expr,Expr_type),	% Наложение шаблонов
   	
   	msg(gr_an,Sentence),
   	retractall(done(text_scanned)),
   	assert(done(plates_imposed)),   	
   	msg_impose(Expr,Btop),
   	try_calc(Expr,Result),	% Вычисление (выполнение) выражения (команды)
   	not(fatal_error),
  	
  	cutbacktrack(Btop),
   	
   	retractall(done(_)),
   	msg_calc(Result),
   	answer(Expr_type,Sentence,Result),   	
   	retract(execute_command_error),
   	add_to_defin(Expr_type,Sentence),!,
   	message_make_inconsist
%   	,db_delete("concept.dub",in_memory)
        .
execute_command(_) :-
 	load_concept_dubl,
%	db_delete("concept.dub",in_memory),
	fail.



/*
load_concept_dubl:-              %изменять вместе с save_conncept_dubl
   db_open(concept_dubl,"concept.dub",in_memory),
   retractall(_,concept),
   fail.
load_concept_dubl:- 
   chain_terms(concept_dubl,ch,concept,Term,_),
   assert(Term,concept),
   fail.
load_concept_dubl:-
   db_close(concept_dubl).
*/

load_concept_dubl :-
	retractall(_,concept),
	save("concept-del.dub", concept),
	consult("concept.dub", concept).


/*
put_out_symbol(Source,Symbol,Rest):-frontchar(Source,Symbol,Rest1),!,
   put_out_symbol(Rest1,Symbol,Rest).
put_out_symbol(Source,_,Source).

replace_defin2_if_no_error(Rest):-not(execute_command_error),!,
   put_defin2(Rest) ; true.
*/

/*
save_concept_dubl:-        % изменять вместе с load_concept_dubl 
   db_create(concept_dubl,"concept.dub",in_memory),
   fail.
save_concept_dubl:- 
   retract(Term,concept),
   chain_insertz(concept_dubl,ch,concept,Term,_),
   fail.
save_concept_dubl:-
   db_close(concept_dubl).
*/

save_concept_dubl:-
	save("concept.dub", concept).


try_calc(Expr,Result):-calc(Expr,Result),!.
try_calc(_,_):-load_concept_dubl,fail.

set_concept_name_without_error:-
	get_current_concept_name(ErrName),
        concat("ERROR**",Name,ErrName),!,
        set_current_concept_name(Name).
        
        %edit_window(WinTn,"tnotion",_),!,
        %edit_SetStatusLineColor(WinTn,  0x00000000,0x00C0C0C0).
set_concept_name_without_error:-!.
	%edit_window(WinTn,"tnotion",_),!,
        %edit_SetStatusLineColor(WinTn,  0x00000000,0x00C0C0C0).


set_concept_name_with_error:-
	    get_current_concept_name(Name),
            not(concat("ERROR**",_,Name)),!,
            concat("ERROR**",Name,ErrName),
            set_current_concept_name(ErrName),
           
           % edit_window(WinTn,"tnotion",_),!,
            % edit_SetStatusLineColor(WinTn,  0x00000000,0x000000FF).
set_concept_name_with_error.

msg_calc(Expr):-
   expr_to_str(Expr,Expr_text),
   format(_Text,"Результат:  %<BR>",   Expr_text),
   msg_n(calc,82,[Expr_text],b_false).
   
msg_impose(_,_).
/*
message_make_inconsist:-
	not(current_inconsist(_)),!. 
message_make_inconsist:-
             retractall(current_inconsist(_)),
             dlg_note("ВАША ОНТОЛОГИЯ ПРОТИВОРЕЧИВА"),
             expr_to_str(ex("msg",
             		[nc("ПРЕДЫДУЩЕЕ  ВЫРАЖЕНИЕ  ВЫЗВАЛО ПРОТИВОРЕЧИЕ")]),Txt),
            concat("\n",Txt,Msg),
             update_description(Msg),
           set_concept_name_with_error,!.            
message_make_inconsist.

update_description(S):-
   get_defin1(Current_text),
  % concat(S,".\n",Tmp_text),
   concat(Current_text,S ,New_text),
   put_defin1(New_text),set_concept_change_flag,
  tnotion_window(WinTn,"tnotion",_),!,
  edit_AppendStr(WinTn,S),
  win_SetFocus(WinTn).
*/  


update_description_html(S):-
	%get_defin1(Current_text),
	text_to_execute("tnotion", Current_text),!,
   	concat(Current_text,S ,New_text),
   	put_defin1(New_text),set_concept_change_flag,
   	retractall(text_to_execute("tnotion", _)),!,
   	%assert(text_to_execute("tnotion", S)).
   	assert(text_to_execute("tnotion", New_text)).
  

set_concept_change_flag:-concept_changed,!.
set_concept_change_flag:-assert(concept_changed).
