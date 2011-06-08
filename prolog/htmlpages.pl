/*****************************************************************************

		Copyright (c) 2008 OpenKnowledge

 Project:  EDITOR
 FileName: D:\MY DOCUMENTS\UNIVERSITY\DIPLOMA\PROJECTS\EDITOR LOCALHOST\HTMLPAGES.PRO
 Purpose: Open Language for Knoledge Representation
 Written by: Ilya Zemlinsky
 Comments:
******************************************************************************/


:- module(htmlpages,[
dlgAsk_eq/1,
dlgAsk/1,
fatalErrorMsg/1,
cnptExplorerPage/5,
tmplExplorerPage/5,
treeCnptPage/0,
msgSetup/3,
text_into_html_page_readonly/2,
treeCnptPage_text_area/1,
indexPage/0,
curCnptPage/5,
send_afterdel/0,
commandPage/2,
curCnptReadonly/5,
after_changever/0
]).
:- style_check(+string).

:- use_module(vp52_compat).
:- use_module(editor).
:- use_module(calc).
:- use_module(dbctrl).
:- use_module(db).
:- use_module(cgitools).


%include "editor.pre"





subPath(Location, EndToCut, EndToSubstitute, NewPath):-!,
 searchstring(Location, EndToCut, Pos),

 LenOfSplit is Pos-1,
 
 frontstr(LenOfSplit, Location, Path, _Name),
 concat(Path, EndToSubstitute, NewPath). 

write_messages:-
	my_message(X),
	write(X), %nl,
	fail.
write_messages. 


write_ext_templatesMain([]):-!. 
write_ext_templatesMain([H|T]):-
	write_ext_templates(H), 
	write("###"),
	write_ext_templatesMain(T).

write_ext_templates([H,T]):-
	write(H),
	write("@@@"), 
	write(T),!.
write_ext_templates(_).

get_used_concepts(""):-
	write(""),!.
get_used_concepts(CurId):-
	str_int(CurId,CuridInt),
	findall(IdUsedConcept, use(_OforUseOr1forParent,CuridInt,IdUsedConcept),ListOfUsedIds),
	check_used(ListOfUsedIds).
	
check_used([]):-!,	
get_current_environment_file(NameEnvId),
write("["),
write(NameEnvId),
write("]").

check_used(ListOfUsedIds):-!,
write(ListOfUsedIds).
	
get_inset(S):-inset(S),!.
get_inset("CC").

get_ver(Version):-
	version(Version),!.
get_ver("").


write_charset :-
	parm("encoding", Encoding),!,
	write("<meta http-equiv=\"content-type\" content=\"text/html; charset="),
	write(Encoding), write("\"> ").


%*********************************************************************************************************************** 

curCnptPage(Text, Text2, _LastCMD,Func,UserName) :-!,
write("Content-type: text/html\n\n"),
write(" <html> "),
write(" <head> "),
write(" <title>Система представления понятий ЭЗОП</title> "),
%write(" <meta http-equiv=\"content-type\" content=\"text/html; charset=windows-1251\"> "),
write_charset,


%<!--%%%%%%%%%%%% Menu Styles %%%%%%%%%%%-->
write(" <link rel=\"Stylesheet\" type=\"text/css\" href=\"Start/menu.css\" /> "),
write(" <link rel=\"Stylesheet\" type=\"text/css\" href=\"Start/css.css\" /> "),
%<!-- Add-On Core Code (Remove when not using any add-on's) -->
write(" <style type=\"text/css\">.qmfv{visibility:visible !important;}.qmfh{visibility:hidden !important;}</style> "),
write(" <script type=\"text/javascript\">qmad=new Object();qmad.bvis=\"\";qmad.bhide=\"\";</script> "),
	%<!-- Add-On Settings -->
write(" <script type=\"text/JavaScript\" src=\"Start/AddOnSettings.js\"></script> "),
%<!-- Core Menu Code -->
write(" <script type=\"text/javascript\" src=\"Start/CoreMenu.js\"></script> "),
%<!-- Add-On Code: Slide Animation -->
write(" <script type=\"text/javascript\" src=\"Start/AddOnSlideAnimation.js\"></script> "),
%<!-- Add-On Code: Item Bullets (CSS - Imageless) -->
write(" <script type=\"text/javascript\" src=\"Start/AddOnItemBullets.js\"></script> "),
%<!-- Main JScriptFile -->
write(" <script type=\"text/javascript\" src=\"Start/Main.js\"></script> "),

write(" </head> "),
write(" <body style=\"background-image:url(headbg.jpg); background-repeat:no-repeat;\" onLoad=\" "), 
write(Func),write("\">"),

%get_defin1(Defin1),
send_to_debug("Defin1="),
send_to_debug(Text),
change(Text,Defin1Changed),
write("<textarea id=\"defin1\" style=\"display:none;\">"),
write(Defin1Changed),%send_to_debug(Defin1Changed),
write("</textarea>"),


get_current_concept_name( Name ),
test_inconsistence(ClassID),
ifReadonlyNoCnptName(Readonly, ShowSubMenu),  
ifBuilt(Text2,ShowState),
%<!-- QuickMenu Structure [Menu 0] -->

write(" <ul id=\"qm0\" class=\"qmmc\"> "),

	write(" <li><a class=\"qmparent\" href=\"javascript:void(0)\">Текущая онтология</a> "),

		write(" <ul> "),
		write(ShowSubMenu),
		%write(" <li><a href=\"javascript:RenameOntology();\">Переименовать онтологию</a></li> "),
		write(" </ul></li> "),

	write(" <li><a class=\"qmparent\" href=\"javascript:void(0)\">Команда</a> "),

		write(" <ul> "),
		write(" <li><a href=\"javascript:New_Command();\">Новая команда</a></li> "),
		write(" </ul></li> "),

	write(" <li><a class=\"qmparent\" href=\"javascript:void(0)\">Словари</a> "),

		write(" <ul> "),
		write(" <li><a href=\"javascript:Show_DIC_all_ext_templ();\">Все внешние шаблоны</a></li> "),
		write(" <li><a href=\"javascript:Show_DIC_all_cur_c();\">Все шаблоны текущей онтологии</a></li> "),
		write(" <li><a href=\"javascript:Show_DIC_all_cur_env();\">Все шаблоны текущей онтологии и среды</a></li> "),
		write(" <li><a href=\"javascript:Show_DIC_rules();\">Продукции</a></li> "),
		write(" </ul></li> "),

	write(" <li><a class=\"qmparent\" href=\"javascript:void(0)\">Сообщения</a> "),

		write(" <ul> "),
		%write(" <li><a href=\"javascript:void(0)\">Показать</a></li> "),
		write(" <li><a href=\"javascript:Set_Messages();\">Установить</a></li> "),
		write(" </ul></li> "),

write(" <li class=\"qmclear\">&nbsp;</li></ul> "),
%<!-- Create Menu Settings: (Menu ID, Is Vertical, Show Timer, Hide Timer, On Click ('all', 'main' or 'lev2'), Right to Left, Horizontal Subs, Flush Left, Flush Top) -->
write(" <script type=\"text/javascript\">qm_create(0,false,0,500,false,false,false,false,false);</script> "),
write("<br>"),
write(" <div> "),  
ifIncons(ClassID),
%choose_mode(Name, ModeLabel, ModeValue),
write(" <SPAN class=\"header\"> Текущая онтология: </SPAN> "),
%writef("<SPAN id = \"name_cnpt\" class=\"%\"> ", ClassID),
write("<input type=\"text\" id = \"name_cnpt\" name=\"ontoname\" value=\""),
%write(" <SPAN class=\"header2\">  "),
write( Name ),
get_inset(Inset),
write("\" onKeyPress=\"RenameOntology(event);\""),
write_readonly_forname(Inset),
write(">"),
%write(" </SPAN> "),
write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
write(" <SPAN class=\"header\"> Версия: </SPAN> "),
write("<input type=\"text\" size=\"5\" id = \"cnpt_ver\" name=\"cnpt_ver\" value=\""),
get_ver(Version),
write(Version),
write("\""),
write_readonly_forver(Inset),
write(">"),
write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
get_current_environment_name( NameEnv ),
send_to_debug(NameEnv),
write(" <SPAN class=\"header\"> Разработано в среде: </SPAN> "),
write(" <SPAN id = \"SPAN\" class=\"header2\">  "),
write( NameEnv ),
write(" </SPAN> "),
%write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
%write(" <SPAN class=\"header\"> Режим: </SPAN> "),
%write(" <SPAN id = \"SPAN\" class=\"header2\">  "),
%write( ModeLabel ),
%write(" </SPAN> "),
write("<div id=\"state\" "),write( ShowState ),%ShowState: style=\"display:none;\"> or style=\"display:inline;\">
send_to_debug(ShowState),
write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
write(" <SPAN class=\"header\"> Статус: </SPAN> "),
write(" <SPAN id = \"SPAN\" class=\"header2\">  "),
write("Построена"),
write(" </SPAN> "),
write("</div>"),
%user(UserName),
write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
write(" <SPAN class=\"header\"> Автор: </SPAN> "),
write(" <SPAN id = \"SPAN\" class=\"header2\">  "),
write(UserName),
write(" </SPAN> "),
write(" <table height = \"70%\" width = \"100%\" cellspacing=\"0\" border=\"1%\" bordercolor= \"#6699cc\"> "), 
write(" <tr> "),
write(" <td width = \"60%\"> "),
write(" <textarea id = \"concept_textarea\" name = \"concept_textarea\" SIZE=\"59\" style = \"WIDTH: 100%; HEIGHT: 440px; background-color:#ffffff; \" rows=14 cols=78 wrap=\"soft\" "),
write(Readonly), 
write("onChange=\"textChanged();\" "),
write(">"), 
change(Text,TextChanged),
write(TextChanged),%send_to_debug(TextChanged),
write("</textarea>"),
write(" </td> "),
/*write(" <td width = \"40%\"> "), 
write(" <table height = \"100%\" width = \"100%\" cellspacing=\"0\" border=\"0\" bordercolor= \"#1a39fb\"> "),
write(" <tr height = \"40%\"> "),
write("  <td> "),
%write(" <SPAN class=\"header\"> Новая команда или вопрос: </SPAN> "),
write(" <textarea id = \"concept_cmd\" name = \"concept_cmd\" style = \"WIDTH: 100%; HEIGHT: 150px\" SCROLL=\"YES\" WRAP=hard >"),
write_option_list(LeftList, "left"),
write("</textarea>"),
write(" </td> "),
write(" </tr> "),*/
/*write(" <tr height = \"20%\"> "),
write(" <td> "),
write("<div>"),
write(" <SPAN class=\"header\">Результат</SPAN><br> "),
write_answer,
write("</div>"),
write(" </td> "),
write(" </tr> "),*/
write(" <tr height = \"40%\" > "),
write(" <td> "),
write(" <SPAN class=\"header\"> Необработанная часть текста: </SPAN> "),
write(" <textarea id = \"concept_rest\" name = \"concept_rest\" style = \"WIDTH: 100%; HEIGHT: 200px\" "),
write(Readonly), write(">"), 
change(Text2,Defin2Changed),
write(Defin2Changed),
write("</textarea>"),
write(" </td></tr></table> "),
write(" </td></tr></table> "),
write(" </div> "),
write(" <SPAN class=\"header\"> Сообщения: </SPAN> "),

write(" <textarea id=\"field_for_messages\" height=\"150px\"> "),
write_messages,
write(" </textarea> "),


write(" <textarea id=\"field_for_addedTempl\" height=\"350px\" style=\"display:none;\"> "),
write_added_templates,
write(" </textarea> "),


write(" <textarea id=\"field_for_ExtTempl\" height=\"350px\" style=\"display:none;\"> "),
send_templates_list("DIC_all_ext_templ", Res),
write_ext_templatesMain(Res),
write(" </textarea> "),

/*write(" <p><input type=\"button\" id=\"sendExtTempl\" name = \"sendExtTempl\" value = \"Послать внешние шаблоны\" onclick=\"SendET();\"></input> "),

write(" <form action=\"../proc_data.php\" target=\"_blank\" method=\"POST\" id = \"SendTempl_form\"> "),
write(" <p><input type=\"hidden\" id=\"menu_item2\" name = \"menu_item2\" value = \"SendExtTempl\"> "),
write(" <p><input type=\"hidden\" id=\"ExtTemplates\" name = \"ExtTemplates\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"button\" onclick = \"submit();\"> "),
write(" </form> "),*/

/*
*/

curid(NewVersionId),
send_to_debug(NewVersionId),
oldid(OldId),
send_to_debug(OldId),
get_current_environment_file(NameEnvId),
send_to_debug(NameEnvId),

send_to_debug(Inset),
%concat_texts(Defin1Changed,Text2,OntoText),
%send_to_debug("OntoText="),
%send_to_debug(OntoText),
%user(UserName),
%send_to_debug(UserName),
write(" <form action=\"editor.exe\" target=\"_self\" method=\"POST\" id = \"main_form\"> "),
write(" <p><input type=\"hidden\" id=\"dlg_ask\" name = \"dlg_ask\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"msg_to_display\" name = \"msg_to_display\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"menu_item\" name = \"menu_item\" value = \"\"> "),
writef(" <p><input type=\"hidden\" id=\"curcnpt_id\" name = \"curcnpt_id\" value = \"%\"> ", NewVersionId),
writef(" <p><input type=\"hidden\" id=\"old_id\" name = \"old_id\" value = \"%\"> ", OldId),
writef(" <p><input type=\"hidden\" id=\"curcnpt_name\" name = \"curcnpt_name\" value = \"%\">",Name),
writef(" <p><input type=\"hidden\" id=\"env_id\" name = \"env_id\" value = \"%\"> ", NameEnvId),
writef(" <p><input type=\"hidden\" id=\"curcnpt_text\" name = \"curcnpt_text\" value = \"%\"> ", Defin1Changed),
%write(" <p><input type=\"hidden\" id=\"cmd\" name = \"cmd\" value = \"\"> "), 
writef(" <p><input type=\"hidden\" id=\"rest\" name = \"rest\" value = \"%\"> ", Defin2Changed),
write(" <p><input type=\"hidden\" id=\"usedIds\" name = \"usedIds\" value = \""),
get_used_concepts(NewVersionId), 
write("\"> "), 
%writef(" <p><input type=\"hidden\" id=\"mode\" name = \"mode\" value = \"%\"> ", ModeValue),
writef(" <p><input type=\"hidden\" id=\"inset\" name = \"inset\" value = \"%\"> ", Inset),
%writef(" <p><input type=\"hidden\" id=\"user\" name = \"user\" value = \"%\"> ", UserName),


write(" <p><input type=\"hidden\" id=\"toolbar_button\" name = \"toolbar_button\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"explorer_selected_left\" name = \"explorer_selected_left\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"explorer_selected_right\" name = \"explorer_selected_right\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"new_folder_name\" name = \"new_folder_name\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"new_cnpt_name\" name = \"new_cnpt_name\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"cmd\" name = \"cmd\" value = \"\"> "), 
writef(" <p><input type=\"hidden\" id=\"user\" name = \"user\" value = \"%\"> ", UserName),
write(" <p><input type=\"hidden\" id=\"ext_templ\" name = \"ext_templ\" value = \"%\"> "),
writef(" <p><input type=\"hidden\" id=\"version\" name = \"version\" value = \"%\"> ",Version),
write(" <p><input type=\"hidden\" id=\"button\" onclick = \"submit();\"> "),
write(" </form> "),

%write(" <form action=\"editor.exe\" target=\"_blank\" method=\"POST\" id = \"SendTempl_form\"> "),
%write(" <p><input type=\"hidden\" id=\"ExtTemplates\" name = \"ExtTemplates\" value = \"\"> "),
%write(" <p><input type=\"hidden\" id=\"button\" onclick = \"submit();\"> "),
%write(" </form> "),

write(" </body> "),
write(" </html> ").
 
/**************************************************************************************************************************************************************/
curCnptReadonly(Text, Text2, _LastCMD,Func,UserName) :-!,
write("Content-type: text/html\n\n"),
write(" <html> "),
write(" <head> "),
write(" <title>Система представления понятий ЭЗОП</title> "),
%write(" <meta http-equiv=\"content-type\" content=\"text/html; charset=windows-1251\"> "),
write_charset,

%<!--%%%%%%%%%%%% Menu Styles %%%%%%%%%%%-->
write(" <link rel=\"Stylesheet\" type=\"text/css\" href=\"Start/menu.css\" /> "),
write(" <link rel=\"Stylesheet\" type=\"text/css\" href=\"Start/css.css\" /> "),
%<!-- Add-On Core Code (Remove when not using any add-on's) -->
write(" <style type=\"text/css\">.qmfv{visibility:visible !important;}.qmfh{visibility:hidden !important;}</style> "),
write(" <script type=\"text/javascript\">qmad=new Object();qmad.bvis=\"\";qmad.bhide=\"\";</script> "),
	%<!-- Add-On Settings -->
write(" <script type=\"text/JavaScript\" src=\"Start/AddOnSettings.js\"></script> "),
%<!-- Core Menu Code -->
write(" <script type=\"text/javascript\" src=\"Start/CoreMenu.js\"></script> "),
%<!-- Add-On Code: Slide Animation -->
write(" <script type=\"text/javascript\" src=\"Start/AddOnSlideAnimation.js\"></script> "),
%<!-- Add-On Code: Item Bullets (CSS - Imageless) -->
write(" <script type=\"text/javascript\" src=\"Start/AddOnItemBullets.js\"></script> "),
%<!-- Main JScriptFile -->
write(" <script type=\"text/javascript\" src=\"Start/Main.js\"></script> "),

write(" </head> "),
write(" <body style=\"background-image:url(headbg.jpg); background-repeat:no-repeat;\" onLoad=\" "), 
write(Func),write("\">"),

get_defin1(Defin1),
change(Defin1,Defin1Changed),
write("<textarea id=\"defin1\" style=\"display:none;\">"),
write(Defin1Changed),%send_to_debug(Defin1Changed),
write("</textarea>"),


get_current_concept_name( Name ),
test_inconsistence(ClassID),
ifReadonlyNoCnptName(Readonly, ShowSubMenu),  
ifBuilt(Text2,ShowState),
%<!-- QuickMenu Structure [Menu 0] -->

write(" <ul id=\"qm0\" class=\"qmmc\"> "),

	write(" <li><a class=\"qmparent\" href=\"javascript:void(0)\">Текущая онтология</a> "),

		write(" <ul> "),
		%write(ShowSubMenu),
		write(" <li><a href=\"javascript:NewVersion();\">Создать версию текущей онтологии</a></li> "),
		write(" </ul></li> "),

	write(" <li><a class=\"qmparent\" href=\"javascript:void(0)\">Команда</a> "),

		write(" <ul> "),
		write(" <li><a href=\"javascript:New_Command();\">Новая команда</a></li> "),
		write(" </ul></li> "),

	write(" <li><a class=\"qmparent\" href=\"javascript:void(0)\">Словари</a> "),

		write(" <ul> "),
		write(" <li><a href=\"javascript:Show_DIC_all_ext_templ();\">Все внешние шаблоны</a></li> "),
		write(" <li><a href=\"javascript:Show_DIC_all_cur_c();\">Все шаблоны текущей онтологии</a></li> "),
		write(" <li><a href=\"javascript:Show_DIC_all_cur_env();\">Все шаблоны текущей онтологии и среды</a></li> "),
		write(" <li><a href=\"javascript:Show_DIC_rules();\">Продукции</a></li> "),
		write(" </ul></li> "),

	write(" <li><a class=\"qmparent\" href=\"javascript:void(0)\">Сообщения</a> "),

		write(" <ul> "),
		%write(" <li><a href=\"javascript:void(0)\">Показать</a></li> "),
		write(" <li><a href=\"javascript:Set_Messages();\">Установить</a></li> "),
		write(" </ul></li> "),

write(" <li class=\"qmclear\">&nbsp;</li></ul> "),
%<!-- Create Menu Settings: (Menu ID, Is Vertical, Show Timer, Hide Timer, On Click ('all', 'main' or 'lev2'), Right to Left, Horizontal Subs, Flush Left, Flush Top) -->
write(" <script type=\"text/javascript\">qm_create(0,false,0,500,false,false,false,false,false);</script> "),
write("<br>"),
write(" <div> "),  
ifIncons(ClassID),
%choose_mode(Name, ModeLabel, ModeValue),
write(" <SPAN class=\"header\"> Текущая онтология: </SPAN> "),
%writef("<SPAN id = \"name_cnpt\" class=\"%\"> ", ClassID),
write("<input type=\"text\" id = \"name_cnpt\" name=\"ontoname\" value=\""),
%write(" <SPAN class=\"header2\">  "),
write( Name ),
get_inset(Inset),
write("\" onKeyPress=\"RenameOntology(event);\""),
write_readonly_forname(Inset),
write(">"),
%write(" </SPAN> "),
write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
write(" <SPAN class=\"header\"> Версия: </SPAN> "),
write("<input type=\"text\" size=\"5\" id = \"cnpt_ver\" name=\"cnpt_ver\" value=\""),
get_ver(Version),
write(Version),
write("\""),
write_readonly_forver(Inset),
write(">"),
write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
get_current_environment_name( NameEnv ),
send_to_debug(NameEnv),
write(" <SPAN class=\"header\"> Разработано в среде: </SPAN> "),
write(" <SPAN id = \"SPAN\" class=\"header2\">  "),
write( NameEnv ),
write(" </SPAN> "),
%write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
%write(" <SPAN class=\"header\"> Режим: </SPAN> "),
%write(" <SPAN id = \"SPAN\" class=\"header2\">  "),
%write( ModeLabel ),
%write(" </SPAN> "),
write("<div id=\"state\" "),write( ShowState ),%ShowState: style=\"display:none;\"> or style=\"display:inline;\">
send_to_debug(ShowState),
write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
write(" <SPAN class=\"header\"> Статус: </SPAN> "),
write(" <SPAN id = \"SPAN\" class=\"header2\">  "),
write("Построена"),
write(" </SPAN> "),
write("</div>"),
%user(UserName),
write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
write(" <SPAN class=\"header\"> Автор: </SPAN> "),
write(" <SPAN id = \"SPAN\" class=\"header2\">  "),
write(UserName),
write(" </SPAN> "),
write(" <table height = \"70%\" width = \"100%\" cellspacing=\"0\" border=\"1%\" bordercolor= \"#6699cc\"> "), 
write(" <tr> "),
write(" <td width = \"60%\"> "),
write(" <textarea id = \"concept_textarea\" name = \"concept_textarea\" SIZE=\"59\" style = \"WIDTH: 100%; HEIGHT: 440px; background-color:#ffffff; \" rows=14 cols=78 wrap=\"soft\" "),
write(Readonly), 
write("onChange=\"textChanged();\" "),
write(">"), 
change(Text,TextChanged),
write(TextChanged),%send_to_debug(TextChanged),
write("</textarea>"),
write(" </td> "),
/*write(" <td width = \"40%\"> "), 
write(" <table height = \"100%\" width = \"100%\" cellspacing=\"0\" border=\"0\" bordercolor= \"#1a39fb\"> "),
write(" <tr height = \"40%\"> "),
write("  <td> "),
%write(" <SPAN class=\"header\"> Новая команда или вопрос: </SPAN> "),
write(" <textarea id = \"concept_cmd\" name = \"concept_cmd\" style = \"WIDTH: 100%; HEIGHT: 150px\" SCROLL=\"YES\" WRAP=hard >"),
write_option_list(LeftList, "left"),
write("</textarea>"),
write(" </td> "),
write(" </tr> "),*/
/*write(" <tr height = \"20%\"> "),
write(" <td> "),
write("<div>"),
write(" <SPAN class=\"header\">Результат</SPAN><br> "),
write_answer,
write("</div>"),
write(" </td> "),
write(" </tr> "),*/

write(" </table> "),
write(" </td></tr></table> "),
write(" </div> "),



write(" <textarea id=\"field_for_addedTempl\" height=\"350px\" style=\"display:none;\"> "),
write_added_templates,
write(" </textarea> "),

 
write(" <textarea id=\"field_for_ExtTempl\" height=\"350px\" style=\"display:none;\"> "),
send_templates_list("DIC_all_ext_templ", Res),
write_ext_templatesMain(Res),
write(" </textarea> "),

/*write(" <p><input type=\"button\" id=\"sendExtTempl\" name = \"sendExtTempl\" value = \"Послать внешние шаблоны\" onclick=\"SendET();\"></input> "),

write(" <form action=\"../proc_data.php\" target=\"_blank\" method=\"POST\" id = \"SendTempl_form\"> "),
write(" <p><input type=\"hidden\" id=\"menu_item2\" name = \"menu_item2\" value = \"SendExtTempl\"> "),
write(" <p><input type=\"hidden\" id=\"ExtTemplates\" name = \"ExtTemplates\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"button\" onclick = \"submit();\"> "),
write(" </form> "),*/

/*
*/

curid(NewVersionId),
send_to_debug(NewVersionId),
oldid(OldId),
send_to_debug(OldId),
get_current_environment_file(NameEnvId),
send_to_debug(NameEnvId),

send_to_debug(Inset),
%concat_texts(Defin1Changed,Text2,OntoText),
%send_to_debug("OntoText="),
%send_to_debug(OntoText),
%user(UserName),
%send_to_debug(UserName),
write(" <form action=\"editor.exe\" target=\"_self\" method=\"POST\" id = \"main_form\"> "),
write(" <p><input type=\"hidden\" id=\"dlg_ask\" name = \"dlg_ask\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"msg_to_display\" name = \"msg_to_display\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"menu_item\" name = \"menu_item\" value = \"\"> "),
writef(" <p><input type=\"hidden\" id=\"curcnpt_id\" name = \"curcnpt_id\" value = \"%\"> ", NewVersionId),
writef(" <p><input type=\"hidden\" id=\"old_id\" name = \"old_id\" value = \"%\"> ", OldId),
writef(" <p><input type=\"hidden\" id=\"curcnpt_name\" name = \"curcnpt_name\" value = \"%\">",Name),
writef(" <p><input type=\"hidden\" id=\"env_id\" name = \"env_id\" value = \"%\"> ", NameEnvId),
writef(" <p><input type=\"hidden\" id=\"curcnpt_text\" name = \"curcnpt_text\" value = \"%\"> ", Defin1Changed),
%write(" <p><input type=\"hidden\" id=\"cmd\" name = \"cmd\" value = \"\"> "), 
writef(" <p><input type=\"hidden\" id=\"rest\" name = \"rest\" value = \"%\"> ", Text2),
write(" <p><input type=\"hidden\" id=\"usedIds\" name = \"usedIds\" value = \""),
get_used_concepts(NewVersionId), 
write("\"> "), 
%writef(" <p><input type=\"hidden\" id=\"mode\" name = \"mode\" value = \"%\"> ", ModeValue),
writef(" <p><input type=\"hidden\" id=\"inset\" name = \"inset\" value = \"%\"> ", Inset),
%writef(" <p><input type=\"hidden\" id=\"user\" name = \"user\" value = \"%\"> ", UserName),


write(" <p><input type=\"hidden\" id=\"toolbar_button\" name = \"toolbar_button\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"explorer_selected_left\" name = \"explorer_selected_left\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"explorer_selected_right\" name = \"explorer_selected_right\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"new_folder_name\" name = \"new_folder_name\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"new_cnpt_name\" name = \"new_cnpt_name\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"cmd\" name = \"cmd\" value = \"\"> "), 
writef(" <p><input type=\"hidden\" id=\"user\" name = \"user\" value = \"%\"> ", UserName),
write(" <p><input type=\"hidden\" id=\"ext_templ\" name = \"ext_templ\" value = \"%\"> "),
writef(" <p><input type=\"hidden\" id=\"version\" name = \"version\" value = \"%\"> ",Version),
write(" <p><input type=\"hidden\" id=\"button\" onclick = \"submit();\"> "),
write(" </form> "),

%write(" <form action=\"editor.exe\" target=\"_blank\" method=\"POST\" id = \"SendTempl_form\"> "),
%write(" <p><input type=\"hidden\" id=\"ExtTemplates\" name = \"ExtTemplates\" value = \"\"> "),
%write(" <p><input type=\"hidden\" id=\"button\" onclick = \"submit();\"> "),
%write(" </form> "),

write(" </body> "),
write(" </html> ").
 
/*******************************************************************************************************************************************/

send_afterdel :-!,
get_paramVal("curcnpt_id",Id),
get_paramVal("drupal_id",Drupal_Id),
get_paramVal("curcnpt_name",Curcnpt_name),
%send_to_debug(Name),
%send_to_debug(Id),
write("Content-type: text/html\n\n"),
write(" <html> "),
write(" <head> "),

%write(" <meta http-equiv=\"content-type\" content=\"text/html; charset=windows-1251\"> "),
write_charset,


write(" <script type=\"text/javascript\" src=\"Start/Main.js\"></script> "),
write("</head> "),
write(" <body onload=\"afterDel();\"> "),
write(" <form action=\"../proc_data.php\" method=\"POST\" id = \"SendAfterDel_form\"> "),
writef(" <p><input type=\"hidden\" id=\"old_id\" name = \"old_id\" value = \"%\"> ", Id),
write(" <p><input type=\"hidden\" id=\"menu_item\" name = \"menu_item\" value = \""),
inset(Inset),
which_inset(Inset),
write("\"> "),
writef(" <p><input type=\"hidden\" id=\"drupal_id\" name = \"drupal_id\" value = \"%\"> ", Drupal_Id),
writef(" <p><input type=\"hidden\" id=\"curcnpt_name\" name = \"curcnpt_name\" value = \"%\"> ", Curcnpt_name),
%writef(" <p><input type=\"hidden\" id=\"old_id\" name = \"old_id\" value = \"%\"> ", Id),
%write(" <p><input type=\"hidden\" id=\"menu_item\" name = \"menu_item\" value = \"delete_ontology\"> "),
%write(" <p><input type=\"button\" id=\"button\" onclick = \"submit();\"> "),
write(" </form> "),
write(" </body> "),

write(" </html> "),
send_to_debug("aftedel was sent").



/*****************************************************************************************************************/

after_changever :- !,

%send_to_debug(Name),
%send_to_debug(Id),
write("Content-type: text/html\n\n"),
write(" <html> "),
write(" <head> "),

%write(" <meta http-equiv=\"content-type\" content=\"text/html; charset=windows-1251\"> "),
write_charset,


write(" <script type=\"text/javascript\" src=\"Start/Main.js\"></script> "),
write("</head> "),
write(" <body onload=\"afterChangeVer();\"> "),
write(" </body> "),
write(" </html> ").

/*****************************************************************************************************************/

which_inset("delete_draft"):-!,
write("delete_draft").
which_inset(_):-
write("delete_ontology").

/*****************************************************************************************************************/
commandPage(LeftList,UserName):-
% location(Location),
% subPath(Location,"editor.exe","TemplateExplorer/", _), %TemplateExplorerPath),
 write("Content-type: text/html\n\n"),
 write(" <html> "),
 write(" <head> "),
% write(" <meta http-equiv=\"content-type\" content=\"text/html; charset=windows-1251\"> "),
 write_charset,



%<!-- Main JScriptFile -->
write(" <script type=\"text/javascript\" src=\"Start/Main.js\"></script> "),

 write(" <title>Команда</title> "),
 write(" <link rel=\"Stylesheet\" type=\"text/css\" href=\"Start/css.css\" /> "),
  write(" <style> "),
 write(" .MenuButton, .MenuButtonWithoutAction, .MenuButtonSelectedLeft, #text {font-family: Book Antiqua; font-size:13; font-weight: bolder;  color:#336699;}"),
 write(" .SelectDec{font-family: Book Antiqua; font-size:15;font-weight: bolder; color:#336699; width:100% } "),
 write(" </style> "), 
write(" </head> "),
 

 
 
% write(" <body onload=\"ifDisabled();\"> "),
 write(" <body style=\"background-image:url(headbg.jpg); background-repeat:no-repeat;\"> "),

get_current_concept_name(Name),
send_to_debug(Name),
curid(NewVersionId),
oldid(OldId),
get_current_environment_file(NameEnvId),
get_inset(Inset),  
get_defin1(Defin1),
change(Defin1,Defin1Changed),
choose_mode(Name, _ModeLabel, ModeValue),
%user(UserName),

%choose_mode(Name, ModeLabel, ModeValue),
%get_current_concept_name( Name ),
write(" <SPAN class=\"header\"> Текущая онтология: </SPAN> "),
%writef("<SPAN id = \"name_cnpt\" class=\"%\"> ", ClassID),
write(" <SPAN class=\"header2\">  "),
write( Name ),
write(" </SPAN> "),
write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
get_current_environment_name( NameEnv ),
write(" <SPAN class=\"header\"> Разработано в среде: </SPAN> "),
write(" <SPAN id = \"SPAN\" class=\"header2\">  "),
write( NameEnv ),
write(" </SPAN> "),
write(" &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; "),
write(" <SPAN class=\"header\"> Автор: </SPAN> "),
write(" <SPAN id = \"SPAN\" class=\"header2\">  "),
write(UserName),
write(" </SPAN> "),
write(" <br> "),
write(" <br> "),
write(" <SPAN class=\"header\"> Новая команда или вопрос: </SPAN> "),
write(" <br> "),
write(" <table height = \"50%\" width = \"50%\" cellspacing=\"0\" border=\"1%\" bordercolor= \"#6699cc\"> "), 
write(" <tr> "),
write(" <td > "),
write(" <textarea id = \"concept_cmd\" name = \"concept_cmd\" SCROLL=\"YES\" style = \"WIDTH: 100%; HEIGHT: 250px\"  WRAP=hard >"),
%style = \"WIDTH: 100%; HEIGHT: 250px\" 
write_answer,
write("</textarea>"),
%write(" </tr> "),
%write(" </td >"),
write(" </table >"),
%write(" <td width = \"40%\"> "), 

write(" <table height = \"50%\" width = \"48%\"  cellspacing=\"0\" border=\"1\" bordercolor= \"#6699cc\" STYLE=\"position : absolute; right:10px; top:49px\"> "),
write("<caption class=\"header\" align=\"left\">Шаблоны: </caption>"),
write(" <tr > "),
write("  <td > "),

write(" <textarea id = \"concept_tmpl\" name = \"concept_tmpl\" SCROLL=\"YES\" style = \"WIDTH: 100%; HEIGHT: 250px\" WRAP=hard readonly>"),
%style = \"WIDTH: 50%; HEIGHT: 150px\" 
write_option_list2(LeftList, "left"),
write("</textarea>"),
write(" </td> "),
write(" </tr> "),
write("</table>"),
write(" <p><input type=\"button\" id=\"run_com\" name = \"run_com\" value = \"Выполнить\"onClick=\"Run_Command()\"> "),
write("<div>"),
write(" <br> "),
write(" <br> "),
write(" <SPAN class=\"header\">Необработанная часть текста</SPAN><br> "),
%write(" <br> "),
write(" <table height = \"50%\" width = \"100%\" cellspacing=\"0\" border=\"1%\" bordercolor= \"#6699cc\"> "), 
write(" <tr> "),
write(" <td width = \"60%\"> "),
write(" <textarea id = \"cmd_answer\" name = \"cmd_answer\" style = \"WIDTH:100%; HEIGHT: 250px\" SCROLL=\"YES\" WRAP=hard readonly>"),
text_to_execute("command", Rest),!,
write(Rest), 

write("</textarea>"),
write(" </tr> "),
write(" </td >"),
write(" </table >"),

write("</div>"),
%write(" </td> "),
%write(" </tr> "),
write(" <br> "),
write(" <SPAN class=\"header\"> Сообщения: </SPAN> "),

write(" <textarea id=\"field_for_messages\" height=\"150px\"> "),
write_messages,
write(" </textarea> "),

  
write(" <form action=\"editor.exe\" target=\"exp_concept_text\" method=\"POST\" id = \"main_form\"> "),
write(" <p><input type=\"hidden\" id=\"dlg_ask\" name = \"dlg_ask\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"msg_to_display\" name = \"msg_to_display\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"menu_item\" name = \"menu_item\" value = \"tmpl_left_display\"> "),
writef(" <p><input type=\"hidden\" id=\"curcnpt_id\" name = \"curcnpt_id\" value = \"%\"> ", NewVersionId),
writef(" <p><input type=\"hidden\" id=\"old_id\" name = \"old_id\" value = \"%\"> ", OldId),
writef(" <p><input type=\"hidden\" id=\"curcnpt_name\" name = \"curcnpt_name\" value = \"%\"> ", Name),
writef(" <p><input type=\"hidden\" id=\"env_id\" name = \"env_id\" value = \"%\"> ", NameEnvId),
writef(" <p><input type=\"hidden\" id=\"curcnpt_text\" name = \"curcnpt_text\" value = \"%\"> ", Defin1Changed),

write(" <p><input type=\"hidden\" id=\"rest\" name = \"rest\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"usedIds\" name = \"usedIds\" value = \""),
get_used_concepts(NewVersionId), 
write("\"> "), 

%writef(" <p><input type=\"hidden\" id=\"mode\" name = \"mode\" value = \"%\"> ", ModeValue),
writef(" <p><input type=\"hidden\" id=\"inset\" name = \"inset\" value = \"%\"> ", Inset),
 
write(" <p><input type=\"hidden\" id=\"toolbar_button\" name = \"toolbar_button\" value = \"123\"> "),
write(" <p><input type=\"hidden\" id=\"explorer_selected_left\" name = \"explorer_selected_left\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"explorer_selected_right\" name = \"explorer_selected_right\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"new_folder_name\" name = \"new_folder_name\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"new_cnpt_name\" name = \"new_cnpt_name\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"cmd\" name = \"cmd\" value = \"\"> "),
writef(" <p><input type=\"hidden\" id=\"user\" name = \"user\" value = \"%\"> ", UserName),
write(" <p><input type=\"hidden\" id=\"button\" onclick = \"submit();\"> "),
write(" </form> "), 
 write(" </body> "),
 write(" </html> "). 


/******************************************************************************************************************/
msgSetup(Text,Text2,LastCMD):-
 location(Location),
 subPath(Location,"editor.exe","MsgSetup/", MsgSetupPath),
 write("Content-type: text/html\n\n"), 
 IDList = ["ch1", "ch2", "ch3", "ch4", "ch5", "ch6", "ch7", "ch8"],
 textlist(TextList),
 statelist(StateList),
 divideList(IDList, IDList1, IDList2),
 divideList(StateList, StateList1, StateList2),
 divideList(TextList, TextList1, TextList2),

   write(" <html> "),
   write(" <head> "),
   %write(" <meta http-equiv=\"pragma\" content=\"nocache\"> "),
   write("<title> Выводить сообщения:</title> "),
   write("<style> "),
   write(".text {font-family: Book Antiqua; font-size:15;font-weight: bolder; color:#336699; } "),
   write(".button {font-family: Book Antiqua; font-size:13; font-weight: bolder;  color:#336699; width:100px;} "),
   write("</style> "),
   writef("<script src = \"%sscript.js\"> ", MsgSetupPath),
   write("</script> "),
   write("</head> "),
   
write(" <body style=\"background-image:url(headbg.jpg); background-repeat:no-repeat;\" > "),

   write("<br><br> "),
   write("<table bordercolor=\"#CCCC99\" border=\"3\" width = \"100%\"> "),
   write("<tr><td width = \"50%\"> "),
   write("<table width = \"100%\"> "),
	
   write_chb_list(IDList1, StateList1, TextList1),

   write("</table></td> "),
   write("<td width = \"50%\"> "),
   write("<table width = \"100%\"> "),
   
   write_chb_list(IDList2, StateList2, TextList2),

   write("</table></td></tr> "),

   write("</table><BR><BR> "),
   write("<center> "),
   write("<input type=button value=\"ОК\" class= \"button\" onclick = \"GetState();\">&nbsp;&nbsp;  "),
   write("<input type=button value=\"Отмена\" class=\"button\" onclick =  \"history.back();\">  "),
   write("</center> "),




get_current_concept_name( Name ),
test_inconsistence(ClassID),
%ifReadonlyNoCnptName(Readonly, ShowSubMenu),  

ifIncons(ClassID),
choose_mode(Name, _ModeLabel, ModeValue),


change(Text,TextChanged),
curid(NewVersionId),
oldid(OldId),
get_current_environment_file(NameEnvId),
get_inset(Inset),

write(" <form action=\"editor.exe\" target=\"_self\" method=\"POST\" id = \"main_form\"> "),
write(" <p><input type=\"hidden\" id=\"dlg_ask\" name = \"dlg_ask\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"msg_to_display\" name = \"msg_to_display\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"menu_item\" name = \"menu_item\" value = \"\"> "),
writef(" <p><input type=\"hidden\" id=\"curcnpt_id\" name = \"curcnpt_id\" value = \"%\"> ", NewVersionId),
writef(" <p><input type=\"hidden\" id=\"old_id\" name = \"old_id\" value = \"%\"> ", OldId),
writef(" <p><input type=\"hidden\" id=\"curcnpt_name\" name = \"curcnpt_name\" value = \"%\"> ", Name),
writef(" <p><input type=\"hidden\" id=\"env_id\" name = \"env_id\" value = \"%\"> ", NameEnvId),
writef(" <p><input type=\"hidden\" id=\"curcnpt_text\" name = \"curcnpt_text\" value = \"%\"> ", TextChanged),
writef(" <p><input type=\"hidden\" id=\"cmd\" name = \"cmd\" value = \"%\"> ", LastCMD), 
writef(" <p><input type=\"hidden\" id=\"rest\" name = \"rest\" value = \"%\"> ", Text2),
write(" <p><input type=\"hidden\" id=\"usedIds\" name = \"usedIds\" value = \""),
get_used_concepts(NewVersionId), 
write("\"> "), 
writef(" <p><input type=\"hidden\" id=\"mode\" name = \"mode\" value = \"%\"> ", ModeValue),
writef(" <p><input type=\"hidden\" id=\"inset\" name = \"inset\" value = \"%\"> ", Inset),
write(" <p><input type=\"hidden\" id=\"toolbar_button\" name = \"toolbar_button\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"explorer_selected_left\" name = \"explorer_selected_left\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"explorer_selected_right\" name = \"explorer_selected_right\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"new_folder_name\" name = \"new_folder_name\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"new_cnpt_name\" name = \"new_cnpt_name\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"button\" onclick = \"submit();\"> "),
write(" </form> "),

write(" </body> "),
write(" </html> ").
      
/**************************************************************************************************************************************************************/

treeCnptPage :-!,
 location(Location),
 kbname(KBName),
 subPath(Location,"editor.exe","TreeCnpt/", TreeCnptPath),
 write("Content-type: text/html\n\n"),
 %write( "http://localhost/treecnpt/index_tree_cnt.htm").

 write(" <html> "),
 write(" <head> "),
 writef(" <title>Дерево папок понятий %</title> ", Location),
 write(" </head> "),
 writef(" <INPUT TYPE=hidden NAME=\"kb_name\" ID = \"kb_name\" value=\"%\">", KBName),
 write(" 	<frameset cols=\"42%,58%\" > "),
 write(" 		<frameset rows = \"80%, 20%\"> "),
 writef(" 			<frame src = \"%tree_cnt.htm\" name=\"tree\" noresize > ",TreeCnptPath),
 writef(" 			<frame src = \"%toolbar.htm\" name=\"toolbar\" noresize > ", TreeCnptPath),
 write(" 		</frameset> "),
 write(" 		<frame src = \"about:blank\" name=\"textarea\"  noresize> "),
 write(" 	</frameset> "),
 write(" </html> ").
 
 treeCnptPage_text_area(Name):-
 	name_file(Name,_File, _FileEnvironment, Defin1),!,
 	text_into_html_page_readonly(Defin1, Name).

 % write "about:blank"
 treeCnptPage_text_area(_Name):-!.
 
 
 
/****************************************************************************************************************************************************************/ 
 
 
 
 
test_inconsistence("cnpterr"):-
	inconsist(_,_,_),!.
test_inconsistence("cnptok"):-!.

ifIncons("cnptok"):-!.
ifIncons(ClassID):-!,
	writef(" <SPAN class = \"%\">",ClassID),
	write(" <IMG src=\"err.bmp\">"),
	write("Ошибка: онтология с противоречиями!"),
	write(" </SPAN> <BR>").

write_readonly_forname("CC_newver"):-!,write("readonly=readonly").
write_readonly_forname("SaveOntology"):-!,write("readonly=readonly").
write_readonly_forname(_):-
		version(""),!,
		write("")
		;
		write("readonly=readonly").

write_readonly_forver("CC_newver"):-!,write("").
write_readonly_forver(_):-!,write("readonly=readonly").


ifReadonly("Новая онтология", "", ""):-!.
ifReadonly(_, "","<li><a href=\"javascript:Build_All();\">Построить всё</a></li><li><a href=\"javascript:Save_Draft();\">Сохранить черновик</a></li><li><a href=\"javascript:SaveOntologyFinalSRV();SaveOntologyFinal();\">Окончание сохранение</a></li>"):-		
	readonly_concept("false"),!.
ifReadonly(_, "readonly=readonly", "<li><a href=\"javascript:Edit_Ontology();\">Редактировать</a></li>"):-!.

ifReadonlyNoCnptName("","<li><a href=\"javascript:Build_All();\">Построить всё</a></li><li><a href=\"javascript:Save_Draft();\">Сохранить черновик</a></li><li><a href=\"javascript:SaveOntologyFinalSRV();SaveOntologyFinal();\">Окончательное сохранение</a></li>"):-		
	readonly_concept("false"),!.
ifReadonlyNoCnptName("readonly=readonly", "<li><a href=\"javascript:NewVersion();\">Создать версию текущей онтологии</a></li>"):-!.

%<li><a href=\"javascript:Build_Complete();\">Достроить</a></li>
%<li><a href=\"javascript:SaveOntologyFinalSRV();SaveOntologyFinal();\">Сохранить онтологию и Завершить редактирование</a></li> 
ifBuilt(Text2,"style=\"display:none;\">"):-
	not(Text2=""),!.
ifBuilt("","style=\"display:inline;\">"):-ifbuilt_concept("true"),!.%если необработанная часть пуста и онтология построена
ifBuilt("","style=\"display:none;\">"):-!.

choose_mode("Новая онтология", "Редактирование","editing"):-!.
choose_mode(_, "Редактирование","editing"):-
	readonly_concept("false"),!.
choose_mode(_, "Просмотр",""):-!.


concat_texts("","",""):-!.
concat_texts(Defin1Changed,"",Defin1Changed):-!.
concat_texts(Defin1Changed,Text2,OntoText):-
	%concat(Defin1Changed,"\n",Defin1ChangedNew),
	concat(Defin1Changed,Text2,OntoText).

/**************************************************************************************************************************************************************/
 
if_nil_answer(nil, ""):-!.
if_nil_answer(Result, Text):-
	expr_to_str(Result,Answer),
       	concat("\n",Answer,Text).
       	%concat(Text1,"\n",Text).
 

write_answer:-
	answer_text(Command_string,Result),
	if_nil_answer(Result, Text2),       	
       	concat(Command_string,Text2,Text3),       	
       	answer_text_all(OldText),
       	concat(OldText,Text3,NewText),
       	retractall(answer_text_all(_)),
       	assert(answer_text_all(NewText)),
       	fail.      	
write_answer:-
	answer_text_all(Text),!,
	change(Text,Chg_Text),
	write(Chg_Text).
write_answer.	



/**************************************************************************************************************************************************************/

text_into_html_page_readonly(Text, Title):-!,
write("Content-type: text/html\n\n"),
 	write(" <html> "),
 	write(" <head> "),
 	write(" <title> "),
 	write(Title),
 	write(" </title> "),
	write(" </head> "),
 	write(" <body> "),
  	write(" <textarea style = \"WIDTH: 100%; HEIGHT: 100%\" readonly=readonly> "),
 	write(Text),
 	write(" </textarea> "),
 	write(" </body> ").
 
/**************************************************************************************************************************************************************/
 	
 
/**************************************************************************************************************************************************************/	

 
 ifFolder(OptionName,"Folder"):-
 	concat(" - [] ", _FolderName, OptionName),!.
 ifFolder(OptionName,"Route"):-
 	concat("   [..]", _FolderName, OptionName),!.
 ifFolder(_,"NotFolder"):-!. 
 
 
 
 
/**************************************************************************************************************************************************************/
 
 cnptExplorerPage(_LeftList,_RightList,LPath,RPath, _CnptText):-
 location(Location),
 subPath(Location,"editor.exe","ConceptExplorer/", ConceptExplorerPath),
 write("Content-type: text/html\n\n"),
 write(" <html> "),
 write(" <head> "),
 write(" <title>Список понятий</title> "),
 write(" <style> "),
 write(" .MenuButton, .MenuButtonWithoutAction, .MenuButtonSelectedLeft, .MenuButtonSelectedRight, .MenuButtonNew, #text {font-family: Book Antiqua; font-size:13; font-weight: bolder;  color:#336699;}"),
 
 writef(" .MenuButton {behavior: URL(%onclick.htc)} ", ConceptExplorerPath),
 
 writef(" .MenuButtonSelectedLeft {behavior: URL(%onclickL.htc)} ", ConceptExplorerPath),
 
 writef(" .MenuButtonSelectedRight {behavior: URL(%onclickR.htc)} ", ConceptExplorerPath),
 
 writef(" .MenuButtonNew { behavior: URL(%onclickNew.htc)} ", ConceptExplorerPath),
 
 writef(" .NotFolder { behavior: URL(%notFolder.htc)} ", ConceptExplorerPath),

 write(" .SelectDec{font-family: Book Antiqua; font-size:15;font-weight: bolder; color:#336699; width:100% } "),

 write(" </style> "),
 writef(" <script src = \"%explorer.js\"> ",ConceptExplorerPath),
 write(" </script> "),
 write(" </head> "),
 write(" <body onload=\"ifDisabled();\"> "),
 write(" <table bordercolor=\"#CCCC99\" border=\"3\"> "),
 write(" <tr> "),
 write(" 	<td width=\"60%\" align=\"center\"><table> "),
 writef(" <tr><td colspan=\"5\" class=\"MenuButtonWithoutAction\" id=\"fldr_left_path\">%</td></tr>", LPath),
 write(" <tr> "),
 write(" 	<td><input type=button id=\"fldr_left_all\" value=\"Все понятия\" class=\"MenuButton\" style=\"WIDTH: 92px;\" ></td> "),
 write(" 	<td><input type=button id=\"fldr_left_create\" value=\"Создать папку\" class=\"MenuButton\" style=\"WIDTH: 106px;\"></td> "),
 write(" 	<td><input type=button id=\"fldr_left_fromfolder\" value=\"Удалить из папки\" class=\"MenuButtonSelectedLeft\" style=\"WIDTH: 142px;\"></td> "),
 write(" 	<td><input type=button id=\"fldr_left_replace\" value=\"Переместить\" class=\"MenuButtonSelectedLeft\" style=\"WIDTH: 96px;\"></td> "),
 write(" 	<td><input type=button id=\"fldr_left_copy\" value=\"Копировать->\" class=\"MenuButtonSelectedLeft\" style=\"WIDTH: 98px;\"></td> "),
 write(" </tr> "),
 write(" <tr> "),
 write(" 	<td colspan=\"5\"> "),
 write(" 	<form> "),
 write(" 	<select size=5 id = \"select_left\" class=\"SelectDec\" ondblclick = \"DblLeft();\" onclick=\"DisplayBottom();\" onchange=\"ChangeSelection(this.options[this.selectedIndex].value, \'explorer_selected_left\');\"> "),
 %write_option_list(LeftList, "left"),
 write(" 	</select> "),
 write(" 	</form> "),
 write(" 	</td> "),
 write(" </tr> "),
 write(" <tr><td colspan=\"5\"><table><tr> "),
 write(" 	<td><form><input type=button id=\"fldr_left_display\" value=\"Показать онтологию\" class=\"MenuButtonWithoutAction\" style=\"WIDTH: 132px;\" "),
 write("onclick=\"DisplayBlank();\"></form></td> "),
 write(" 	<td><form><input type=button id=\"fldr_left_delete\" value=\"Удалить онтологию\" class=\"MenuButtonSelectedLeft\" style=\"WIDTH: 132px;\"></form> </td> "),
 write(" 	<td><form><input type=button id=\"fldr_left_intocur\" value=\"Сделать текущей\" class=\"MenuButtonNew\" style=\"WIDTH: 132px;\" onclick=\"CCInset();\"></form> </td> "),
 write(" 	<td><form><input type=button id=\"fldr_left_intoenv\" value=\"Сделать средой\" class=\"MenuButtonSelectedLeft\" style=\"WIDTH: 132px;\" onclick=\"CCInset();\"></form> </td> "),
 write("</tr></table></td></tr> "),
 write(" 	</table></td> "),

 write(" 	<td width=\"40%\" align=\"center\"><table> "),
 write(" <tr><td colspan=\"3\" class=\"MenuButtonWithoutAction\" id=\"fldr_right_path\">",RPath,"</td></tr>" ),
 write(" <tr> "),
 write(" 	<td><input type=button class=\"MenuButton\" id=\"fldr_right_open\" value=\"<-Открыть\" style=\"WIDTH: 80px;\"></td> "),
 write(" 	<td><input type=button class=\"MenuButton\" id=\"fldr_right_create\" value=\"Созадть папку\" style=\"WIDTH: 102px;\"></td> "),
 write(" 	<td><input type=button class=\"MenuButtonSelectedRight\" id=\"fldr_right_delete\" value=\"Удалить\" style=\"WIDTH: 80px;\"></td> "),
 write(" </tr> "),
 write(" <tr> "),
 write(" 	<td colspan=\"3\"> "),
 write(" 	<form> "),
 write(" 	<select size=7 id = \"select_right\" class=\"SelectDec\" ondblclick = \"DblRight();\" onchange=\"ChangeSelection(this.options[this.selectedIndex].value,\'explorer_selected_right\');\"> "),
 %write_option_list(RightList, "right"),	   
 write(" 	</form> "),
 write(" 	</td> "),
 write(" </tr> "),
 write(" 	</table></td> "),
 write(" </tr></table> "),
 write(" <SPAN id=\"text\">Текст понятия:</SPAN> "),
 write(" <iframe style = \"WIDTH: 100%\" id=\"exp_concept_text\" name = \"exp_concept_text\" src=\"about:blank\"></iframe>"),
/* 
 write(" <textarea style = \"WIDTH: 100%; HEIGHT: 100%\"> "),
 write(CnptText),
 write(" </textarea> "),
 */
 /***************/
 %close_msg_file,
 /**************/
 write(" </body> "),
 write(" </html> ").
 
/**************************************************************************************************************************************************************/
 
tmplExplorerPage(LeftList,_RightList,LPath,_RPath, _CnptText):-
% location(Location),
% subPath(Location,"editor.exe","TemplateExplorer/", _), %TemplateExplorerPath),
 write("Content-type: text/html\n\n"),
 write(" <html> "),
 write(" <head> "),
 write(" <title>Список понятий</title> "),
 write(" <link rel=\"Stylesheet\" type=\"text/css\" href=\"Start/css.css\" /> "),
  write(" <style> "),
 write(" .MenuButton, .MenuButtonWithoutAction, .MenuButtonSelectedLeft, #text {font-family: Book Antiqua; font-size:13; font-weight: bolder;  color:#336699;}"),
 write(" .SelectDec{font-family: Book Antiqua; font-size:15;font-weight: bolder; color:#336699; width:100% } "),
 write(" </style> "), 
% writef(" <script src = \"%explorer.js\"> ",TemplateExplorerPath),
% write(" </script> "), 
/* 
 write(" <script> "),
 write(" function fnChange(){ "),
 write("   if (templatesList.options[templatesList.selectedIndex].value!=\"\"){     "),
 write("  exp_concept_text.value=templatesList.options[templatesList.selectedIndex].value; "),
 write("   } "),
 write("   else {  "),
 write("   exp_concept_text.value=\"Описание для этого шаблона ещё не определено.\"; "),
 write("   }   "), 
 write(" } "),	
 write(" </script> "), 
*/
 write(" <script language=\"javascript\"> "),
 write(" function fnChange(){ "),
 write("  document.getElementById(\"explorer_selected_left\").value = document.getElementById(\"tmpl_select_left\").options[document.getElementById(\"tmpl_select_left\").selectedIndex].value; "),
 write("  document.getElementById(\"explorer_selected_right\").value = document.getElementById(\"tmpl_select_left\").options[document.getElementById(\"tmpl_select_left\").selectedIndex].value; "),
% tmpl_left_display
 write("  main_form.submit(); "),
 write(" } "),	
 write(" </script> "), 

 
 
% write(" <body onload=\"ifDisabled();\"> "),
 write(" <body> "),
 write(" <table bordercolor=\"#CCCC99\" border=\"3\" width = \"100%\"> "),
 write(" <tr > "),
 write(" 	<td align=\"center\">"),
 write(" <table width = \"100%\"> "),
 writef(" <tr><td  class=\"MenuButtonWithoutAction\" id=\"fldr_left_path\">%</td></tr>", LPath),
 write(" <tr> "),
 write(" 	<td > "),
 write(" 	<form> "),
  write(" 	<select size=16 id = \"tmpl_select_left\" class=\"SelectDec\" onchange=\"fnChange();\"> "),
% write(" 	<select size=8 id = \"tmpl_select_left\" class=\"SelectDec\" onclick=\"DisplayBottom();\" onchange=\"ChangeSelection(this.options[this.selectedIndex].value, \'explorer_selected_left\');\"> "),
% write(" 	<select size=8 id = \"templatesList\" name = \"templatesList\" class=\"SelectDec\" onchange=\"fnChange();\"> "),	


 write_option_list(LeftList, "left"),
 write(" 	</select> "),
 write(" 	</form> "),
 write(" 	</td> "),
 write(" </tr> "),
 write("</table></td></tr> "),
 write(" 	</table> "),

/*
write(" <textarea id=\"field_for_templ\" height=\"150px\" > "),
send_templates_list ("DIC_all_ext_templ", Res),
write(Res),
write(" </textarea> "),
	*/
	
 write(" <SPAN id=\"text\">Комментарии:</SPAN> "),
 write(" <iframe  style = \"WIDTH: 100%; height: 45%\" id=\"exp_concept_text\" name = \"exp_concept_text\" src=\"about:blank\"></iframe> "),



get_current_concept_name(Name),
curid(NewVersionId),
oldid(OldId),
get_current_environment_file(NameEnvId),
get_inset(Inset),  
get_defin1(Defin1),
change(Defin1,Defin1Changed),
choose_mode(Name, _ModeLabel, ModeValue),
  
write(" <form action=\"editor.exe\" target=\"exp_concept_text\" method=\"POST\" id = \"main_form\"> "),
write(" <p><input type=\"hidden\" id=\"dlg_ask\" name = \"dlg_ask\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"msg_to_display\" name = \"msg_to_display\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"menu_item\" name = \"menu_item\" value = \"tmpl_left_display\"> "),
writef(" <p><input type=\"hidden\" id=\"curcnpt_id\" name = \"curcnpt_id\" value = \"%\"> ", NewVersionId),
writef(" <p><input type=\"hidden\" id=\"old_id\" name = \"old_id\" value = \"%\"> ", OldId),
writef(" <p><input type=\"hidden\" id=\"curcnpt_name\" name = \"curcnpt_name\" value = \"%\"> ", Name),
writef(" <p><input type=\"hidden\" id=\"env_id\" name = \"env_id\" value = \"%\"> ", NameEnvId),
writef(" <p><input type=\"hidden\" id=\"curcnpt_text\" name = \"curcnpt_text\" value = \"%\"> ", Defin1Changed),
%write(" <p><input type=\"hidden\" id=\"cmd\" name = \"cmd\" value = \"\"> "), 
write(" <p><input type=\"hidden\" id=\"rest\" name = \"rest\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"usedIds\" name = \"usedIds\" value = \""),
get_used_concepts(NewVersionId), 
write("\"> "), 
%writef(" <p><input type=\"hidden\" id=\"mode\" name = \"mode\" value = \"%\"> ", ModeValue),
writef(" <p><input type=\"hidden\" id=\"inset\" name = \"inset\" value = \"%\"> ", Inset),
write(" <p><input type=\"hidden\" id=\"toolbar_button\" name = \"toolbar_button\" value = \"123\"> "),
write(" <p><input type=\"hidden\" id=\"explorer_selected_left\" name = \"explorer_selected_left\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"explorer_selected_right\" name = \"explorer_selected_right\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"new_folder_name\" name = \"new_folder_name\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"new_cnpt_name\" name = \"new_cnpt_name\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"cmd\" name = \"cmd\" value = \"\"> "),
write(" <p><input type=\"hidden\" id=\"button\" onclick = \"submit();\"> "),
write(" </form> "), 
 write(" </body> "),
 write(" </html> "). 
 
/**************************************************************************************************************************************************************/ 

write_option_list([H|T],Mode):-!,
 	change(H,Hpr),
 	write_option(Hpr,Mode),
 	write_option_list(T, Mode).
 write_option_list([], _Mode):-!.
 
 %write_option (_,_):-!.
 write_option(Option, "right"):-!,
 	writef("<option value = \"%\" >",Option),
 	write(Option).
 write_option(Option, "left"):-!,
       	 ifFolder(Option, ClassName),
 	 writef("<option value = \'%\' class = \"%\">",Option, ClassName),
 	 write(Option).

write_option_list2([H|T],Mode):-!,
 	change(H,Hpr),
 	write_option2(Hpr,Mode),
 	write_option_list2(T, Mode).
 write_option_list2([], _Mode):-!.
 

 write_option2(Option, "left"):-!,
       	 write(Option),
       	 write("\n").
/*
 write_option_list([H|T],Mode):-!,
 	write_option(H,Mode),
 	write_option_list(T, Mode).
 write_option_list([], _Mode):-!.
 
 write_option ([Templ,TemplText], "left"):-!,
 	ifFolder (Templ, ClassName),
 	 writef(" 	<option value = \"%\" class = \"%\">", TemplText, ClassName),
 	 write(Templ).
	 
 write_option ([Templ,TemplText], "right"):-!,
 	writef(" 	<option value = \"%\" >",TemplText),
 	write(Templ).

*/








/**************************************************************************************************************************************************************/ 

fatalErrorMsg(ErrorMessage):-
 	write("Content-type: text/html\n\n"),
 	write(" <html> "),
 	write(" <head> "),
	%write(" <meta http-equiv=\"content-type\" content=\"text/html; charset=windows-1251\"> "),
	write_charset,
	write(" <LINK REL=\"stylesheet\" TYPE=\"text/css\" HREF=\"Start/css.css\"> "),
	write(" </head> "),
	write(" <body> "),
	write(" <CENTER> "),
	write(" <BR><BR> "),
	write(" <IMG SRC = \"err.bmp\"><SPAN class=\"error\">Ошибка!</SPAN><BR><BR> "),
	write(" <SPAN class=\"fatal\">"),
	write(ErrorMessage),
	write(" </SPAN><BR><BR> "),
	write(" <INPUT TYPE=BUTTON VALUE=\"OK\" class=\"button\" style = \"width:142px;\" ONCLICK = \"history.back();\"> "),
	write(" </CENTER> "),
	write(" </body> "),
	write(" </html> "),!.
fatalErrorMsg(_ErrorMessage).

dlgMsg(Message):-!,
 	write(" <html> "),
 	write(" <head> "),
 	%write(" <meta http-equiv=\"content-type\" content=\"text/html; charset=windows-1251\"> "),
 	write_charset,
	write(" <LINK REL=\"stylesheet\" TYPE=\"text/css\" HREF=\"Start/css.css\"> "),
	write(" </head> "),
	write(" <body> "),
	write(" <CENTER> "),
	write(" <BR><BR> "),
	write(" <IMG SRC = \"err.bmp\"><SPAN class=\"error\">Сообщение!</SPAN><BR><BR> "),
	write(" <SPAN class=\"fatal\">"),
	write(Message),
	write(" </SPAN><BR><BR> "),
	write(" <INPUT TYPE=BUTTON VALUE=\"OK\" class=\"button\" style = \"width:142px;\" ONCLICK = \"history.back();\"> "),
	write(" </CENTER> "),
	write(" </body> "),
	write(" </html> ").

	
dlgAsk(Question):-!,
 	write("Content-type: text/html\n\n"),
 	write(" <html> "),
 	write(" <head> "),
	write(" <LINK REL=\"stylesheet\" TYPE=\"text/css\" HREF=\"main menu/css.css\"> "),
	write(" <script src =\"mainmenu.js\"></script> "),
	write(" </head> "),
	write(" <body> "),
	write(" <CENTER> "),
	write(" <BR><BR> "),
	write(" <IMG SRC = \"att.bmp\"><SPAN class=\"error\">Внимание!</SPAN><BR><BR> "),
	write(" <SPAN class=\"fatal\">"),
	write(Question),
	write(" </SPAN><BR><BR> "),
	write(" <INPUT TYPE=BUTTON VALUE=\"Да\" class=\"button\" style = \"width:100px;\" ONCLICK = \"Answer(\'yes\');\">&nbsp;&nbsp; "),
	%write(" <INPUT TYPE=BUTTON VALUE=\"Нет\" class=\"button\" style = \"width:100px;\" ONCLICK = \"Answer(\'no\');\"> &nbsp;&nbsp;"),
	write(" <INPUT TYPE=BUTTON VALUE=\"Нет\" class=\"button\" style = \"width:100px;\" ONCLICK =  \"history.back();\"> &nbsp;&nbsp;"),
	write(" <INPUT TYPE=BUTTON VALUE=\"Отмена\" class=\"button\" style = \"width:100px;\" ONCLICK = \"history.back();\"> "),
	write(" </CENTER> "),
	write(" </body> "),
	write(" </html> ").
	
dlgAsk_eq(Question):-!,
 	write("Content-type: text/html\n\n"),
 	write(" <html> "),
 	write(" <head> "),
	write(" <LINK REL=\"stylesheet\" TYPE=\"text/css\" HREF=\"main menu/css.css\"> "),
	write(" <script src =\"mainmenu.js\"></script> "),
	write(" </head> "),
	write(" <body> "),
	write(" <CENTER> "),
	write(" <BR><BR> "),
	write(" <IMG SRC = \"att.bmp\"><SPAN class=\"error\">Внимание!</SPAN><BR><BR> "),
	write(" <SPAN class=\"fatal\">"),
	write(Question),
	write(" </SPAN><BR><BR> "),
	write(" <INPUT TYPE=BUTTON VALUE=\"Да\" class=\"button\" style = \"width:100px;\" ONCLICK = \"Answer(\'yes\');\">&nbsp;&nbsp; "),
	write(" <INPUT TYPE=BUTTON VALUE=\"Нет\" class=\"button\" style = \"width:100px;\" ONCLICK = \"Answer(\'no\');\"> &nbsp;&nbsp;"),
	%write(" <INPUT TYPE=BUTTON VALUE=\"Нет\" class=\"button\" style = \"width:100px;\" ONCLICK =  \"history.back();\"> &nbsp;&nbsp;"),
	write(" <INPUT TYPE=BUTTON VALUE=\"Отмена\" class=\"button\" style = \"width:100px;\" ONCLICK = \"history.back();\"> "),
	write(" </CENTER> "),
	write(" </body> "),
	write(" </html> ").
write_chb_list([IDH|IDT], ["1"|StateT], [TextListH|TextListT]):-
	write(" <TR><TD class = \"text\"> "),
	writef(" <INPUT type=checkbox ID = \"%\" CHECKED = true>% ", IDH, TextListH),
	write(" </TD></TR> "),
	write_chb_list(IDT, StateT, TextListT).
write_chb_list([IDH|IDT], ["0"|StateT], [TextListH|TextListT]):-
	write(" <TR><TD class = \"text\"> "),
	writef(" <INPUT type=checkbox ID = \"%\">% ", IDH, TextListH),
	write(" </TD></TR> "),
	write_chb_list(IDT, StateT, TextListT).
write_chb_list([],[],[]):-!.

divideList(ListIn, ListOut1, ListOut2):-
	divide_list_help(ListIn, ListOut1,ListOut2, 0).

divide_list_help( [ListInH|ListInT], [ListInH| ListOut1T], ListOut2, N):-
	NN = N+1,
	divide_list_help(ListInT, ListOut1T, ListOut2, NN),!.
divide_list_help(List,[], List, 4):-!.

   
 indexPage:-
  %location (Location),
  %subPath(Location,"editor.exe","Start/", StartPath),
  write("Content-type: text/html\n\n"), 
  write(" <head> "),
  write(" <html> "),
  write(" <title>Система представления понятий ЭЗОП</title> "),
  %write(" <meta http-equiv=\"content-type\" content=\"text/html; charset=windows-1251\"> "),
  write_charset,
  write(" <LINK REL=\"stylesheet\" TYPE=\"text/css\" HREF=\"css.css\"> "),
  write(" </head> "),
  write(" <FRAMESET ROWS = \"168, *, 100\" border=\"0\" > "),
  write("  	<FRAME SRC = \"Start\\op.html\"   NORESIZE  NAME=\"start_title\" scrolling = \"no\" marginwidth=\"0\" marginheight=\"0\" align = \"middle\"> "),
  write("   	<FRAME SRC = \"kbmenu.htm\" NAME=\"start_content\"> "),

  write("  	<FRAME SRC = \"Start\\bottom.html\"   NORESIZE  NAME=\"start_bottom\" scrolling = \"no\" marginwidth=\"0\" marginheight=\"0\" align = \"middle\" > "),
  write(" </FRAMESET> "),
  write(" </html> ").
  
  
  