
:- module(db,[
define_db/0,

     idconcept/1,
     in_template/8,
     exec_templ/5,
     del_template/4,
     used_concept/3,
     element_concept/4,
     resalt_op/3,
     rwt_rule/8,
     descriptor/3,
     not_eq_desc/3,
     subobject/3,
     not_subobject/3,
     element/3,
     not_element/3,
     current_inconsist/1,
     inconsist/3,
     defin1/2,
     defin2/2,
     last_cmd/2,

location/1, %(string)
kbname/1, % (string)
text_to_execute/2, % (string, string)
dialog_answer/1, % (string)
new_cnpt_name/1, % (string)
readonly_concept/1, % (string)
ifbuilt_concept/1,
curid/1, % (string)
oldid/1, % (string)
my_message/1, % (string)
version/1,
inset/1, %(string)

%GLOBAL DATABASE - fordebugging
parToCheck/1,


% GLOBAL DATABASE - cgiParms
parm/2,

%GLOBAL DATABASE - globalParams
location/1,
kbname/1,
text_to_execute/2,
dialog_answer/1,
new_cnpt_name/1,
readonly_concept/1,
ifbuilt_concept/1,
curid/1,
oldid/1,
my_message/1,
version/1,
inset/1,

%GLOBAL DATABASE - msg_list
textlist/1,
typelist/1,
statelist/1,

%Global DATABASE - notion_base
% notion_base/1,

%GLOBAL DATABASE - concept_names
name_file/4,
use/3,
ver/2,
dir_c/2,
dir_dc/2,
dir_con/1,
dir_papki/1,

%Facts - dictionaries
 word_dictionary/3,

%Facts - sintax  %a?aiaiiua aaiiua aey ?acai?a i?aaei?aiey
 word_of_sentence/4,
 restText/1,
 lenText/1,
 lastTerm/2,

%GLOBAL DATABASE - execute_state
 done/1,
 fatal_error/0,
 execute_command_error/0,
 state_interpret_well/0,
 glob_int/1,
 glob_ilist/1,
 glob_slist/1,
 glob_string/1,
 mode_answer/0,
 marker_present/0,
 answer_text/2,
 answer_text_all/1,

%GLOBAL DATABASE - config
 tmessage/3,
 deduce_on_approx/1,
 dlg_ask_do_element/1,
 maxlong_deduce/1,

%GLOBAL DATABASE - message_ezp
 msg_text/2,


%GLOBAL DATABASE - modi    /* a ii?iaeuiii ninoiyiee yoa aaca ionoa */
  eq_desc/2,
  modification_resalt_op/2,

%global facts -synonym
        synonym_desc/2,


%GLOBAL DATABASE - status
 concept_changed/0,
 current_concept_name/1,

%GLOBAL DATABASE - templates
 template/7,
 dir_dtt/3,
 dir_tt/4,
 dir_con_tt/1,
 dir_papki_tt/1,

%GLOBAL DATABASE - id_4_xml
  id_4_xml/1,

%Global DATABASE - dic_windows
 dic_window/1,
 dir_dt/2,
 dir_t/3,
 dir_con_t/1,
 dir_papki_t/1,

%global facts - dic_templates
  dic/1,
  
%global facts -counts
  count_l/1,
%global facts - tmp_concept
  t/1




]).
:- style_check(+string).


:- use_module(database).


:- dynamic
     idconcept/1,
     in_template/8,
     exec_templ/5,
     del_template/4,
     used_concept/3,
     element_concept/4,
     resalt_op/3,
     rwt_rule/8,
     descriptor/3,
     not_eq_desc/3,
     subobject/3,
     not_subobject/3,
     element/3,
     not_element/3,
     current_inconsist/1,
     inconsist/3,
     defin1/2,
     defin2/2,
     last_cmd/2,

location/1, %(string)
kbname/1, % (string)
text_to_execute/2, % (string, string)
dialog_answer/1, % (string)
new_cnpt_name/1, % (string)
readonly_concept/1, % (string)
ifbuilt_concept/1,
curid/1, % (string)
oldid/1, % (string)
my_message/1, % (string)
version/1,
inset/1, %(string)

parToCheck/1,
% GLOBAL DATABASE - cgiParms
parm/2,
%GLOBAL DATABASE - globalParams
location/1,
kbname/1,
text_to_execute/2,
dialog_answer/1,
new_cnpt_name/1,
readonly_concept/1,
ifbuilt_concept/1,
curid/1,
oldid/1,
my_message/1,
version/1,
inset/1,

%GLOBAL DATABASE - msg_list
textlist/1,
typelist/1,
statelist/1,

%Global DATABASE - notion_base
notion_base/1,

%GLOBAL DATABASE - concept_names
name_file/4,
use/3,
ver/2, %(string IdConceptMain, string IdConceptVersion)
dir_c/2,
dir_dc/2,
dir_con/1,
dir_papki/1,

%Facts - dictionaries
 word_dictionary/3,

%Facts - sintax  %a?aiaiiua aaiiua aey ?acai?a i?aaei?aiey
 word_of_sentence/4,
 restText/1,
 lenText/1,
 lastTerm/2,

%GLOBAL DATABASE - execute_state
 done/1,
 fatal_error/0,
 execute_command_error/0,
 state_interpret_well/0,
 glob_int/1,
 glob_ilist/1,
 glob_slist/1,
 glob_string/1,
 mode_answer/0,
 marker_present/0,
 answer_text/2,
 answer_text_all/1,


%GLOBAL DATABASE - config
 tmessage/3,
 deduce_on_approx/1,
 dlg_ask_do_element/1,
 maxlong_deduce/1,

%GLOBAL DATABASE - message_ezp
 msg_text/2,


%GLOBAL DATABASE - modi    /* a ii?iaeuiii ninoiyiee yoa aaca ionoa */
  eq_desc/2,
  modification_resalt_op/2,

%global facts -synonym
        synonym_desc/2,


%GLOBAL DATABASE - status
 concept_changed/0,
 current_concept_name/1,

%GLOBAL DATABASE - templates
 template/7,
 dir_dtt/3,
 dir_tt/4,
 dir_con_tt/1,
 dir_papki_tt/1,

%GLOBAL DATABASE - id_4_xml
  id_4_xml/1,

%Global DATABASE - dic_windows
 dic_window/1,
 dir_dt/2,
 dir_t/3,
 dir_con_t/1,
 dir_papki_t/1,

%global facts - dic_templates
  dic/1,

%global facts -counts
  count_l/1,
%global facts - tmp_concept
  t/1

.


define_db :- !,

  define_database_module('db'),
  define_single([readonly_concept/1, ifbuilt_concept/1, textlist/1, typelist/1, statelist/1, notion_base/1,id_4_xml/1]),
  define_determ([
        location/1,
        kbname/1,
        dialog_answer/1,
        new_cnpt_name/1,
        curid/1,
        oldid/1,
        inset/1,
        glob_int/1,
        glob_ilist/1,
        glob_string/1,
        answer_text/2,
        answer_text_all/1,
        current_concept_name/1,
        deduce_on_approx/1,
        dlg_ask_do_element/1,
        maxlong_deduce/1,
        environment/1,
        dir_con_tt/1,
        dir_papki_tt/1,
        dir_con/1,
        dir_papki/1,
        idconcept/1,
        restText/1,
        lenText/1,
        lastTerm/2,
        count_l/1]),

  define_database('fordebugging', [ parToCheck/1 ] ),
  define_database('cgiParms', [ parm/2 ] ),

  define_database('concept', [
     idconcept/1,
     in_template/8,
     exec_templ/5,
     del_template/4,
     used_concept/3,
     element_concept/4,
     resalt_op/3,
     rwt_rule/8,
     descriptor/3,
     not_eq_desc/3,
     subobject/3,
     not_subobject/3,
     element/3,
     not_element/3,
     current_inconsist/1,
     inconsist/3,
     defin1/2,
     defin2/2,
     last_cmd/2]),
     
  define_database('message_ezp', [msg_text/2]),
  define_database('templates', [template/7, dir_dtt/3, dir_tt/4, dir_con_tt/1, dir_papki_tt/1]),
  define_database('config', [tmessage/3, deduce_on_approx/1, dlg_ask_do_element/1, maxlong_deduce/1]),
  define_database('concept_names', [name_file/4, use/3, ver/2, dir_c/2, dir_dc/2, dir_con/1, dir_papki/1]),
  define_database('counts', [count_l/1]),
  define_database('execute_state', [done/1,fatal_error/0,execute_command_error/0,state_interpret_well/0, glob_int/1, glob_ilist/1,
    glob_slist/1, glob_string/1, mode_answer/0, marker_present/0, answer_text/2]),
  define_database('sintax', [word_of_sentence/4,restText/1, lenText/1, lastTerm/2]),
  define_database('dictionaries', [ word_dictionary/3]),
  define_database('dic_templates', [dic/1]),
  
  define_database('tmp_concept', [t/1])


.





%notion_base("kernel.ezp").
id_4_xml(0).
readonly_concept("true").
typelist([err, did, warning, diagn, gr_an, calc, approx, rwrt]).
textlist(["Ошибка", "Сделано", "Предупреждение", "Диагностика", "Грамматический анализ","Вычисление","Изменение в аппроксимации","Правило переписывания"]).
statelist(["1","1","1","1","1","1","1","0"]).
