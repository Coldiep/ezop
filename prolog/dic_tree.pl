/*****************************************************************************

		Copyright (c) 2003 My Company

 Project:  EDITOR
 FileName: DIC_TREE.PRO
 Purpose: Windos Ezop
 Written by: E. Beniaminov
 Comments:
******************************************************************************/


:- module(dic_tree, [
create_xml_file/0
]).
:- style_check(+string).

:- use_module(vp52_compat).
:- use_module(editor).
:- use_module(db).

%include "editor.pre"




paren(V1,V2):-dir_dc(V1, V2).
paren(V1,V2):-dir_c(V1, V2).

paren(V1,V2):- name_file(V1,N_V1,_,_),
		name_file(V2,_,N_V1,_),
		V1<>V2.
		
folder(V1):-dir_dc(V1, _V2),!.
folder(V2):-dir_dc(_V1, V2),!.
folder(V1):-dir_c(V1, _V2),!.		
obj(C):- not(env(C)),name_file(C,_,_,_),!.
env(V1):-name_file(V1,FileEnvironment,_,_),
	name_file(_V2,_File,FileEnvironment,_Defin1),!.


order_string(List,OrderList):-order_string_aux(List,[],OrderList).

order_string_aux([],L,L):-!.
order_string_aux([H|T],Order,Res):-
		insert(H, Order,Order1),
		order_string_aux(T,Order1,Res).

insert(H, [],[H]):-!.
insert(H, [H1|T],[H,H1|T]):-H<H1,!.
insert(H, [H1|T],[H1|T1]):-
	insert(H, T,T1).

children(S,List):-findall(X,paren(S,X),List1),
			order_string(List1,List2),
			del_element(S,List2,List).
del_element(_,[],[]):-!.
del_element(S,[S|R],R):-!.
del_element(S,[H|R1],[H|R]):-
	del_element(S,R1,R).
%(Anna

increment_id(NewID):-
	id_4_xml(ID),
	NewID = ID +1,
	assert(id_4_xml(NewID)).
%Anna)
%(Anna
write_subdir_tag(Name, PictureType):-
		increment_id(ID),
		writef("<subdir name = \"%s\" id = \"%\" pictureType = \"%s\" >\n", Name, ID, PictureType),!.
%write_subdir_tag(Name, PictureType):-
%		name_file(Name,_File, _FileEnvironment, Defin1),!,
%		writef("<subdir name = \"%s\" pictureType = \"%s\" defin = \"%s\" >\n", Name, PictureType, Defin1).
%write_subdir_tag(Name, PictureType):-
%		writef("<subdir name = \"%s\" pictureType = \"%s\" defin = \"%s\" >\n", Name, PictureType, ""),
%		!.

create_xml(S):-
		folder(S),
                 obj(S),
                 children(S,List),List=[_|_],!,
                 write_subdir_tag(S,"folder_paper"),
                 create_xml_list(List),
                 write("</subdir>\n").
                 
create_xml(S):-
		folder(S),
                 children(S,List),List=[_|_],!,
                 write_subdir_tag(S,"folder"),
                 create_xml_list(List),
                 write("</subdir>\n").
create_xml(S):-
		folder(S),!,
		%leaf case
		write_subdir_tag(S,"folder"),
                 write("</subdir>\n").
create_xml(S):-
		env(S),!,
		children(S,List),List=[_|_],
		write_subdir_tag( S,"envpaper"),
                 create_xml_list(List),
                 write("</subdir>\n").
create_xml(S):-
		obj(S),!,
		%leaf case
		write_subdir_tag( S,"paper"),
                 write("</subdir>\n").


create_xml_list([]):-!.
create_xml_list([H|T]):-
	create_xml(H),
	create_xml_list(T).
	
create_xml_file :-
		children("Папки понятий", List),
		List = [_|_],!,
		writedevice(OldWriteDevice),
		
		% TO CHANGE DIRECTORY OF CREATED FILE
		
		openwrite(myfile,"treecnpt\\dir.xml"),
		%openwrite(myfile,"dir.xml"),
		%openwrite(myfile,"http://localhost/treecnpt/dir.xml"),
		
		
		writedevice(myfile),
		%Writing the beginning of the XML file
		writef("<?xml version = \"%s\" encoding = \"%s\" ?>\n","1.0","WINDOWS-1251"),
		write("<?xml-stylesheet type='text/xsl' href='dir.xsl'?>\n"),
		write("<catalog>\n"),
		writef("\t<subdir name = \"%s\" id =\"0\">\n", "catalog"),
		%Creating body
		create_xml_list(List),
		write("\t</subdir>\n"),
		write("</catalog>\n"),
		closefile(myfile),
		writedevice(OldWriteDevice).
		%End of the file	
create_xml_file :- % case of empty concept tree
		writedevice(OldWriteDevice),
		
		% TO CHANGE DIRECTORY OF CREATED FILE
		
		openwrite(myfile,"treecnpt\\dir.xml"),
		%openwrite(myfile,"dir.xml"),
		
		writedevice(myfile),
		%write(OldWriteDevice),
		closefile(myfile),
		writedevice(OldWriteDevice).
%Anna)

