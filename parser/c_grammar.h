


/****************************************************************************************************************
C language grammar

Terminals:

D      [0-9]
L      [a-zA-Z_]
H      [a-fA-F0-9]
E      [Ee][+-]?{D}+
FS      (f|F|l|L)
IS      (u|U|l|L)*

IDENTIFIER      [_A-Za-z][_A-Za-z0-9]*
CONSTANT      (0[xX]{H}+{IS}?)|(0[xX]{H}+{IS}?)|(0{D}+{IS}?)|(0{D}+{IS}?)|({D}+{IS}?)|({D}+{IS}?)
          ('(\\.|[^\\'])+')|({D}+{E}{FS}?)|({D}*"."{D}+({E})?{FS}?)|({D}+"."{D}*({E})?{FS}?)
STRING_LITERAL    "(.|[^"])*"
SIZEOF        "sizeof"

PTR_OP
INC_OP
DEC_OP
LEFT_OP
RIGHT_OP
LE_OP
GE_OP
EQ_OP
NE_OP
AND_OP
OR_OP

MUL_ASSIGN
DIV_ASSIGN
MOD_ASSIGN
ADD_ASSIGN
SUB_ASSIGN
LEFT_ASSIGN
RIGHT_ASSIGN
AND_ASSIGN
XOR_ASSIGN
OR_ASSIGN

TYPE_NAME    

TYPEDEF      'typedef"
EXTERN      "extern"
STATIC      "static"
AUTO      "auto"
REGISTER    "register"

CHAR      "char"
SHORT      "short"
INT        "int"
LONG      "long"
SIGNED      "signed"
UNSIGNED    "unsigned"
FLOAT      "float"
DOUBLE      "double"
CONST      "const"
VOLATILE    "volatile"
VOID      "void"

STRUCT      "struct"
UNION      "union"
ENUM      "enum"
ELIPSIS      \.\.\.
RANGE      

CASE      "case"
DEFAULT      "default"
IF        "if"
ELSE      "else"
SWITCH      "switch"
WHILE      "while"
DO        "do"
FOR        "for"
GOTO      "goto"
CONTINUE    "continue"
BREAK      "break"
RETURN      "return"

Grammar:

start nonterminal: file

primary_expr
  : identifier
  | CONSTANT
  | STRING_LITERAL
  | '(' expr ')'
  ;

postfix_expr
  : primary_expr
  | postfix_expr '[' expr ']'
  | postfix_expr '(' ')'
  | postfix_expr '(' argument_expr_list ')'
  | postfix_expr '.' identifier
  | postfix_expr PTR_OP identifier
  | postfix_expr INC_OP
  | postfix_expr DEC_OP
  ;

argument_expr_list
  : assignment_expr
  | argument_expr_list ',' assignment_expr
  ;

unary_expr
  : postfix_expr
  | INC_OP unary_expr
  | DEC_OP unary_expr
  | unary_operator cast_expr
  | SIZEOF unary_expr
  | SIZEOF '(' type_name ')'
  ;

unary_operator
  : '&'
  | '*'
  | '+'
  | '-'
  | '~'
  | '!'
  ;

cast_expr
  : unary_expr
  | '(' type_name ')' cast_expr
  ;

multiplicative_expr
  : cast_expr
  | multiplicative_expr '*' cast_expr
  | multiplicative_expr '/' cast_expr
  | multiplicative_expr '%' cast_expr
  ;

additive_expr
  : multiplicative_expr
  | additive_expr '+' multiplicative_expr
  | additive_expr '-' multiplicative_expr
  ;

shift_expr
  : additive_expr
  | shift_expr LEFT_OP additive_expr
  | shift_expr RIGHT_OP additive_expr
  ;

relational_expr
  : shift_expr
  | relational_expr '<' shift_expr
  | relational_expr '>' shift_expr
  | relational_expr LE_OP shift_expr
  | relational_expr GE_OP shift_expr
  ;

equality_expr
  : relational_expr
  | equality_expr EQ_OP relational_expr
  | equality_expr NE_OP relational_expr
  ;

and_expr
  : equality_expr
  | and_expr '&' equality_expr
  ;

exclusive_or_expr
  : and_expr
  | exclusive_or_expr '^' and_expr
  ;

inclusive_or_expr
  : exclusive_or_expr
  | inclusive_or_expr '|' exclusive_or_expr
  ;

logical_and_expr
  : inclusive_or_expr
  | logical_and_expr AND_OP inclusive_or_expr
  ;

logical_or_expr
  : logical_and_expr
  | logical_or_expr OR_OP logical_and_expr
  ;

conditional_expr
  : logical_or_expr
  | logical_or_expr '?' logical_or_expr ':' conditional_expr
  ;

assignment_expr
  : conditional_expr
  | unary_expr assignment_operator assignment_expr
  ;

assignment_operator
  : '='
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | ADD_ASSIGN
  | SUB_ASSIGN
  | LEFT_ASSIGN
  | RIGHT_ASSIGN
  | AND_ASSIGN
  | XOR_ASSIGN
  | OR_ASSIGN
  ;

expr
  : assignment_expr
  | expr ',' assignment_expr
  ;

constant_expr
  : conditional_expr
  ;

declaration
  : declaration_specifiers ';'
  | declaration_specifiers init_declarator_list ';'
  ;

declaration_specifiers
  : storage_class_specifier
  | storage_class_specifier declaration_specifiers
  | type_specifier
  | type_specifier declaration_specifiers
  ;

init_declarator_list
  : init_declarator
  | init_declarator_list ',' init_declarator
  ;

init_declarator
  : declarator
  | declarator '=' initializer
  ;

storage_class_specifier
  : TYPEDEF
  | EXTERN
  | STATIC
  | AUTO
  | REGISTER
  ;

type_specifier
  : CHAR
  | SHORT
  | INT
  | LONG
  | SIGNED
  | UNSIGNED
  | FLOAT
  | DOUBLE
  | CONST
  | VOLATILE
  | VOID
  | struct_or_union_specifier
  | enum_specifier
  | TYPE_NAME
  ;

struct_or_union_specifier
  : struct_or_union identifier '{' struct_declaration_list '}'
  | struct_or_union '{' struct_declaration_list '}'
  | struct_or_union identifier
  ;

struct_or_union
  : STRUCT
  | UNION
  ;

struct_declaration_list
  : struct_declaration
  | struct_declaration_list struct_declaration
  ;

struct_declaration
  : type_specifier_list struct_declarator_list ';'
  ;

struct_declarator_list
  : struct_declarator
  | struct_declarator_list ',' struct_declarator
  ;

struct_declarator
  : declarator
  | ':' constant_expr
  | declarator ':' constant_expr
  ;

enum_specifier
  : ENUM '{' enumerator_list '}'
  | ENUM identifier '{' enumerator_list '}'
  | ENUM identifier
  ;

enumerator_list
  : enumerator
  | enumerator_list ',' enumerator
  ;

enumerator
  : identifier
  | identifier '=' constant_expr
  ;

declarator
  : declarator2
  | pointer declarator2
  ;

declarator2
  : identifier
  | '(' declarator ')'
  | declarator2 '[' ']'
  | declarator2 '[' constant_expr ']'
  | declarator2 '(' ')'
  | declarator2 '(' parameter_type_list ')'
  | declarator2 '(' parameter_identifier_list ')'
  ;

pointer
  : '*'
  | '*' type_specifier_list
  | '*' pointer
  | '*' type_specifier_list pointer
  ;

type_specifier_list
  : type_specifier
  | type_specifier_list type_specifier
  ;

parameter_identifier_list
  : identifier_list
  | identifier_list ',' ELIPSIS
  ;

identifier_list
  : identifier
  | identifier_list ',' identifier
  ;

parameter_type_list
  : parameter_list
  | parameter_list ',' ELIPSIS
  ;

parameter_list
  : parameter_declaration
  | parameter_list ',' parameter_declaration
  ;

parameter_declaration
  : type_specifier_list declarator
  | type_name
  ;

type_name
  : type_specifier_list
  | type_specifier_list abstract_declarator
  ;

abstract_declarator
  : pointer
  | abstract_declarator2
  | pointer abstract_declarator2
  ;

abstract_declarator2
  : '(' abstract_declarator ')'
  | '[' ']'
  | '[' constant_expr ']'
  | abstract_declarator2 '[' ']'
  | abstract_declarator2 '[' constant_expr ']'
  | '(' ')'
  | '(' parameter_type_list ')'
  | abstract_declarator2 '(' ')'
  | abstract_declarator2 '(' parameter_type_list ')'
  ;

initializer
  : assignment_expr
  | '{' initializer_list '}'
  | '{' initializer_list ',' '}'
  ;

initializer_list
  : initializer
  | initializer_list ',' initializer
  ;

statement
  : labeled_statement
  | compound_statement
  | expression_statement
  | selection_statement
  | iteration_statement
  | jump_statement
  ;

labeled_statement
  : identifier ':' statement
  | CASE constant_expr ':' statement
  | DEFAULT ':' statement
  ;

compound_statement
  : '{' '}'
  | '{' statement_list '}'
  | '{' declaration_list '}'
  | '{' declaration_list statement_list '}'
  ;

declaration_list
  : declaration
  | declaration_list declaration
  ;

statement_list
  : statement
  | statement_list statement
  ;

expression_statement
  : ';'
  | expr ';'
  ;

selection_statement
  : IF '(' expr ')' statement
  | IF '(' expr ')' statement ELSE statement
  | SWITCH '(' expr ')' statement
  ;

iteration_statement
  : WHILE '(' expr ')' statement
  | DO statement WHILE '(' expr ')' ';'
  | FOR '(' ';' ';' ')' statement
  | FOR '(' ';' ';' expr ')' statement
  | FOR '(' ';' expr ';' ')' statement
  | FOR '(' ';' expr ';' expr ')' statement
  | FOR '(' expr ';' ';' ')' statement
  | FOR '(' expr ';' ';' expr ')' statement
  | FOR '(' expr ';' expr ';' ')' statement
  | FOR '(' expr ';' expr ';' expr ')' statement
  ;

jump_statement
  : GOTO identifier ';'
  | CONTINUE ';'
  | BREAK ';'
  | RETURN ';'
  | RETURN expr ';'
  ;

file
  : external_definition
  | file external_definition
  ;

external_definition
  : function_definition
  | declaration
  ;

function_definition
  : declarator function_body
  | declaration_specifiers declarator function_body
  ;

function_body
  : compound_statement
  | declaration_list compound_statement
  ;

identifier
  : IDENTIFIER
  ;



*****************************************************************************************************************/

#include <string>
#include <fstream>
#include <map>

#include "grammar.h"
#include "lexer.h"

namespace c_grammar{

enum{
  EN_EOF = -1,
  UNKNOWN = 0,
  
  IDENTIFIER,      // [_A-Za-z][_A-Za-z0-9]*
  
  OCTAL,        // 0{D}+{IS}?
  HEX,        // 0[xX]{H}+{IS}?
  INTEGER,      // {D}+{IS}?
  REAL,        // ({D}+{E}{FS}?) | ({D}*\.{D}+({E})?{FS}?) | ({D}+"."{D}*({E})?{FS}?)
  
  CHARACTER_LITERAL,  // '(\\.|[^\\']){0-2}'
  STRING_LITERAL,    // "(.|[^"])*"

  STAR,        // *
  PLUS,        // +
  MINUS,        // -
  SLASH,        // /
  MOD,        // %
  
  EQUAL,        // =
  LESS,        // <
  MORE,        // >
  DOT,        // .
  AND,        // &
  OR,          // |
  XOR,        // ^
  EXCLAMATION,    // !
  TILDA,        // ~
  QUESTION,      // ?
  
  LEFT_BRACE,      // {
  RIGHT_BRACE,    // }
  LEFT_SQ_BRACKET,  // [
  RIGHT_SQ_BRACKET,  // ]
  LEFT_CL_BRACKET,  // (
  RIGHT_CL_BRACKET,  // )
  
  COLON,        // :
  SEMICOLON,      // ;
  COMMA,        // ,
  
  PTR_OP,        // ->
  INC_OP,        // ++
  DEC_OP,        // --
  LEFT_OP,      // <<
  RIGHT_OP,      // >>
  LE_OP,        // <=
  GE_OP,        // >=
  EQ_OP,        // ==
  NE_OP,        // !=
  AND_OP,        // &&
  OR_OP,        // ||

  MUL_ASSIGN,      // *=
  DIV_ASSIGN,      // /=
  MOD_ASSIGN,      // %=
  ADD_ASSIGN,      // +=
  SUB_ASSIGN,      // -=
  LEFT_ASSIGN,    // <<=
  RIGHT_ASSIGN,    // >>=
  AND_ASSIGN,      // &=
  XOR_ASSIGN,      // ^=
  OR_ASSIGN,      // |=
  
  ELIPSIS,      // ...
  RANGE,        //   

  SIZEOF,        // "sizeof"
  TYPEDEF,      // "typedef"
  EXTERN,        // "extern"
  STATIC,        // "static"
  AUTO,        // "auto"
  REGISTER,      // "register"

  CHAR,        // "char"
  SHORT,        // "short"
  INT,        // "int"
  LONG,        // "long"
  SIGNED,        // "signed"
  UNSIGNED,      // "unsigned"
  FLOAT,        // "float"
  DOUBLE,        // "double"
  CONST,        // "const"
  VOLATILE,      // "volatile"
  VOID,        // "void"

  STRUCT,        // "struct"
  UNION,        // "union"
  ENUM,        //"enum"

  CASE,        // "case"
  DEFAULT,      // "default"
  IF,          // "if"
  ELSE,        // "else"
  SWITCH,        // "switch"
  WHILE,        // "while"
  DO,          // "do"
  FOR,        // "for"
  GOTO,        // "goto"
  CONTINUE,      // "continue"
  BREAK,        // "break"
  RETURN,        // "return"
  
  primary_expr,
  postfix_expr,
  argument_expr_list,
  unary_expr,
  unary_operator,
  cast_expr,
  multiplicative_expr,
  additive_expr,
  shift_expr,
  relational_expr,
  equality_expr,
  and_expr,
  exclusive_or_expr,
  inclusive_or_expr,
  logical_and_expr,
  logical_or_expr,
  conditional_expr,
  assignment_expr,
  assignment_operator,
  expr,
  constant_expr,
  
  declaration,
  declaration_specifiers,
  init_declarator_list,
  init_declarator,
  storage_class_specifier,
  type_specifier,
  struct_or_union_specifier,
  struct_or_union,
  struct_declaration_list,
  struct_declaration,
  struct_declarator_list,
  struct_declarator,
  enum_specifier,
  enumerator_list,
  enumerator,
  declarator,
  declarator2,
  pointer,
  type_specifier_list,
  parameter_identifier_list,
  identifier_list,
  parameter_type_list,
  parameter_list,
  parameter_declaration,
  type_name,
  abstract_declarator,
  abstract_declarator2,
  initializer,
  initializer_list,
  
  statement,
  labeled_statement,
  compound_statement,
  declaration_list,
  statement_list,
  expression_statement,
  selection_statement,
  iteration_statement,
  jump_statement,
  
  file,
  external_definition,
  function_definition,
  function_body,
  
  identifier,
  constant,
  
  SYMBOL_TABLE_LAST
};



typedef std::map< std::string, int >  keywords_t;

class CLexer : public parser::Lexer {
  int              cur_char_;      // the current character
  std::ifstream        in_;        // input file
  unsigned int        pos_;        // current position
  unsigned int        start_token_pos_;  // start position of token reading
  unsigned int        line_num_;      // current line number
  unsigned int        prev_line_len_;    // the length of previous line
  std::string          file_name_;      // the input file name
  
  keywords_t          keywords_;      // the keyword vs type table
  
  parser::token        cur_token_;      // the last returned token
  
public:
  
  lexer( const char* _file_name )
  :
  file_name_(_file_name),
  in_( _file_name, std::ios::binary|std::ios::in ),
  cur_char_(0),
  pos_(0),
  start_token_pos_(0),
  line_num_(1),
  prev_line_len_(0)
  {init_tokens(); init_keywords();}

  // return the current token
  parser::token        get_token();
  
  // is th end of input?
  bool            is_end();

    unsigned int                get_line_num() const {return line_num_;}
    unsigned int                get_pos() const {return pos_;}

  
private:
  // skip whitespaces
  void            skip_ws();
  
  // skip comments
  void            skip_comments();

  // read next character from the input
  char            next_char();

  // peek next character from the input
  char            peek_next();

  // put back read character
  void            put_back();
  
  // throw the error
  void            error( const std::string& );
  
  // init keyword table
  void            init_keywords();
  
  // init tokens
  void            init_tokens();
  
  // return keyword token on passed string
  int              get_keyword( const std::string& _str );
  
  // is the character octal digit
  bool            isodigit( int _char );
  
  // create token of passed type and value
  parser::token        create_token( int _type, const std::string& _str = "" );
};

// initialize public grammar by c language grammar
void              init_grammar( parser::PublicGrammar* );

enum rule{

  // expressions
  
  primary_expr__identifier,
  primary_expr__constant,
  primary_expr__STRING_LITERAL,
  primary_expr__LEFT_BRACE_expr_RIGHT_BRACE,
  
  postfix_expr__primary_expr,
  postfix_expr__postfix_expr_LEFT_SQ_BRACKET_expr_RIGHT_SQ_BRACKET,
  postfix_expr__postfix_expr_LEFT_BRACE_RIGHT_BRACE,
  postfix_expr__postfix_expr_LEFT_BRACE_argument_expr_list_RIGHT_BRACE,
  postfix_expr__postfix_expr_DOT_identifier,
  postfix_expr__postfix_expr_PTR_OP_identifier,
  postfix_expr__postfix_expr_INC_OP,
  postfix_expr__postfix_expr_DEC_OP,

  argument_expr_list__assignment_expr,
  argument_expr_list__argument_expr_list_COMMA_assignment_expr,

  unary_expr__postfix_expr,
  unary_expr__INC_OP_unary_expr,
  unary_expr__DEC_OP_unary_expr,
  unary_expr__unary_operator_cast_expr,
  unary_expr__SIZEOF_unary_expr,
  unary_expr__SIZEOF_LEFT_BRACE_type_name_RIGHT_BRACE,

  unary_operator__AND,
  unary_operator__STAR,
  unary_operator__PLUS,
  unary_operator__MINUS,
  unary_operator__TILDA,
  unary_operator__EXCLAMATION,

  cast_expr__unary_expr,
  cast_expr__LEFT_BRACE_type_name_RIGHT_BRACE_cast_expr,

  multiplicative_expr__cast_expr,
  multiplicative_expr__multiplicative_expr_STAR_cast_expr,
  multiplicative_expr__multiplicative_expr_SLASH_cast_expr,
  multiplicative_expr__multiplicative_expr_MOD_cast_expr,

  additive_expr__multiplicative_expr,
  additive_expr__additive_expr_PLUS_multiplicative_expr,
  additive_expr__additive_expr_MINUS_multiplicative_expr,

  shift_expr__additive_expr,
  shift_expr__shift_expr_LEFT_OP_additive_expr,
  shift_expr__shift_expr_RIGHT_OP_additive_expr,

  relational_expr__shift_expr,
  relational_expr__relational_expr_LESS_shift_expr,
  relational_expr__relational_expr_MORE_shift_expr,
  relational_expr__relational_expr_LE_OP_shift_expr,
  relational_expr__relational_expr_GE_OP_shift_expr,
  
  equality_expr__relational_expr,
  equality_expr__equality_expr_EQ_OP_relational_expr,
  equality_expr__equality_expr_NE_OP_relational_expr,

  and_expr__equality_expr,
  and_expr__and_expr_AND_equality_expr,
  
  exclusive_or_expr__and_expr,
  exclusive_or_expr__exclusive_or_expr_XOR_and_expr,
  
  inclusive_or_expr__exclusive_or_expr,
  inclusive_or_expr__inclusive_or_expr_OR_exclusive_or_expr,
  
  logical_and_expr__inclusive_or_expr,
  logical_and_expr__logical_and_expr_AND_OP_inclusive_or_expr,
  
  logical_or_expr__logical_and_expr,
  logical_or_expr__logical_or_expr_OR_OP_logical_and_expr,
  
  conditional_expr__logical_or_expr,
  conditional_expr__logical_or_expr_QUESTION_logical_or_expr_COLON_conditional_expr,
  
  assignment_expr__conditional_expr,
  assignment_expr__unary_expr_assignment_operator_assignment_expr,
  
  assignment_operator__EQUAL,
  assignment_operator__MUL_ASSIGN,
  assignment_operator__DIV_ASSIGN,
  assignment_operator__MOD_ASSIGN,
  assignment_operator__ADD_ASSIGN,
  assignment_operator__SUB_ASSIGN,
  assignment_operator__LEFT_ASSIGN,
  assignment_operator__RIGHT_ASSIGN,
  assignment_operator__AND_ASSIGN,
  assignment_operator__XOR_ASSIGN,
  assignment_operator__OR_ASSIGN,
  
  expr__assignment_expr,
  expr__expr_COMMA_assignment_expr,

  constant_expr__conditional_expr,

  // declarations

  declaration__declaration_specifiers_SEMICOLON,
  declaration__declaration_specifiers_init_declarator_list_SEMICOLON,

  declaration_specifiers__storage_class_specifier,
  declaration_specifiers__storage_class_specifier_declaration_specifiers,
  declaration_specifiers__type_specifier,
  declaration_specifiers__type_specifier_declaration_specifiers,
  
  init_declarator_list__init_declarator,
  init_declarator_list__init_declarator_list_COMMA_init_declarator,

  init_declarator__declarator,
  init_declarator__declarator_EQUAL_initializer,

  storage_class_specifier__TYPEDEF,
  storage_class_specifier__EXTERN,
  storage_class_specifier__STATIC,
  storage_class_specifier__AUTO,
  storage_class_specifier__REGISTER,

  type_specifier__CHAR,
  type_specifier__SHORT,
  type_specifier__INT,
  type_specifier__LONG,
  type_specifier__SIGNED,
  type_specifier__UNSIGNED,
  type_specifier__FLOAT,
  type_specifier__DOUBLE,
  type_specifier__CONST,
  type_specifier__VOLATILE,
  type_specifier__VOID,
  type_specifier__struct_or_union_specifier,
  type_specifier__enum_specifier,

  struct_or_union_specifier__struct_or_union_identifier_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET,
  struct_or_union_specifier__struct_or_union_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET,
  struct_or_union_specifier__struct_or_union_identifier,

  struct_or_union__STRUCT,
  struct_or_union__UNION,

  struct_declaration_list__struct_declaration,
  struct_declaration_list__struct_declaration_list_struct_declaration,

  struct_declaration__type_specifier_list_struct_declarator_list_SEMICOLON,
  
  struct_declarator_list__struct_declarator,
  struct_declarator_list__struct_declarator_list_COMMA_struct_declarator,

  struct_declarator__declarator,
  struct_declarator__COLON_constant_expr,
  struct_declarator__declarator_COLON_constant_expr,

  enum_specifier__ENUM_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET,
  enum_specifier__ENUM_identifier_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET,
  enum_specifier__ENUM_identifier,

  enumerator_list__enumerator,
  enumerator_list__enumerator_list_COMMA_enumerator,
  
  enumerator__identifier,
  enumerator__identifier_EQUAL_constant_expr,

  declarator__declarator2,
  declarator__pointer_declarator2,

  declarator2__identifier,
  declarator2__LEFT_BRACE_declarator_RIGHT_BRACE,
  declarator2__declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET,
  declarator2__declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET,
  declarator2__declarator2_LEFT_BRACE_RIGHT_BRACE,
  declarator2__declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE,
  declarator2__declarator2_LEFT_BRACE_parameter_identifier_list_RIGHT_BRACE,

  pointer__STAR,
  pointer__STAR_type_specifier_list,
  pointer__STAR_pointer,
  pointer__STAR_type_specifier_list_pointer,

  type_specifier_list__type_specifier,
  type_specifier_list__type_specifier_list_type_specifier,

  parameter_identifier_list__identifier_list,
  parameter_identifier_list__identifier_list_COMMA_ELIPSIS,
  
  identifier_list__identifier,
  identifier_list__identifier_list_COMMA_identifier,

  parameter_type_list__parameter_list,
  parameter_type_list__parameter_list_COMMA_ELIPSIS,

  parameter_list__parameter_declaration,
  parameter_list__parameter_list_COMMA_parameter_declaration,

  parameter_declaration__type_specifier_list_declarator,
  parameter_declaration__type_name,

  type_name__type_specifier_list,
  type_name__type_specifier_list_abstract_declarator,

  abstract_declarator__pointer,
  abstract_declarator__abstract_declarator2,
  abstract_declarator__pointer_abstract_declarator2,

  abstract_declarator2__LEFT_BRACE_abstract_declarator_RIGHT_BRACE,
  abstract_declarator2__LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET,
  abstract_declarator2__LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET,
  abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET,
  abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET,
  abstract_declarator2__LEFT_BRACE_RIGHT_BRACE,
  abstract_declarator2__LEFT_BRACE_parameter_type_list_RIGHT_BRACE,
  abstract_declarator2__abstract_declarator2_LEFT_BRACE_RIGHT_BRACE,
  abstract_declarator2__abstract_declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE,

  initializer__assignment_expr,
  initializer__LEFT_SQ_BRACKET_initializer_list_RIGHT_SQ_BRACKET,
  initializer__LEFT_SQ_BRACKET_initializer_list_COMMA_RIGHT_SQ_BRACKET,

  initializer_list__initializer,
  initializer_list__initializer_list_COMMA_initializer,


  // statement declarations
  
  statement__labeled_statement,
  statement__compound_statement,
  statement__expression_statement,
  statement__selection_statement,
  statement__iteration_statement,
  statement__jump_statement,

  labeled_statement__identifier_COLON_statement,
  labeled_statement__CASE_constant_expr_COLON_statement,
  labeled_statement__DEFAULT_COLON_statement,

  compound_statement__LEFT_CL_BRACKET_RIGHT_CL_BRACKET,
  compound_statement__LEFT_CL_BRACKET_statement_list_RIGHT_CL_BRACKET,
  compound_statement__LEFT_CL_BRACKET_declaration_list_RIGHT_CL_BRACKET,
  compound_statement__LEFT_CL_BRACKET_declaration_list_statement_list_RIGHT_CL_BRACKET,

  declaration_list__declaration,
  declaration_list__declaration_list_declaration,

  statement_list__statement,
  statement_list__statement_list_statement,

  expression_statement__SEMICOLON,
  expression_statement__expr_SEMICOLON,

  selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement,
  selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement_ELSE_statement,
  selection_statement__SWITCH_LEFT_BRACE_expr_RIGHT_BRACE_statement,

  iteration_statement__WHILE_LEFT_BRACE_expr_RIGHT_BRACE_statement,
  iteration_statement__DO_statement_WHILE_LEFT_BRACE_expr_RIGHT_BRACE,
  iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement,
  iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement,
  iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement,
  iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement,
  iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement,
  iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement,
  iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement,
  iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement,

  jump_statement__GOTO_identifier_SEMICOLON,
  jump_statement__CONTINUE_SEMICOLON,
  jump_statement__BREAK_SEMICOLON,
  jump_statement__RETURN_SEMICOLON,
  jump_statement__RETURN_expr_SEMICOLON,

  // file scope declarations

  file__external_definition,
  file__file_external_definition,

  external_definition__function_definition,
  external_definition__declaration,

  function_definition__declarator_function_body,
  function_definition__declaration_specifiers_declarator_function_body,

  function_body__compound_statement,
  function_body__declaration_list_compound_statement,

  identifier__IDENTIFIER,
  
  constant__HEX,
  constant__OCTAL,
  constant__INTEGER,
  constant__REAL,
  constant__CHARACTER_LITERAL,
  
  LAST_RULE
};

} // namespace c_grammar



