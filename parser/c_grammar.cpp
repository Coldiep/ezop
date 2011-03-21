

#include <sstream>
#include <cstdlib>
#include <stdexcept>

#include "c_grammar.h"
using namespace c_grammar;

static const char* token_names_buffer_[ SYMBOL_TABLE_LAST + 2 ];
static const char** token_names_ = token_names_buffer_ + 1;

// print token to the ostream
void parser::token::print( std::ostream& _str )
{
  _str << token_names_[ type_ ] << ": (" << line_pos_ << "," << col_pos_ << ")";
  if( type_ == IDENTIFIER || type_ == HEX || type_ == OCTAL || type_ == INTEGER ||
    type_ == REAL || type_ == CHARACTER_LITERAL || type_ == STRING_LITERAL )
  {
    _str << " \"" << str_value_ << "\"";
  }
  
  _str << std::endl;
}

void lexer::init_tokens()
{
  token_names_[ EN_EOF ] = "EOF";
  token_names_[ UNKNOWN ] = "UNKNOWN";
  token_names_[ IDENTIFIER ] = "IDENTIFIER";
  token_names_[ OCTAL ] = "OCTAL";
  token_names_[ HEX ] = "HEX";
  token_names_[ INTEGER ] = "INTEGER";
  token_names_[ REAL ] = "REAL";
  token_names_[ CHARACTER_LITERAL ] = "CHARACTER_LITERAL";
  token_names_[ STRING_LITERAL ] = "STRING_LITERAL";
  token_names_[ STAR ] = "STAR";
  token_names_[ PLUS ] = "PLUS";
  token_names_[ MINUS ] = "MINUS";
  token_names_[ SLASH ] = "SLASH";
  token_names_[ MOD ] = "MOD";
  token_names_[ EQUAL ] = "EQUAL";
  token_names_[ LESS ] = "LESS";
  token_names_[ MORE ] = "MORE";
  token_names_[ DOT ] = "DOT";
  token_names_[ AND ] = "AND";
  token_names_[ OR ] = "OR";
  token_names_[ XOR ] = "XOR";
  token_names_[ EXCLAMATION ] = "EXCLAMATION";
  token_names_[ TILDA ] = "TILDA";
  token_names_[ QUESTION ] = "QUESTION";
  token_names_[ LEFT_BRACE ] = "LEFT_BRACE";
  token_names_[ RIGHT_BRACE ] = "RIGHT_BRACE";
  token_names_[ LEFT_SQ_BRACKET ] = "LEFT_SQ_BRACKET";
  token_names_[ RIGHT_SQ_BRACKET ] = "RIGHT_SQ_BRACKET";
  token_names_[ LEFT_CL_BRACKET ] = "LEFT_CL_BRACKET";
  token_names_[ RIGHT_CL_BRACKET ] = "RIGHT_CL_BRACKET";
  token_names_[ COLON ] = "COLON";
  token_names_[ SEMICOLON ] = "SEMICOLON";
  token_names_[ COMMA ] = "COMMA";
  token_names_[ PTR_OP ] = "PTR_OP";
  token_names_[ INC_OP ] = "INC_OP";
  token_names_[ DEC_OP ] = "DEC_OP";
  token_names_[ LEFT_OP ] = "LEFT_OP";
  token_names_[ RIGHT_OP ] = "RIGHT_OP";
  token_names_[ LE_OP ] = "LE_OP";
  token_names_[ GE_OP ] = "GE_OP";
  token_names_[ NE_OP ] = "NE_OP";
  token_names_[ AND_OP ] = "AND_OP";
  token_names_[ OR_OP ] = "OR_OP";
  token_names_[ MUL_ASSIGN ] = "MUL_ASSIGN";
  token_names_[ DIV_ASSIGN ] = "DIV_ASSIGN";
  token_names_[ MOD_ASSIGN ] = "MOD_ASSIGN";
  token_names_[ ADD_ASSIGN ] = "ADD_ASSIGN";
  token_names_[ SUB_ASSIGN ] = "SUB_ASSIGN";
  token_names_[ LEFT_ASSIGN ] = "LEFT_ASSIGN";
  token_names_[ RIGHT_ASSIGN ] = "RIGHT_ASSIGN";
  token_names_[ AND_ASSIGN ] = "AND_ASSIGN";
  token_names_[ XOR_ASSIGN ] = "XOR_ASSIGN";
  token_names_[ OR_ASSIGN ] = "OR_ASSIGN";
  token_names_[ ELIPSIS ] = "ELIPSIS";
  token_names_[ RANGE ] = "RANGE";
  token_names_[ SIZEOF ] = "SIZEOF";
  token_names_[ TYPEDEF ] = "TYPEDEF";
  token_names_[ EXTERN ] = "EXTERN";
  token_names_[ STATIC ] = "STATIC";
  token_names_[ AUTO ] = "AUTO";
  token_names_[ REGISTER ] = "REGISTER";
  token_names_[ CHAR ] = "CHAR";
  token_names_[ SHORT ] = "SHORT";
  token_names_[ INT ] = "INT";
  token_names_[ LONG ] = "LONG";
  token_names_[ INT ] = "INT";
  token_names_[ LONG ] = "LONG";
  token_names_[ SIGNED ] = "SIGNED";
  token_names_[ UNSIGNED ] = "UNSIGNED";
  token_names_[ FLOAT ] = "FLOAT";
  token_names_[ DOUBLE ] = "DOUBLE";
  token_names_[ CONST ] = "CONST";
  token_names_[ VOLATILE ] = "VOLATILE";
  token_names_[ VOID ] = "VOID";
  token_names_[ STRUCT ] = "STRUCT";
  token_names_[ UNION ] = "UNION";
  token_names_[ ENUM ] = "ENUM";
  token_names_[ CASE ] = "CASE";
  token_names_[ DEFAULT ] = "DEFAULT";
  token_names_[ IF ] = "IF";
  token_names_[ ELSE ] = "ELSE";
  token_names_[ SWITCH ] = "SWITCH";
  token_names_[ WHILE ] = "WHILE";
  token_names_[ DO ] = "DO";
  token_names_[ FOR ] = "FOR";
  token_names_[ GOTO ] = "GOTO";
  token_names_[ CONTINUE ] = "CONTINUE";
  token_names_[ BREAK ] = "BREAK";
  token_names_[ RETURN ] = "RETURN";
}

// read next character from the input
char lexer::next_char()
{
  cur_char_ = in_.get();
  
  // if current symbol is printable increase position
  if(
    EOF != cur_char_
    && '\t' != cur_char_
    && '\r' != cur_char_
    && '\n' != cur_char_
    && '\v' != cur_char_
     )
  {
    ++ pos_;
  }
     
  // if the symbol is the end of the line set cureent position to null
  // and remember the previous line length
  if( '\n' == cur_char_ )
  {
    ++ line_num_;
    prev_line_len_ = pos_;
    pos_ = 0;
  }
    
  return cur_char_;
}

// peek next character from the input
char lexer::peek_next()
{
  return in_.peek();
}

// put back read character
void lexer::put_back()
{
  // we descease the position only if the symbol read is printable
  if(
    EOF != cur_char_
    && '\t' != cur_char_
    && '\r' != cur_char_
    && '\n' != cur_char_
    && '\v' != cur_char_
     ) -- pos_;
  
  // if it was the line symbol decrease the line number
  if( '\n' == cur_char_ )
  {
    -- line_num_;
    pos_ = prev_line_len_;
  }
  
  in_.putback( cur_char_ );
}

// throw the error
void lexer::error( const std::string& error )
{
  std::stringstream st;
  st << "LEXER ERROR:\n";
  st << "Position = " << pos_;
  st << ", Line = " << line_num_;
  st << "\nDescription: " << error;
  throw std::runtime_error( st.str().c_str() );
}

void lexer::skip_comments()
{
  skip_ws();
  
  // skip comments  
  if( cur_char_ == '/' )
  {
    if( peek_next() == '*' )
    {
      next_char();
      bool is_end_of_comment = false;
      do
      {
        for( next_char(); cur_char_ != '*'; next_char() )
        {
          if ( cur_char_ == EOF ) error ( "the comment is not finished in the file" );
        }
        
        if ( peek_next() == '/' )
        {
          next_char();
          is_end_of_comment = true;
        }
      } while ( ! is_end_of_comment );
    }
    
    skip_ws();
  }
  
  // skip any preprocessor directive
  if ( cur_char_ == '#' )
  {
    for( next_char(); cur_char_ != '\n'; next_char() )
    {
      if ( cur_char_ == '\\' )
        if( peek_next() == '\n' ) next_char();
      if ( cur_char_ == EOF ) error ( "the preprocessor directive is not finished in the file" );
    }

    next_char();
  }
}

void lexer::skip_ws()
{
  for( next_char();
            cur_char_ == ' '
            || cur_char_ == '\t'
            || cur_char_ == '\v'
            || cur_char_ == '\n'
            || cur_char_ == '\r'
            || cur_char_ == '\\';
     next_char() )
     {
      if ( cur_char_ == '\\' )
        if( peek_next() == '\n' ) next_char();
        else break;
     }
}

bool lexer::isodigit( int _char )
{
  switch( _char )
  {
    case 0:case 1:case 2:case 3:case 4:case 5:case 6:case 7: return true;
  }
  
  return false;
}

parser::token lexer::create_token( int _type, const std::string& _str )
{
  cur_token_ = parser::token( _type, start_token_pos_, line_num_, _str );
  return cur_token_;
}

bool lexer::is_end()
{
  return cur_token_.type_ == EOF;
}

// return the current token
parser::token lexer::get_token()
{
  // skip nonprintable symbols
  skip_comments();
  
  if( cur_char_ == EOF ) return create_token( EOF );
  
  start_token_pos_ = pos_;
  
  // parse IDENTIFIER
  if( isalpha( cur_char_ ) || cur_char_ == '_' )
  {
    std::string _cur_id;
    _cur_id += cur_char_;
    for( next_char(); isalnum( cur_char_ ) || cur_char_ == '_'; next_char() )
    {
      _cur_id += cur_char_;
    }
    put_back();
    
    int res = get_keyword( _cur_id );
    if( UNKNOWN != res ) return create_token( res );
  
    return create_token( IDENTIFIER, _cur_id );
  }
  
  
  // parse number
  //  D      [0-9]
  //  L      [a-zA-Z_]
  //  H      [a-fA-F0-9]
  //  E      [Ee][+-]?{D}+
  //  FS      (f|F|l|L)
  //  IS      (u|U|l|L)*
  
  int state = 0;
  std::string _dec;

  if( isdigit( cur_char_ ) )
  {
    // we have hex or octal number
    if( '0' == cur_char_ )
    {
      // hex number 0[xX]{H}+{IS}?
      if( 'x' == peek_next() || 'X' == peek_next() )
      {
        next_char();
        
        std::string _hex = "0x";
        for( next_char(); isxdigit( cur_char_ ); next_char() )
        {
          _hex += cur_char_;
        }
        
        if( 'f' == cur_char_ || 'F' == cur_char_ || 'u' == cur_char_ || 'U' == cur_char_ )
          _hex += cur_char_;
        else put_back();
        
        return create_token( HEX, _hex );
      }
      
      // octal number 0{D}+{IS}?
      else if( isodigit( peek_next() ) )
      {
        std::string _octal = "0";
        for( next_char(); isodigit( cur_char_ ); next_char() )
        {
          _octal += cur_char_;
        }
        
        if( 'f' == cur_char_ || 'F' == cur_char_ || 'u' == cur_char_ || 'U' == cur_char_ )
          _octal += cur_char_;
        else put_back();
        
        return create_token( OCTAL, _octal );
      }
      
      else
      {
        // state 1: some digit after 0.
        if( '.' == peek_next() )
        {
          _dec = "0.";
        
          next_char();
          
          state = 1;
          goto read_digits;
        }
        
        // integer null
        else return create_token( INTEGER, "0" );
      }
    }
    
    // we have decimal number {D}+"."{D}*({E})?{FS}? or {D}+{E}{FS}? or {D}+{IS}?
    else
    {
    read_digits:
    
      for( ; isdigit( cur_char_ ); next_char() )
      {
        _dec += cur_char_;
      }

      // state 2: dot after digit occured
      if( '.' == cur_char_ )
      {
        if( 0 != state ) error( "incorrect real number format" );
        
        _dec += cur_char_;
        
        next_char();
        
        state = 2;
        goto read_digits;
      }
      
      // state 4: E occured
      else if( 'e' == cur_char_ || 'E' == cur_char_ )
      {
        if( 4 == state ) error( "incorrect real number format" );
        
        _dec += cur_char_;
        
        if( '+' == peek_next() || '-' == peek_next() )
        {
          next_char();
          _dec += cur_char_;
        }
        
        if( ! isdigit( peek_next() ) ) error( "incorrect real number format" );
        
        next_char();
        
        state = 4;
        goto read_digits;
      }
        
      if( 'f' == cur_char_ || 'F' == cur_char_ || 'u' == cur_char_ || 'U' == cur_char_ )
        _dec += cur_char_;
      else put_back();
      
      switch( state )
      {
      // integer number
      case 0: return create_token( INTEGER, _dec );
      
      // real number
      case 1:case 2:case 3: case 4: return create_token( REAL, _dec );
      }
    }
  }
  
  // state 3: first dot in the real number
  else if( '.' == cur_char_ )
  {
    // we have real decimal number \.{D}+({E})?{FS}?
    if( isdigit( peek_next() ) )
    {
      _dec = ".";
      
      next_char();
      
      state = 3;
      goto read_digits;
    }
  }
  
  // parse CHARACTER_LITERAL '(\\.|[^\\'])+'
  if( '\'' == cur_char_ )
  {
    // empty character cannot be
    if( '\'' == peek_next() ) error( "character literal must contain at least one character" );
    
    std::string _char;
    for( next_char(); '\'' != cur_char_; next_char() )
    {
      if( EOF == cur_char_ ) error( "EOF in constant" );
      
      if( '\\' == cur_char_ )
      {
        next_char();
        _char += cur_char_;
      }
      else _char += cur_char_;
    }
  
    return create_token( CHARACTER_LITERAL, _char );
  }

  // parse STRING_LITERAL "(.|[^"])*"
  if( '"' == cur_char_ )
  {
    std::string _str;
    for( next_char(); cur_char_ != '"'; next_char() );
    {
      if( EOF == cur_char_ ) error( "EOF in constant" );
      else if( '\n' == cur_char_ ) error( "new line in constant" );
      
      _str += cur_char_;
    }
  
    return create_token( STRING_LITERAL, _str );
  }
  
  // parse elipsis
  if( '.' == cur_char_ )
  {
    if( '.' == peek_next() )
    {
      next_char();
      if( '.' == peek_next() )
      {
        next_char();
        return create_token( ELIPSIS );
      }
      else put_back();
    }
  }

  // parse operators
  switch( cur_char_ )
  {
  case '*':
    if( '=' == peek_next() ){next_char(); return create_token( MUL_ASSIGN );}
    return create_token( STAR );
    
  case '+':
    if( '=' == peek_next() ){next_char(); return create_token( ADD_ASSIGN );}
    else if( '+' == peek_next() ){next_char(); return create_token( INC_OP );}
    return create_token( PLUS );

  case '-':
    if( '=' == peek_next() ){next_char(); return create_token( SUB_ASSIGN );}
    else if( '>' == peek_next() ){next_char(); return create_token( PTR_OP );}
    else if( '-' == peek_next() ){next_char(); return create_token( DEC_OP );}
    return create_token( MINUS );

  case '/':
    if( '=' == peek_next() ){next_char(); return create_token( DIV_ASSIGN );}
    return create_token( SLASH );

  case '%':
    if( '=' == peek_next() ){next_char(); return create_token( MOD_ASSIGN );}
    return create_token( MOD );

  case '=':
    if( '=' == peek_next() ){next_char(); return create_token( EQ_OP );}
    return create_token( EQUAL );

  case '<':
    if( '=' == peek_next() ){next_char(); return create_token( LE_OP );}
    else if( '<' == peek_next() )
    {
      next_char();
      if( '=' == peek_next() ){next_char(); return create_token( LEFT_ASSIGN );}
      return create_token( LEFT_OP );
    }
    return create_token( LESS );

  case '>':
    if( '=' == peek_next() ){next_char(); return create_token( GE_OP );}
    else if( '>' == peek_next() )
    {
      next_char();
      if( '=' == peek_next() ){next_char(); return create_token( RIGHT_ASSIGN );}
      return create_token( RIGHT_OP );
    }
    return create_token( MORE );

  case '&':
    if( '=' == peek_next() ){next_char(); return create_token( AND_ASSIGN );}
    else if( '&' == peek_next() ){next_char(); return create_token( AND_OP );}
    return create_token( AND );

  case '|':
    if( '=' == peek_next() ){next_char(); return create_token( OR_ASSIGN );}
    else if( '|' == peek_next() ){next_char(); return create_token( OR_OP );}
    return create_token( OR );

  case '^':
    if( '=' == peek_next() ){next_char(); return create_token( XOR_ASSIGN );}
    return create_token( XOR );

  case '!':
    if( '=' == peek_next() ){next_char(); return create_token( NE_OP );}
    return create_token( EXCLAMATION );

  case '.': return create_token( DOT );
  case '~': return create_token( TILDA );
  case '?': return create_token( QUESTION );
  case '{': return create_token( LEFT_CL_BRACKET );
  case '}': return create_token( RIGHT_CL_BRACKET );
  case '[': return create_token( LEFT_SQ_BRACKET );
  case ']': return create_token( RIGHT_SQ_BRACKET );
  case '(': return create_token( LEFT_BRACE );
  case ')': return create_token( RIGHT_BRACE );
  case ';': return create_token( SEMICOLON );
  case ':': return create_token( COLON );
  case ',': return create_token( COMMA );
  }

  if( EOF == cur_char_ ) return create_token( EOF );
  
  return create_token( UNKNOWN );
}

int lexer::get_keyword( const std::string& _str )
{
  keywords_t::iterator it = keywords_.find( _str );
  if( it != keywords_.end() ) return (*it).second;
   
  return UNKNOWN;
}


void lexer::init_keywords()
{
  keywords_[ "typedef" ]    = TYPEDEF;
  keywords_[ "extern" ]    = EXTERN;
  keywords_[ "static" ]    = STATIC;
  keywords_[ "auto" ]      = AUTO;
  keywords_[ "register" ]    = REGISTER;
  
  keywords_[ "char" ]      = CHAR;
  keywords_[ "short" ]    = SHORT;
  keywords_[ "int" ]      = INT;
  keywords_[ "long" ]      = LONG;
  keywords_[ "signed" ]    = SIGNED;
  keywords_[ "unsigned" ]    = UNSIGNED;
  keywords_[ "float" ]    = FLOAT;
  keywords_[ "double" ]    = DOUBLE;
  keywords_[ "const" ]    = CONST;
  keywords_[ "volatile" ]    = VOLATILE;
  keywords_[ "void" ]      = VOID;

  keywords_[ "struct" ]    = STRUCT;
  keywords_[ "union" ]    = UNION;
  keywords_[ "enum" ]      = ENUM;

  keywords_[ "case" ]      = CASE;
  keywords_[ "default" ]    = DEFAULT;
  keywords_[ "if" ]      = IF;
  keywords_[ "else" ]      = ELSE;
  keywords_[ "switch" ]    = SWITCH;
  keywords_[ "do" ]      = DO;
  keywords_[ "for" ]      = FOR;
  keywords_[ "goto" ]      = GOTO;
  keywords_[ "continue" ]    = CONTINUE;
  keywords_[ "break" ]    = BREAK;
  keywords_[ "return" ]    = RETURN;
}

// initialize public grammar by c language grammar
void c_grammar::init_grammar( parser::PublicGrammar* _gr ) {
  // add terminals
  _gr->AddTerminal( EN_EOF, "EOF" );
  _gr->AddTerminal( UNKNOWN, "unknown" );
  
  _gr->AddTerminal( IDENTIFIER, "IDENTIFIER" );
  
  _gr->AddTerminal( OCTAL, "OCTAL" );
  _gr->AddTerminal( HEX, "HEX" );
  _gr->AddTerminal( INTEGER, "INTEGER" );
  _gr->AddTerminal( REAL, "REAL" );
  
  _gr->AddTerminal( CHARACTER_LITERAL, "CHARACTER_LITERAL" );
  _gr->AddTerminal( STRING_LITERAL, "STRING_LITERAL" );
  
  _gr->AddTerminal( STAR, "*" );
  _gr->AddTerminal( PLUS, "+" );
  _gr->AddTerminal( MINUS, "-" );
  _gr->AddTerminal( SLASH, "/" );
  _gr->AddTerminal( MOD, "%" );
  
  _gr->AddTerminal( EQUAL, "=" );
  _gr->AddTerminal( LESS, "<" );
  _gr->AddTerminal( MORE, ">" );
  _gr->AddTerminal( DOT, "->" );
  _gr->AddTerminal( AND, "&" );
  _gr->AddTerminal( OR, "|" );
  _gr->AddTerminal( XOR, "^" );
  _gr->AddTerminal( EXCLAMATION, "!" );
  _gr->AddTerminal( TILDA, "~" );
  _gr->AddTerminal( QUESTION, "?" );
  
  _gr->AddTerminal( LEFT_BRACE, "(" );
  _gr->AddTerminal( RIGHT_BRACE, ")" );
  _gr->AddTerminal( LEFT_SQ_BRACKET, "[" );
  _gr->AddTerminal( RIGHT_SQ_BRACKET, "]" );
  _gr->AddTerminal( LEFT_CL_BRACKET, "{" );
  _gr->AddTerminal( RIGHT_CL_BRACKET, "}" );
  
  _gr->AddTerminal( COLON, ":" );
  _gr->AddTerminal( SEMICOLON, ";" );
  _gr->AddTerminal( COMMA, "," );
  
  _gr->AddTerminal( PTR_OP, "->" );
  _gr->AddTerminal( INC_OP, "++" );
  _gr->AddTerminal( DEC_OP, "--" );
  _gr->AddTerminal( LEFT_OP, "<<" );
  _gr->AddTerminal( RIGHT_OP, ">>" );
  _gr->AddTerminal( LE_OP, "<=" );
  _gr->AddTerminal( GE_OP, ">=" );
  _gr->AddTerminal( EQ_OP, "==" );
  _gr->AddTerminal( NE_OP, "!=" );
  _gr->AddTerminal( AND_OP, "&&" );
  _gr->AddTerminal( OR_OP, "||" );
  
  _gr->AddTerminal( MUL_ASSIGN, "*=" );
  _gr->AddTerminal( DIV_ASSIGN, "?=" );
  _gr->AddTerminal( MOD_ASSIGN, "%=" );
  _gr->AddTerminal( ADD_ASSIGN, "+=" );
  _gr->AddTerminal( SUB_ASSIGN, "-=" );
  _gr->AddTerminal( LEFT_ASSIGN, "<<=" );
  _gr->AddTerminal( RIGHT_ASSIGN, ">>=" );
  _gr->AddTerminal( AND_ASSIGN, "&=" );
  _gr->AddTerminal( XOR_ASSIGN, "^=" );
  _gr->AddTerminal( OR_ASSIGN, "|=" );
  
  _gr->AddTerminal( ELIPSIS, "..." );
  _gr->AddTerminal( RANGE, "range" );
  
  _gr->AddTerminal( SIZEOF, "sizeof" );
  _gr->AddTerminal( TYPEDEF, "typedef" );
  _gr->AddTerminal( EXTERN, "extern" );
  _gr->AddTerminal( STATIC, "static" );
  _gr->AddTerminal( AUTO, "auto" );
  _gr->AddTerminal( REGISTER, "register" );
  
  _gr->AddTerminal( CHAR, "char" );
  _gr->AddTerminal( SHORT, "short" );
  _gr->AddTerminal( INT, "int" );
  _gr->AddTerminal( LONG, "long" );
  _gr->AddTerminal( SIGNED, "signed" );
  _gr->AddTerminal( UNSIGNED, "unsigned" );
  _gr->AddTerminal( FLOAT, "float" );
  _gr->AddTerminal( DOUBLE, "double" );
  _gr->AddTerminal( CONST, "const" );
  _gr->AddTerminal( VOLATILE, "volatile" );
  _gr->AddTerminal( VOID, "void" );
  
  _gr->AddTerminal( STRUCT, "struct" );
  _gr->AddTerminal( UNION, "union" );
  _gr->AddTerminal( ENUM, "enum" );
  
  _gr->AddTerminal( CASE, "case" );
  _gr->AddTerminal( DEFAULT, "default" );
  _gr->AddTerminal( IF, "if" );
  _gr->AddTerminal( ELSE, "else" );
  _gr->AddTerminal( SWITCH, "switch" );
  _gr->AddTerminal( WHILE, "while" );
  _gr->AddTerminal( DO, "do" );
  _gr->AddTerminal( FOR, "for" );
  _gr->AddTerminal( GOTO, "goto" );
  _gr->AddTerminal( CONTINUE, "continue" );
  _gr->AddTerminal( BREAK, "break" );
  _gr->AddTerminal( RETURN, "break" );
  
  // add nonterminals
  _gr->AddNonterminal( primary_expr, "primary expr" );
  _gr->AddNonterminal( postfix_expr, "postfix expr" );
  _gr->AddNonterminal( argument_expr_list, "argument expr list" );
  _gr->AddNonterminal( unary_expr, "unary expr" );
  _gr->AddNonterminal( unary_operator, "unary operator" );
  _gr->AddNonterminal( cast_expr, "cast expr" );
  _gr->AddNonterminal( multiplicative_expr, "multiplicative expr" );
  _gr->AddNonterminal( additive_expr, "additive expr" );
  _gr->AddNonterminal( shift_expr, "shift expr" );
  _gr->AddNonterminal( relational_expr, "relational expr" );
  _gr->AddNonterminal( equality_expr, "equality expr" );
  _gr->AddNonterminal( and_expr, "and expr" );
  _gr->AddNonterminal( exclusive_or_expr, "exclusive or expr" );
  _gr->AddNonterminal( inclusive_or_expr, "inclusive or expr" );
  _gr->AddNonterminal( logical_and_expr, "logical and expr" );
  _gr->AddNonterminal( logical_or_expr, "logical or expr" );
  _gr->AddNonterminal( conditional_expr, "conditional expr" );
  _gr->AddNonterminal( assignment_expr, "assignment expr" );
  _gr->AddNonterminal( assignment_operator, "assignment operator" );
  _gr->AddNonterminal( expr, "expr" );
  _gr->AddNonterminal( constant_expr, "constant expr" );
  
  _gr->AddNonterminal( declaration, "declaration" );
  _gr->AddNonterminal( declaration_specifiers, "declaration specifiers" );
  _gr->AddNonterminal( init_declarator_list, "init declarator list" );
  _gr->AddNonterminal( init_declarator, "init declarator" );
  _gr->AddNonterminal( storage_class_specifier, "storage class specifier" );
  _gr->AddNonterminal( type_specifier, "type specifier" );
  _gr->AddNonterminal( struct_or_union_specifier, "struct or union specifier" );
  _gr->AddNonterminal( struct_or_union, "struct or union" );
  _gr->AddNonterminal( struct_declaration_list, "struct declaration list" );
  _gr->AddNonterminal( struct_declaration, "struct declaration" );
  _gr->AddNonterminal( struct_declarator_list, "struct declarator list" );
  _gr->AddNonterminal( struct_declarator, "struct declarator" );
  _gr->AddNonterminal( enum_specifier, "enum specifier" );
  _gr->AddNonterminal( enumerator_list, "enumerator list" );
  _gr->AddNonterminal( enumerator, "enumerator" );
  _gr->AddNonterminal( declarator, "declarator" );
  _gr->AddNonterminal( declarator2, "declarator2" );
  _gr->AddNonterminal( pointer, "pointer" );
  _gr->AddNonterminal( type_specifier_list, "type specifier list" );
  _gr->AddNonterminal( parameter_identifier_list, "parameter identifier list" );
  _gr->AddNonterminal( identifier_list, "identifier list" );
  _gr->AddNonterminal( parameter_type_list, "parameter type list" );
  _gr->AddNonterminal( parameter_list, "parameter list" );
  _gr->AddNonterminal( parameter_declaration, "parameter declaration" );
  _gr->AddNonterminal( type_name, "type_name" );
  _gr->AddNonterminal( abstract_declarator, "abstract declarator" );
  _gr->AddNonterminal( abstract_declarator2, "abstract declarator2" );
  _gr->AddNonterminal( initializer, "initializer" );
  _gr->AddNonterminal( initializer_list, "initializer list" );
  
  _gr->AddNonterminal( statement, "statement" );
  _gr->AddNonterminal( labeled_statement, "labeled statement" );
  _gr->AddNonterminal( compound_statement, "compound statement" );
  _gr->AddNonterminal( declaration_list, "declaration list" );
  _gr->AddNonterminal( statement_list, "statement list" );
  _gr->AddNonterminal( expression_statement, "expression statement" );
  _gr->AddNonterminal( selection_statement, "selection statement" );
  _gr->AddNonterminal( iteration_statement, "iteration statement" );
  _gr->AddNonterminal( jump_statement, "jump statement" );
  
  _gr->AddNonterminal( file, "file" );
  _gr->AddNonterminal( external_definition, "external definition" );
  _gr->AddNonterminal( function_definition, "function definition" );
  _gr->AddNonterminal( function_body, "function body" );
  
  _gr->AddNonterminal( identifier, "identifier" );
  
  _gr->AddNonterminal( constant, "constant" );
  
  // set start symbol
  _gr->SetStartSymbolId( file );
  
  // add rules
  
  // primary_expr 
  // : identifier
  // | constant
  // | STRING_LITERAL
  // | '(' expr ')'

  _gr->AddRule( primary_expr__identifier, "primary_expr --> identifier" );
  _gr->AddLhsSymbol( primary_expr__identifier, primary_expr );
  _gr->AddRhsSymbol( primary_expr__identifier, identifier );
  
  _gr->AddRule( primary_expr__constant, "primary_expr --> constant" );
  _gr->AddLhsSymbol( primary_expr__constant, primary_expr );
  _gr->AddRhsSymbol( primary_expr__constant, constant );

  _gr->AddRule( primary_expr__STRING_LITERAL, "primary_expr --> STRING_LITERAL" );
  _gr->AddLhsSymbol( primary_expr__STRING_LITERAL, primary_expr );
  _gr->AddRhsSymbol( primary_expr__STRING_LITERAL, STRING_LITERAL );

  _gr->AddRule( primary_expr__LEFT_BRACE_expr_RIGHT_BRACE, "primary_expr --> ( expr )" );
  _gr->AddLhsSymbol( primary_expr__LEFT_BRACE_expr_RIGHT_BRACE, primary_expr );
  _gr->AddRhsSymbol( primary_expr__LEFT_BRACE_expr_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( primary_expr__LEFT_BRACE_expr_RIGHT_BRACE, expr );
  _gr->AddRhsSymbol( primary_expr__LEFT_BRACE_expr_RIGHT_BRACE, RIGHT_BRACE );
  
  // postfix_expr
  // : primary_expr
  // | postfix_expr '[' expr ']'
  // | postfix_expr '(' ')'
  // | postfix_expr '(' argument_expr_list ')'
  // | postfix_expr '.' identifier
  // | postfix_expr PTR_OP identifier
  // | postfix_expr INC_OP
  // | postfix_expr DEC_OP
  
  _gr->AddRule( postfix_expr__primary_expr, "postfix_expr --> primary_expr" );
  _gr->AddLhsSymbol( postfix_expr__primary_expr, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__primary_expr, primary_expr );
  
  _gr->AddRule( postfix_expr__postfix_expr_LEFT_SQ_BRACKET_expr_RIGHT_SQ_BRACKET, "postfix_expr --> postfix_expr [ expr ]" );
  _gr->AddLhsSymbol( postfix_expr__postfix_expr_LEFT_SQ_BRACKET_expr_RIGHT_SQ_BRACKET, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_LEFT_SQ_BRACKET_expr_RIGHT_SQ_BRACKET, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_LEFT_SQ_BRACKET_expr_RIGHT_SQ_BRACKET, LEFT_SQ_BRACKET );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_LEFT_SQ_BRACKET_expr_RIGHT_SQ_BRACKET, RIGHT_SQ_BRACKET );
  
  _gr->AddRule( postfix_expr__postfix_expr_LEFT_BRACE_RIGHT_BRACE, "postfix_expr --> postfix_expr ( )" );
  _gr->AddLhsSymbol( postfix_expr__postfix_expr_LEFT_BRACE_RIGHT_BRACE, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_LEFT_BRACE_RIGHT_BRACE, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_LEFT_BRACE_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_LEFT_BRACE_RIGHT_BRACE, RIGHT_BRACE );
    
  _gr->AddRule( postfix_expr__postfix_expr_LEFT_BRACE_argument_expr_list_RIGHT_BRACE, "postfix_expr --> postfix_expr ( argument_expr_list )" );
  _gr->AddLhsSymbol( postfix_expr__postfix_expr_LEFT_BRACE_argument_expr_list_RIGHT_BRACE, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_LEFT_BRACE_argument_expr_list_RIGHT_BRACE, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_LEFT_BRACE_argument_expr_list_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_LEFT_BRACE_argument_expr_list_RIGHT_BRACE, argument_expr_list );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_LEFT_BRACE_argument_expr_list_RIGHT_BRACE, RIGHT_BRACE );
  
  _gr->AddRule( postfix_expr__postfix_expr_DOT_identifier, "postfix_expr --> postfix_expr . identifier" );
  _gr->AddLhsSymbol( postfix_expr__postfix_expr_DOT_identifier, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_DOT_identifier, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_DOT_identifier, DOT );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_DOT_identifier, identifier );
  
  _gr->AddRule( postfix_expr__postfix_expr_PTR_OP_identifier, "postfix_expr --> postfix_expr -> identifier" );
  _gr->AddLhsSymbol( postfix_expr__postfix_expr_PTR_OP_identifier, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_PTR_OP_identifier, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_PTR_OP_identifier, PTR_OP );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_PTR_OP_identifier, identifier );
  
  _gr->AddRule( postfix_expr__postfix_expr_INC_OP, "postfix_expr --> postfix_expr ++" );
  _gr->AddLhsSymbol( postfix_expr__postfix_expr_INC_OP, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_INC_OP, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_INC_OP, INC_OP );
  
  _gr->AddRule( postfix_expr__postfix_expr_DEC_OP, "postfix_expr --> postfix_expr --" );
  _gr->AddLhsSymbol( postfix_expr__postfix_expr_DEC_OP, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_DEC_OP, postfix_expr );
  _gr->AddRhsSymbol( postfix_expr__postfix_expr_DEC_OP, DEC_OP );
  
  // argument_expr_list
  // : assignment_expr
  // | argument_expr_list ',' assignment_expr
  
  _gr->AddRule( argument_expr_list__assignment_expr, "argument_expr_list --> assignment_expr" );
  _gr->AddLhsSymbol( argument_expr_list__assignment_expr, argument_expr_list );
  _gr->AddRhsSymbol( argument_expr_list__assignment_expr, assignment_expr );
  
  _gr->AddRule( argument_expr_list__argument_expr_list_COMMA_assignment_expr, "argument_expr_list --> argument_expr_list , assignment_expr" );
  _gr->AddLhsSymbol( argument_expr_list__argument_expr_list_COMMA_assignment_expr, argument_expr_list );
  _gr->AddRhsSymbol( argument_expr_list__argument_expr_list_COMMA_assignment_expr, argument_expr_list );
  _gr->AddRhsSymbol( argument_expr_list__argument_expr_list_COMMA_assignment_expr, COMMA );
  _gr->AddRhsSymbol( argument_expr_list__argument_expr_list_COMMA_assignment_expr, assignment_expr );
  
  // unary_expr
  // : postfix_expr
  // | INC_OP unary_expr
  // | DEC_OP unary_expr
  // | unary_operator cast_expr
  // | SIZEOF unary_expr
  // | SIZEOF '(' type_name ')'

  _gr->AddRule( unary_expr__postfix_expr, "unary_expr --> postfix_expr" );
  _gr->AddLhsSymbol( unary_expr__postfix_expr, unary_expr );
  _gr->AddRhsSymbol( unary_expr__postfix_expr, postfix_expr );
  
  _gr->AddRule( unary_expr__INC_OP_unary_expr, "unary_expr --> ++ unary_expr" );
  _gr->AddLhsSymbol( unary_expr__INC_OP_unary_expr, unary_expr );
  _gr->AddRhsSymbol( unary_expr__INC_OP_unary_expr, INC_OP );
  _gr->AddRhsSymbol( unary_expr__INC_OP_unary_expr, unary_expr );
  
  _gr->AddRule( unary_expr__DEC_OP_unary_expr, "unary_expr --> -- unary_expr" );
  _gr->AddLhsSymbol( unary_expr__DEC_OP_unary_expr, unary_expr );
  _gr->AddRhsSymbol( unary_expr__DEC_OP_unary_expr, DEC_OP );
  _gr->AddRhsSymbol( unary_expr__DEC_OP_unary_expr, unary_expr );
  
  _gr->AddRule( unary_expr__unary_operator_cast_expr, "unary_expr --> unary_operator cast_expr" );
  _gr->AddLhsSymbol( unary_expr__unary_operator_cast_expr, unary_expr );
  _gr->AddRhsSymbol( unary_expr__unary_operator_cast_expr, unary_operator );
  _gr->AddRhsSymbol( unary_expr__unary_operator_cast_expr, cast_expr );
  
  _gr->AddRule( unary_expr__SIZEOF_unary_expr, "unary_expr --> sizeof unary_expr" );
  _gr->AddLhsSymbol( unary_expr__SIZEOF_unary_expr, unary_expr );
  _gr->AddRhsSymbol( unary_expr__SIZEOF_unary_expr, SIZEOF );
  _gr->AddRhsSymbol( unary_expr__SIZEOF_unary_expr, unary_expr );
  
  _gr->AddRule( unary_expr__SIZEOF_LEFT_BRACE_type_name_RIGHT_BRACE, "unary_expr --> sizeof ( type_name )" );
  _gr->AddLhsSymbol( unary_expr__SIZEOF_LEFT_BRACE_type_name_RIGHT_BRACE, unary_expr );
  _gr->AddRhsSymbol( unary_expr__SIZEOF_LEFT_BRACE_type_name_RIGHT_BRACE, SIZEOF );
  _gr->AddRhsSymbol( unary_expr__SIZEOF_LEFT_BRACE_type_name_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( unary_expr__SIZEOF_LEFT_BRACE_type_name_RIGHT_BRACE, type_name );
  _gr->AddRhsSymbol( unary_expr__SIZEOF_LEFT_BRACE_type_name_RIGHT_BRACE, RIGHT_BRACE );
  
  // unary_operator
  // : '&'
  // | '*'
  // | '+'
  // | '-'
  // | '~'
  // | '!'

  _gr->AddRule( unary_operator__AND, "unary_operator --> &" );
  _gr->AddLhsSymbol( unary_operator__AND, unary_operator );
  _gr->AddRhsSymbol( unary_operator__AND, AND );
  
  _gr->AddRule( unary_operator__STAR, "unary_operator --> *" );
  _gr->AddLhsSymbol( unary_operator__STAR, unary_operator );
  _gr->AddRhsSymbol( unary_operator__STAR, STAR );
  
  _gr->AddRule( unary_operator__PLUS, "unary_operator --> +" );
  _gr->AddLhsSymbol( unary_operator__PLUS, unary_operator );
  _gr->AddRhsSymbol( unary_operator__PLUS, PLUS );
  
  _gr->AddRule( unary_operator__MINUS, "unary_operator --> -" );
  _gr->AddLhsSymbol( unary_operator__MINUS, unary_operator );
  _gr->AddRhsSymbol( unary_operator__MINUS, MINUS );
  
  _gr->AddRule( unary_operator__TILDA, "unary_operator ~" );
  _gr->AddLhsSymbol( unary_operator__TILDA, unary_operator );
  _gr->AddRhsSymbol( unary_operator__TILDA, TILDA );
  
  _gr->AddRule( unary_operator__EXCLAMATION, "unary_operator --> !" );
  _gr->AddLhsSymbol( unary_operator__EXCLAMATION, unary_operator );
  _gr->AddRhsSymbol( unary_operator__EXCLAMATION, EXCLAMATION );
  
  // cast_expr
  // : unary_expr
  // | '(' type_name ')' cast_expr
  
  _gr->AddRule( cast_expr__unary_expr, "cast_expr --> unary_expr" );
  _gr->AddLhsSymbol( cast_expr__unary_expr, cast_expr );
  _gr->AddRhsSymbol( cast_expr__unary_expr, unary_expr );

  _gr->AddRule( cast_expr__LEFT_BRACE_type_name_RIGHT_BRACE_cast_expr, "cast_expr --> ( type_name ) cast_expr" );
  _gr->AddLhsSymbol( cast_expr__LEFT_BRACE_type_name_RIGHT_BRACE_cast_expr, cast_expr );
  _gr->AddRhsSymbol( cast_expr__LEFT_BRACE_type_name_RIGHT_BRACE_cast_expr, LEFT_BRACE );
  _gr->AddRhsSymbol( cast_expr__LEFT_BRACE_type_name_RIGHT_BRACE_cast_expr, type_name );
  _gr->AddRhsSymbol( cast_expr__LEFT_BRACE_type_name_RIGHT_BRACE_cast_expr, RIGHT_BRACE );
  _gr->AddRhsSymbol( cast_expr__LEFT_BRACE_type_name_RIGHT_BRACE_cast_expr, cast_expr );

  // multiplicative_expr
  // : cast_expr
  // | multiplicative_expr '*' cast_expr
  // | multiplicative_expr '/' cast_expr
  // | multiplicative_expr '%' cast_expr

  _gr->AddRule( multiplicative_expr__cast_expr, "multiplicative_expr --> cast_expr" );
  _gr->AddLhsSymbol( multiplicative_expr__cast_expr, multiplicative_expr );
  _gr->AddRhsSymbol( multiplicative_expr__cast_expr, cast_expr );
  
  _gr->AddRule( multiplicative_expr__multiplicative_expr_STAR_cast_expr, "multiplicative_expr --> multiplicative_expr * cast_expr" );
  _gr->AddLhsSymbol( multiplicative_expr__multiplicative_expr_STAR_cast_expr, multiplicative_expr );
  _gr->AddRhsSymbol( multiplicative_expr__multiplicative_expr_STAR_cast_expr, multiplicative_expr );
  _gr->AddRhsSymbol( multiplicative_expr__multiplicative_expr_STAR_cast_expr, STAR );
  _gr->AddRhsSymbol( multiplicative_expr__multiplicative_expr_STAR_cast_expr, cast_expr );
  
  _gr->AddRule( multiplicative_expr__multiplicative_expr_SLASH_cast_expr, "multiplicative_expr --> multiplicative_expr / cast_expr" );
  _gr->AddLhsSymbol( multiplicative_expr__multiplicative_expr_SLASH_cast_expr, multiplicative_expr );
  _gr->AddRhsSymbol( multiplicative_expr__multiplicative_expr_SLASH_cast_expr, multiplicative_expr );
  _gr->AddRhsSymbol( multiplicative_expr__multiplicative_expr_SLASH_cast_expr, SLASH );
  _gr->AddRhsSymbol( multiplicative_expr__multiplicative_expr_SLASH_cast_expr, cast_expr );
  
  _gr->AddRule( multiplicative_expr__multiplicative_expr_MOD_cast_expr, "multiplicative_expr --> multiplicative_expr % cast_expr" );
  _gr->AddLhsSymbol( multiplicative_expr__multiplicative_expr_MOD_cast_expr, multiplicative_expr );
  _gr->AddRhsSymbol( multiplicative_expr__multiplicative_expr_MOD_cast_expr, multiplicative_expr );
  _gr->AddRhsSymbol( multiplicative_expr__multiplicative_expr_MOD_cast_expr, MOD );
  _gr->AddRhsSymbol( multiplicative_expr__multiplicative_expr_MOD_cast_expr, cast_expr );
  
  // additive_expr
  // : multiplicative_expr
  // | additive_expr '+' multiplicative_expr
  // | additive_expr '-' multiplicative_expr

  _gr->AddRule( additive_expr__multiplicative_expr, "additive_expr --> multiplicative_expr" );
  _gr->AddLhsSymbol( additive_expr__multiplicative_expr, additive_expr );
  _gr->AddRhsSymbol( additive_expr__multiplicative_expr, multiplicative_expr );

  _gr->AddRule( additive_expr__additive_expr_PLUS_multiplicative_expr, "additive_expr --> additive_expr + multiplicative_expr" );
  _gr->AddLhsSymbol( additive_expr__additive_expr_PLUS_multiplicative_expr, additive_expr );
  _gr->AddRhsSymbol( additive_expr__additive_expr_PLUS_multiplicative_expr, additive_expr );
  _gr->AddRhsSymbol( additive_expr__additive_expr_PLUS_multiplicative_expr, PLUS );
  _gr->AddRhsSymbol( additive_expr__additive_expr_PLUS_multiplicative_expr, multiplicative_expr );

  _gr->AddRule( additive_expr__additive_expr_MINUS_multiplicative_expr, "additive_expr --> additive_expr - multiplicative_expr" );
  _gr->AddLhsSymbol( additive_expr__additive_expr_MINUS_multiplicative_expr, additive_expr );
  _gr->AddRhsSymbol( additive_expr__additive_expr_MINUS_multiplicative_expr, additive_expr );
  _gr->AddRhsSymbol( additive_expr__additive_expr_MINUS_multiplicative_expr, MINUS );
  _gr->AddRhsSymbol( additive_expr__additive_expr_MINUS_multiplicative_expr, multiplicative_expr );
  
  // shift_expr
  // : additive_expr
  // | shift_expr LEFT_OP additive_expr
  // | shift_expr RIGHT_OP additive_expr

  _gr->AddRule( shift_expr__additive_expr, "shift_expr --> additive_expr" );
  _gr->AddLhsSymbol( shift_expr__additive_expr, shift_expr );
  _gr->AddRhsSymbol( shift_expr__additive_expr, additive_expr );

  _gr->AddRule( shift_expr__shift_expr_LEFT_OP_additive_expr, "shift_expr --> shift_expr << additive_expr" );
  _gr->AddLhsSymbol( shift_expr__shift_expr_LEFT_OP_additive_expr, shift_expr );
  _gr->AddRhsSymbol( shift_expr__shift_expr_LEFT_OP_additive_expr, shift_expr );
  _gr->AddRhsSymbol( shift_expr__shift_expr_LEFT_OP_additive_expr, LEFT_OP );
  _gr->AddRhsSymbol( shift_expr__shift_expr_LEFT_OP_additive_expr, additive_expr );

  _gr->AddRule( shift_expr__shift_expr_RIGHT_OP_additive_expr, "shift_expr --> shift_expr >> additive_expr" );
  _gr->AddLhsSymbol( shift_expr__shift_expr_RIGHT_OP_additive_expr, shift_expr );
  _gr->AddRhsSymbol( shift_expr__shift_expr_RIGHT_OP_additive_expr, shift_expr );
  _gr->AddRhsSymbol( shift_expr__shift_expr_RIGHT_OP_additive_expr, RIGHT_OP );
  _gr->AddRhsSymbol( shift_expr__shift_expr_RIGHT_OP_additive_expr, additive_expr );
  
  // relational_expr
  // : shift_expr
  // | relational_expr '<' shift_expr
  // | relational_expr '>' shift_expr
  // | relational_expr LE_OP shift_expr
  // | relational_expr GE_OP shift_expr
  
  _gr->AddRule( relational_expr__shift_expr, "relational_expr --> shift_expr" );
  _gr->AddLhsSymbol( relational_expr__shift_expr, relational_expr );
  _gr->AddRhsSymbol( relational_expr__shift_expr, shift_expr );

  _gr->AddRule( relational_expr__relational_expr_LESS_shift_expr, "relational_expr --> relational_expr < shift_expr" );
  _gr->AddLhsSymbol( relational_expr__relational_expr_LESS_shift_expr, relational_expr );
  _gr->AddRhsSymbol( relational_expr__relational_expr_LESS_shift_expr, relational_expr );
  _gr->AddRhsSymbol( relational_expr__relational_expr_LESS_shift_expr, LESS );
  _gr->AddRhsSymbol( relational_expr__relational_expr_LESS_shift_expr, shift_expr );

  _gr->AddRule( relational_expr__relational_expr_MORE_shift_expr, "relational_expr --> relational_expr > shift_expr" );
  _gr->AddLhsSymbol( relational_expr__relational_expr_MORE_shift_expr, relational_expr );
  _gr->AddRhsSymbol( relational_expr__relational_expr_MORE_shift_expr, relational_expr );
  _gr->AddRhsSymbol( relational_expr__relational_expr_MORE_shift_expr, MORE );
  _gr->AddRhsSymbol( relational_expr__relational_expr_MORE_shift_expr, shift_expr );

  _gr->AddRule( relational_expr__relational_expr_LE_OP_shift_expr, "relational_expr --> relational_expr <= shift_expr" );
  _gr->AddLhsSymbol( relational_expr__relational_expr_LE_OP_shift_expr, relational_expr );
  _gr->AddRhsSymbol( relational_expr__relational_expr_LE_OP_shift_expr, relational_expr );
  _gr->AddRhsSymbol( relational_expr__relational_expr_LE_OP_shift_expr, LE_OP );
  _gr->AddRhsSymbol( relational_expr__relational_expr_LE_OP_shift_expr, shift_expr );

  _gr->AddRule( relational_expr__relational_expr_GE_OP_shift_expr, "relational_expr --> relational_expr >= shift_expr" );
  _gr->AddLhsSymbol( relational_expr__relational_expr_GE_OP_shift_expr, relational_expr );
  _gr->AddRhsSymbol( relational_expr__relational_expr_GE_OP_shift_expr, relational_expr );
  _gr->AddRhsSymbol( relational_expr__relational_expr_GE_OP_shift_expr, GE_OP );
  _gr->AddRhsSymbol( relational_expr__relational_expr_GE_OP_shift_expr, shift_expr );
  
  // equality_expr
  // : relational_expr
  // | equality_expr EQ_OP relational_expr
  // | equality_expr NE_OP relational_expr
  
  _gr->AddRule( equality_expr__relational_expr, "equality_expr --> relational_expr" );
  _gr->AddLhsSymbol( equality_expr__relational_expr, equality_expr );
  _gr->AddRhsSymbol( equality_expr__relational_expr, relational_expr );

  _gr->AddRule( equality_expr__equality_expr_EQ_OP_relational_expr, "equality_expr --> equality_expr == relational_expr" );
  _gr->AddLhsSymbol( equality_expr__equality_expr_EQ_OP_relational_expr, equality_expr );
  _gr->AddRhsSymbol( equality_expr__equality_expr_EQ_OP_relational_expr, equality_expr );
  _gr->AddRhsSymbol( equality_expr__equality_expr_EQ_OP_relational_expr, EQ_OP );
  _gr->AddRhsSymbol( equality_expr__equality_expr_EQ_OP_relational_expr, relational_expr );

  _gr->AddRule( equality_expr__equality_expr_NE_OP_relational_expr, "equality_expr --> equality_expr != relational_expr" );
  _gr->AddLhsSymbol( equality_expr__equality_expr_NE_OP_relational_expr, equality_expr );
  _gr->AddRhsSymbol( equality_expr__equality_expr_NE_OP_relational_expr, equality_expr );
  _gr->AddRhsSymbol( equality_expr__equality_expr_NE_OP_relational_expr, NE_OP );
  _gr->AddRhsSymbol( equality_expr__equality_expr_NE_OP_relational_expr, relational_expr );
  
  // and_expr
  // : equality_expr
  // | and_expr '&' equality_expr
  
  _gr->AddRule( and_expr__equality_expr, "and_expr --> equality_expr" );
  _gr->AddLhsSymbol( and_expr__equality_expr, and_expr );
  _gr->AddRhsSymbol( and_expr__equality_expr, equality_expr );

  _gr->AddRule( and_expr__and_expr_AND_equality_expr, "and_expr --> and_expr & equality_expr" );
  _gr->AddLhsSymbol( and_expr__and_expr_AND_equality_expr, and_expr );
  _gr->AddRhsSymbol( and_expr__and_expr_AND_equality_expr, and_expr );
  _gr->AddRhsSymbol( and_expr__and_expr_AND_equality_expr, AND );
  _gr->AddRhsSymbol( and_expr__and_expr_AND_equality_expr, equality_expr );


  // exclusive_or_expr
  // : and_expr
  // | exclusive_or_expr '^' and_expr

  _gr->AddRule( exclusive_or_expr__and_expr, "exclusive_or_expr --> and_expr" );
  _gr->AddLhsSymbol( exclusive_or_expr__and_expr, exclusive_or_expr );
  _gr->AddRhsSymbol( exclusive_or_expr__and_expr, and_expr );

  _gr->AddRule( exclusive_or_expr__exclusive_or_expr_XOR_and_expr, "exclusive_or_expr --> exclusive_or_expr ^ and_expr" );
  _gr->AddLhsSymbol( exclusive_or_expr__exclusive_or_expr_XOR_and_expr, exclusive_or_expr );
  _gr->AddRhsSymbol( exclusive_or_expr__exclusive_or_expr_XOR_and_expr, exclusive_or_expr );
  _gr->AddRhsSymbol( exclusive_or_expr__exclusive_or_expr_XOR_and_expr, XOR );
  _gr->AddRhsSymbol( exclusive_or_expr__exclusive_or_expr_XOR_and_expr, and_expr );
  
  // inclusive_or_expr
  // : exclusive_or_expr
  // | inclusive_or_expr '|' exclusive_or_expr

  _gr->AddRule( inclusive_or_expr__exclusive_or_expr, "inclusive_or_expr --> exclusive_or_expr" );
  _gr->AddLhsSymbol( inclusive_or_expr__exclusive_or_expr, inclusive_or_expr );
  _gr->AddRhsSymbol( inclusive_or_expr__exclusive_or_expr, exclusive_or_expr );
  
  _gr->AddRule( inclusive_or_expr__inclusive_or_expr_OR_exclusive_or_expr, "inclusive_or_expr --> inclusive_or_expr & exclusive_or_expr" );
  _gr->AddLhsSymbol( inclusive_or_expr__inclusive_or_expr_OR_exclusive_or_expr, inclusive_or_expr );
  _gr->AddRhsSymbol( inclusive_or_expr__inclusive_or_expr_OR_exclusive_or_expr, inclusive_or_expr );
  _gr->AddRhsSymbol( inclusive_or_expr__inclusive_or_expr_OR_exclusive_or_expr, OR );
  _gr->AddRhsSymbol( inclusive_or_expr__inclusive_or_expr_OR_exclusive_or_expr, exclusive_or_expr );
  
  // logical_and_expr
  // : inclusive_or_expr
  // | logical_and_expr AND_OP inclusive_or_expr
  
  _gr->AddRule( logical_and_expr__inclusive_or_expr, "logical_and_expr --> inclusive_or_expr" );
  _gr->AddLhsSymbol( logical_and_expr__inclusive_or_expr, logical_and_expr );
  _gr->AddRhsSymbol( logical_and_expr__inclusive_or_expr, inclusive_or_expr );

  _gr->AddRule( logical_and_expr__logical_and_expr_AND_OP_inclusive_or_expr, "logical_and_expr --> logical_and_expr && inclusive_or_expr" );
  _gr->AddLhsSymbol( logical_and_expr__logical_and_expr_AND_OP_inclusive_or_expr, logical_and_expr );
  _gr->AddRhsSymbol( logical_and_expr__logical_and_expr_AND_OP_inclusive_or_expr, logical_and_expr );
  _gr->AddRhsSymbol( logical_and_expr__logical_and_expr_AND_OP_inclusive_or_expr, AND_OP );
  _gr->AddRhsSymbol( logical_and_expr__logical_and_expr_AND_OP_inclusive_or_expr, inclusive_or_expr );
  
  // logical_or_expr
  // : logical_and_expr
  // | logical_or_expr OR_OP logical_and_expr
  
  _gr->AddRule( logical_or_expr__logical_and_expr, "logical_or_expr --> logical_and_expr" );
  _gr->AddLhsSymbol( logical_or_expr__logical_and_expr, logical_or_expr );
  _gr->AddRhsSymbol( logical_or_expr__logical_and_expr, logical_and_expr );
  
  _gr->AddRule( logical_or_expr__logical_or_expr_OR_OP_logical_and_expr, "logical_or_expr --> logical_or_expr || logical_and_expr" );
  _gr->AddLhsSymbol( logical_or_expr__logical_or_expr_OR_OP_logical_and_expr, logical_or_expr );
  _gr->AddRhsSymbol( logical_or_expr__logical_or_expr_OR_OP_logical_and_expr, logical_or_expr );
  _gr->AddRhsSymbol( logical_or_expr__logical_or_expr_OR_OP_logical_and_expr, OR_OP );
  _gr->AddRhsSymbol( logical_or_expr__logical_or_expr_OR_OP_logical_and_expr, logical_and_expr );
  
  // conditional_expr
  // : logical_or_expr
  // | logical_or_expr '?' logical_or_expr ':' conditional_expr
  
  _gr->AddRule( conditional_expr__logical_or_expr, "conditional_expr --> logical_or_expr" );
  _gr->AddLhsSymbol( conditional_expr__logical_or_expr, conditional_expr );
  _gr->AddRhsSymbol( conditional_expr__logical_or_expr, logical_or_expr );
  
  _gr->AddRule( conditional_expr__logical_or_expr_QUESTION_logical_or_expr_COLON_conditional_expr, "conditional_expr --> logical_or_expr ? logical_or_expr : conditional_expr" );
  _gr->AddLhsSymbol( conditional_expr__logical_or_expr_QUESTION_logical_or_expr_COLON_conditional_expr, logical_or_expr );
  _gr->AddRhsSymbol( conditional_expr__logical_or_expr_QUESTION_logical_or_expr_COLON_conditional_expr, logical_or_expr );
  _gr->AddRhsSymbol( conditional_expr__logical_or_expr_QUESTION_logical_or_expr_COLON_conditional_expr, QUESTION );
  _gr->AddRhsSymbol( conditional_expr__logical_or_expr_QUESTION_logical_or_expr_COLON_conditional_expr, logical_or_expr );
  _gr->AddRhsSymbol( conditional_expr__logical_or_expr_QUESTION_logical_or_expr_COLON_conditional_expr, COLON );
  _gr->AddRhsSymbol( conditional_expr__logical_or_expr_QUESTION_logical_or_expr_COLON_conditional_expr, conditional_expr );
  
  // assignment_expr
  // : conditional_expr
  // | unary_expr assignment_operator assignment_expr
  
  _gr->AddRule( assignment_expr__conditional_expr, "assignment_expr --> conditional_expr" );
  _gr->AddLhsSymbol( assignment_expr__conditional_expr, assignment_expr );
  _gr->AddRhsSymbol( assignment_expr__conditional_expr, conditional_expr );
  
  _gr->AddRule( assignment_expr__unary_expr_assignment_operator_assignment_expr, "assignment_expr --> unary_expr assignment_operator assignment_expr" );
  _gr->AddLhsSymbol( assignment_expr__unary_expr_assignment_operator_assignment_expr, assignment_expr );
  _gr->AddRhsSymbol( assignment_expr__unary_expr_assignment_operator_assignment_expr, unary_expr );
  _gr->AddRhsSymbol( assignment_expr__unary_expr_assignment_operator_assignment_expr, assignment_operator );
  _gr->AddRhsSymbol( assignment_expr__unary_expr_assignment_operator_assignment_expr, assignment_expr );

  // assignment_operator
  // : '='
  // | MUL_ASSIGN
  // | DIV_ASSIGN
  // | MOD_ASSIGN
  // | ADD_ASSIGN
  // | SUB_ASSIGN
  // | LEFT_ASSIGN
  // | RIGHT_ASSIGN
  // | AND_ASSIGN
  // | XOR_ASSIGN
  // | OR_ASSIGN

  _gr->AddRule( assignment_operator__EQUAL, "assignment_operator --> =" );
  _gr->AddLhsSymbol( assignment_operator__EQUAL, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__EQUAL, EQUAL );

  _gr->AddRule( assignment_operator__MUL_ASSIGN, "assignment_operator --> *=" );
  _gr->AddLhsSymbol( assignment_operator__MUL_ASSIGN, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__MUL_ASSIGN, MUL_ASSIGN );

  _gr->AddRule( assignment_operator__DIV_ASSIGN, "assignment_operator /=" );
  _gr->AddLhsSymbol( assignment_operator__DIV_ASSIGN, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__DIV_ASSIGN, DIV_ASSIGN );

  _gr->AddRule( assignment_operator__MOD_ASSIGN, "assignment_operator --> %=" );
  _gr->AddLhsSymbol( assignment_operator__MOD_ASSIGN, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__MOD_ASSIGN, MOD_ASSIGN );

  _gr->AddRule( assignment_operator__ADD_ASSIGN, "assignment_operator --> +=" );
  _gr->AddLhsSymbol( assignment_operator__ADD_ASSIGN, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__ADD_ASSIGN, ADD_ASSIGN );

  _gr->AddRule( assignment_operator__SUB_ASSIGN, "assignment_operator --> -=" );
  _gr->AddLhsSymbol( assignment_operator__SUB_ASSIGN, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__SUB_ASSIGN, SUB_ASSIGN );

  _gr->AddRule( assignment_operator__LEFT_ASSIGN, "assignment_operator --> <<=" );
  _gr->AddLhsSymbol( assignment_operator__LEFT_ASSIGN, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__LEFT_ASSIGN, LEFT_ASSIGN );

  _gr->AddRule( assignment_operator__RIGHT_ASSIGN, "assignment_operator >>=" );
  _gr->AddLhsSymbol( assignment_operator__RIGHT_ASSIGN, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__RIGHT_ASSIGN, RIGHT_ASSIGN );

  _gr->AddRule( assignment_operator__AND_ASSIGN, "assignment_operator --> &=" );
  _gr->AddLhsSymbol( assignment_operator__AND_ASSIGN, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__AND_ASSIGN, AND_ASSIGN );

  _gr->AddRule( assignment_operator__XOR_ASSIGN, "assignment_operator --> ^=" );
  _gr->AddLhsSymbol( assignment_operator__XOR_ASSIGN, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__XOR_ASSIGN, XOR_ASSIGN );

  _gr->AddRule( assignment_operator__OR_ASSIGN, "assignment_operator --> |=" );
  _gr->AddLhsSymbol( assignment_operator__OR_ASSIGN, assignment_operator );
  _gr->AddRhsSymbol( assignment_operator__OR_ASSIGN, OR_ASSIGN );

  // expr
  // : assignment_expr
  // | expr ',' assignment_expr

  _gr->AddRule( expr__assignment_expr, "expr --> assignment_expr" );
  _gr->AddLhsSymbol( expr__assignment_expr, expr );
  _gr->AddRhsSymbol( expr__assignment_expr, assignment_expr );

  _gr->AddRule( expr__expr_COMMA_assignment_expr, "expr --> expr , assignment_expr" );
  _gr->AddLhsSymbol( expr__expr_COMMA_assignment_expr, expr );
  _gr->AddRhsSymbol( expr__expr_COMMA_assignment_expr, expr );
  _gr->AddRhsSymbol( expr__expr_COMMA_assignment_expr, COMMA );
  _gr->AddRhsSymbol( expr__expr_COMMA_assignment_expr, assignment_expr );
  
  // constant_expr
  // : conditional_expr

  _gr->AddRule( constant_expr__conditional_expr, "constant_expr --> conditional_expr" );
  _gr->AddLhsSymbol( constant_expr__conditional_expr, constant_expr );
  _gr->AddRhsSymbol( constant_expr__conditional_expr, conditional_expr );
  
  // declaration
  // : declaration_specifiers ';'
  // | declaration_specifiers init_declarator_list ';'
  
  _gr->AddRule( declaration__declaration_specifiers_SEMICOLON, "declaration --> declaration_specifiers ;" );
  _gr->AddLhsSymbol( declaration__declaration_specifiers_SEMICOLON, declaration );
  _gr->AddRhsSymbol( declaration__declaration_specifiers_SEMICOLON, declaration_specifiers );
  _gr->AddRhsSymbol( declaration__declaration_specifiers_SEMICOLON, SEMICOLON );
  
  _gr->AddRule( declaration__declaration_specifiers_init_declarator_list_SEMICOLON, "declaration --> declaration_specifiers init_declarator_list ;" );
  _gr->AddLhsSymbol( declaration__declaration_specifiers_init_declarator_list_SEMICOLON, declaration );
  _gr->AddRhsSymbol( declaration__declaration_specifiers_init_declarator_list_SEMICOLON, declaration_specifiers );
  _gr->AddRhsSymbol( declaration__declaration_specifiers_init_declarator_list_SEMICOLON, init_declarator_list );
  _gr->AddRhsSymbol( declaration__declaration_specifiers_init_declarator_list_SEMICOLON, SEMICOLON );
  
  // declaration_specifiers
  // : storage_class_specifier
  // | storage_class_specifier declaration_specifiers
  // | type_specifier
  // | type_specifier declaration_specifiers
  
  _gr->AddRule( declaration_specifiers__storage_class_specifier, "declaration_specifiers --> storage_class_specifier" );
  _gr->AddLhsSymbol( declaration_specifiers__storage_class_specifier, declaration_specifiers );
  _gr->AddRhsSymbol( declaration_specifiers__storage_class_specifier, storage_class_specifier );
  
  _gr->AddRule( declaration_specifiers__storage_class_specifier_declaration_specifiers, "declaration_specifiers --> storage_class_specifier declaration_specifiers" );
  _gr->AddLhsSymbol( declaration_specifiers__storage_class_specifier_declaration_specifiers, declaration_specifiers );
  _gr->AddRhsSymbol( declaration_specifiers__storage_class_specifier_declaration_specifiers, storage_class_specifier );
  _gr->AddRhsSymbol( declaration_specifiers__storage_class_specifier_declaration_specifiers, declaration_specifiers );
  
  _gr->AddRule( declaration_specifiers__type_specifier, "declaration_specifiers --> type_specifier" );
  _gr->AddLhsSymbol( declaration_specifiers__type_specifier, declaration_specifiers );
  _gr->AddRhsSymbol( declaration_specifiers__type_specifier, type_specifier );
  
  _gr->AddRule( declaration_specifiers__type_specifier_declaration_specifiers, "declaration_specifiers --> type_specifier declaration_specifiers" );
  _gr->AddLhsSymbol( declaration_specifiers__type_specifier_declaration_specifiers, declaration_specifiers );
  _gr->AddRhsSymbol( declaration_specifiers__type_specifier_declaration_specifiers, type_specifier );
  _gr->AddRhsSymbol( declaration_specifiers__type_specifier_declaration_specifiers, declaration_specifiers );
  
  // init_declarator_list
  // : init_declarator
  // | init_declarator_list ',' init_declarator
  
  _gr->AddRule( init_declarator_list__init_declarator, "init_declarator_list --> init_declarator" );
  _gr->AddLhsSymbol( init_declarator_list__init_declarator, init_declarator_list );
  _gr->AddRhsSymbol( init_declarator_list__init_declarator, init_declarator );
  
  _gr->AddRule( init_declarator_list__init_declarator_list_COMMA_init_declarator, "init_declarator_list --> init_declarator_list , init_declarator" );
  _gr->AddLhsSymbol( init_declarator_list__init_declarator_list_COMMA_init_declarator, init_declarator_list );
  _gr->AddRhsSymbol( init_declarator_list__init_declarator_list_COMMA_init_declarator, init_declarator_list );
  _gr->AddRhsSymbol( init_declarator_list__init_declarator_list_COMMA_init_declarator, COMMA );
  _gr->AddRhsSymbol( init_declarator_list__init_declarator_list_COMMA_init_declarator, init_declarator );
  
  // init_declarator
  // : declarator
  // | declarator '=' initializer
  
  _gr->AddRule( init_declarator__declarator, "init_declarator --> declarator" );
  _gr->AddLhsSymbol( init_declarator__declarator, init_declarator );
  _gr->AddRhsSymbol( init_declarator__declarator, declarator );
  
  _gr->AddRule( init_declarator__declarator_EQUAL_initializer, "init_declarator --> declarator = initializer" );
  _gr->AddLhsSymbol( init_declarator__declarator_EQUAL_initializer, init_declarator );
  _gr->AddRhsSymbol( init_declarator__declarator_EQUAL_initializer, declarator );
  _gr->AddRhsSymbol( init_declarator__declarator_EQUAL_initializer, EQUAL );
  _gr->AddRhsSymbol( init_declarator__declarator_EQUAL_initializer, initializer );
  
  // storage_class_specifier
  // : TYPEDEF
  // | EXTERN
  // | STATIC
  // | AUTO
  // | REGISTER
  
  _gr->AddRule( storage_class_specifier__TYPEDEF, "storage_class_specifier --> typedef" );
  _gr->AddLhsSymbol( storage_class_specifier__TYPEDEF, storage_class_specifier );
  _gr->AddRhsSymbol( storage_class_specifier__TYPEDEF, TYPEDEF );
  
  _gr->AddRule( storage_class_specifier__EXTERN, "storage_class_specifier --> extern" );
  _gr->AddLhsSymbol( storage_class_specifier__EXTERN, storage_class_specifier );
  _gr->AddRhsSymbol( storage_class_specifier__EXTERN, EXTERN );
  
  _gr->AddRule( storage_class_specifier__STATIC, "storage_class_specifier --> static" );
  _gr->AddLhsSymbol( storage_class_specifier__STATIC, storage_class_specifier );
  _gr->AddRhsSymbol( storage_class_specifier__STATIC, STATIC );
  
  _gr->AddRule( storage_class_specifier__AUTO, "storage_class_specifier --> auto" );
  _gr->AddLhsSymbol( storage_class_specifier__AUTO, storage_class_specifier );
  _gr->AddRhsSymbol( storage_class_specifier__AUTO, AUTO );
  
  _gr->AddRule( storage_class_specifier__REGISTER, "storage_class_specifier --> register" );
  _gr->AddLhsSymbol( storage_class_specifier__REGISTER, storage_class_specifier );
  _gr->AddRhsSymbol( storage_class_specifier__REGISTER, REGISTER );
  
  // type_specifier
  // : CHAR
  // | SHORT
  // | INT
  // | LONG
  // | SIGNED
  // | UNSIGNED
  // | FLOAT
  // | DOUBLE
  // | CONST
  // | VOLATILE
  // | VOID
  // | struct_or_union_specifier
  // | enum_specifier
  // | TYPE_NAME
  
  _gr->AddRule( type_specifier__CHAR, "type_specifier --> char" );
  _gr->AddLhsSymbol( type_specifier__CHAR, type_specifier );
  _gr->AddRhsSymbol( type_specifier__CHAR, CHAR );

  _gr->AddRule( type_specifier__SHORT, "type_specifier --> short" );
  _gr->AddLhsSymbol( type_specifier__SHORT, type_specifier );
  _gr->AddRhsSymbol( type_specifier__SHORT, SHORT );

  _gr->AddRule( type_specifier__INT, "type_specifier --> int" );
  _gr->AddLhsSymbol( type_specifier__INT, type_specifier );
  _gr->AddRhsSymbol( type_specifier__INT, INT );

  _gr->AddRule( type_specifier__LONG, "type_specifier --> long" );
  _gr->AddLhsSymbol( type_specifier__LONG, type_specifier );
  _gr->AddRhsSymbol( type_specifier__LONG, LONG );

  _gr->AddRule( type_specifier__SIGNED, "type_specifier --> signed" );
  _gr->AddLhsSymbol( type_specifier__SIGNED, type_specifier );
  _gr->AddRhsSymbol( type_specifier__SIGNED, SIGNED );

  _gr->AddRule( type_specifier__UNSIGNED, "type_specifier --> unsigned" );
  _gr->AddLhsSymbol( type_specifier__UNSIGNED, type_specifier );
  _gr->AddRhsSymbol( type_specifier__UNSIGNED, UNSIGNED );

  _gr->AddRule( type_specifier__FLOAT, "type_specifier --> float" );
  _gr->AddLhsSymbol( type_specifier__FLOAT, type_specifier );
  _gr->AddRhsSymbol( type_specifier__FLOAT, FLOAT );

  _gr->AddRule( type_specifier__DOUBLE, "type_specifier --> double" );
  _gr->AddLhsSymbol( type_specifier__DOUBLE, type_specifier );
  _gr->AddRhsSymbol( type_specifier__DOUBLE, DOUBLE );

  _gr->AddRule( type_specifier__CONST, "type_specifier --> const" );
  _gr->AddLhsSymbol( type_specifier__CONST, type_specifier );
  _gr->AddRhsSymbol( type_specifier__CONST, CONST );

  _gr->AddRule( type_specifier__VOLATILE, "type_specifier --> volatile" );
  _gr->AddLhsSymbol( type_specifier__VOLATILE, type_specifier );
  _gr->AddRhsSymbol( type_specifier__VOLATILE, VOLATILE );

  _gr->AddRule( type_specifier__VOID, "type_specifier --> void" );
  _gr->AddLhsSymbol( type_specifier__VOID, type_specifier );
  _gr->AddRhsSymbol( type_specifier__VOID, VOID );

  _gr->AddRule( type_specifier__struct_or_union_specifier, "type_specifier --> struct_or_union_specifier" );
  _gr->AddLhsSymbol( type_specifier__struct_or_union_specifier, type_specifier );
  _gr->AddRhsSymbol( type_specifier__struct_or_union_specifier, struct_or_union_specifier );

  _gr->AddRule( type_specifier__enum_specifier, "type_specifier --> enum_specifier" );
  _gr->AddLhsSymbol( type_specifier__enum_specifier, type_specifier );
  _gr->AddRhsSymbol( type_specifier__enum_specifier, enum_specifier );
  
  // struct_or_union_specifier
  // : struct_or_union identifier '{' struct_declaration_list '}'
  // | struct_or_union '{' struct_declaration_list '}'
  // | struct_or_union identifier
  
  _gr->AddRule( struct_or_union_specifier__struct_or_union_identifier_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, "struct_or_union_specifier --> struct_or_union identifier { struct_declaration_list }" );
  _gr->AddLhsSymbol( struct_or_union_specifier__struct_or_union_identifier_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, struct_or_union_specifier );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_identifier_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, struct_or_union );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_identifier_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, identifier );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_identifier_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, LEFT_CL_BRACKET );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_identifier_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, struct_declaration_list );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_identifier_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, RIGHT_CL_BRACKET );

  _gr->AddRule( struct_or_union_specifier__struct_or_union_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, "struct_or_union_specifier --> struct_or_union { struct_declaration_list }" );
  _gr->AddLhsSymbol( struct_or_union_specifier__struct_or_union_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, struct_or_union_specifier );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, struct_or_union );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, LEFT_CL_BRACKET );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, struct_declaration_list );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_LEFT_CL_BRACKET_struct_declaration_list_RIGHT_CL_BRACKET, RIGHT_CL_BRACKET );

  _gr->AddRule( struct_or_union_specifier__struct_or_union_identifier, "struct_or_union_specifier --> struct_or_union identifier" );
  _gr->AddLhsSymbol( struct_or_union_specifier__struct_or_union_identifier, struct_or_union_specifier );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_identifier, struct_or_union );
  _gr->AddRhsSymbol( struct_or_union_specifier__struct_or_union_identifier, identifier );
  
  // struct_or_union
  // : STRUCT
  // | UNION
  
  _gr->AddRule( struct_or_union__STRUCT, "struct_or_union --> struct" );
  _gr->AddLhsSymbol( struct_or_union__STRUCT, struct_or_union );
  _gr->AddRhsSymbol( struct_or_union__STRUCT, STRUCT );
  
  _gr->AddRule( struct_or_union__UNION, "struct_or_union --> union" );
  _gr->AddLhsSymbol( struct_or_union__UNION, struct_or_union );
  _gr->AddRhsSymbol( struct_or_union__UNION, UNION );
  
  // struct_declaration_list
  // : struct_declaration
  // | struct_declaration_list struct_declaration
  
  _gr->AddRule( struct_declaration_list__struct_declaration, "struct_declaration_list --> struct_declaration" );
  _gr->AddLhsSymbol( struct_declaration_list__struct_declaration, struct_declaration_list );
  _gr->AddRhsSymbol( struct_declaration_list__struct_declaration, struct_declaration );
  
  _gr->AddRule( struct_declaration_list__struct_declaration_list_struct_declaration, "struct_declaration_list --> struct_declaration_list struct_declaration" );
  _gr->AddLhsSymbol( struct_declaration_list__struct_declaration_list_struct_declaration, struct_declaration_list );
  _gr->AddRhsSymbol( struct_declaration_list__struct_declaration_list_struct_declaration, struct_declaration_list );
  _gr->AddRhsSymbol( struct_declaration_list__struct_declaration_list_struct_declaration, struct_declaration );
  
  // struct_declaration
  // : type_specifier_list struct_declarator_list ';'
  
  _gr->AddRule( struct_declaration__type_specifier_list_struct_declarator_list_SEMICOLON, "struct_declaration --> type_specifier_list struct_declarator_list ;" );
  _gr->AddLhsSymbol( struct_declaration__type_specifier_list_struct_declarator_list_SEMICOLON, struct_declaration );
  _gr->AddRhsSymbol( struct_declaration__type_specifier_list_struct_declarator_list_SEMICOLON, type_specifier_list );
  _gr->AddRhsSymbol( struct_declaration__type_specifier_list_struct_declarator_list_SEMICOLON, struct_declarator_list );
  _gr->AddRhsSymbol( struct_declaration__type_specifier_list_struct_declarator_list_SEMICOLON, SEMICOLON );
  
  // struct_declarator_list
  // : struct_declarator
  // | struct_declarator_list ',' struct_declarator
  
  _gr->AddRule( struct_declarator_list__struct_declarator, "struct_declarator_list --> struct_declarator" );
  _gr->AddLhsSymbol( struct_declarator_list__struct_declarator, struct_declarator_list );
  _gr->AddRhsSymbol( struct_declarator_list__struct_declarator, struct_declarator );

  _gr->AddRule( struct_declarator_list__struct_declarator_list_COMMA_struct_declarator, "struct_declarator_list --> struct_declarator_list , struct_declarator" );
  _gr->AddLhsSymbol( struct_declarator_list__struct_declarator_list_COMMA_struct_declarator, struct_declarator_list );
  _gr->AddRhsSymbol( struct_declarator_list__struct_declarator_list_COMMA_struct_declarator, COMMA );
  _gr->AddRhsSymbol( struct_declarator_list__struct_declarator_list_COMMA_struct_declarator, struct_declarator );
  
  // struct_declarator
  // : declarator
  // | ':' constant_expr
  // | declarator ':' constant_expr
  
  _gr->AddRule( struct_declarator__declarator, "struct_declarator --> declarator" );
  _gr->AddLhsSymbol( struct_declarator__declarator, struct_declarator );
  _gr->AddRhsSymbol( struct_declarator__declarator, declarator );

  _gr->AddRule( struct_declarator__COLON_constant_expr, "struct_declarator --> : constant_expr" );
  _gr->AddLhsSymbol( struct_declarator__COLON_constant_expr, struct_declarator );
  _gr->AddRhsSymbol( struct_declarator__COLON_constant_expr, COLON );
  _gr->AddRhsSymbol( struct_declarator__COLON_constant_expr, constant_expr );

  _gr->AddRule( struct_declarator__declarator_COLON_constant_expr, "struct_declarator --> declarator ; constant_expr" );
  _gr->AddLhsSymbol( struct_declarator__declarator_COLON_constant_expr, struct_declarator );
  _gr->AddRhsSymbol( struct_declarator__declarator_COLON_constant_expr, declarator );
  _gr->AddRhsSymbol( struct_declarator__declarator_COLON_constant_expr, COLON );
  _gr->AddRhsSymbol( struct_declarator__declarator_COLON_constant_expr, constant_expr );
  
  // enum_specifier
  // : ENUM '{' enumerator_list '}'
  // | ENUM identifier '{' enumerator_list '}'
  // | ENUM identifier
  
  _gr->AddRule( enum_specifier__ENUM_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, "enum_specifier --> enum { enumerator_list }" );
  _gr->AddLhsSymbol( enum_specifier__ENUM_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, enum_specifier );
  _gr->AddRhsSymbol( enum_specifier__ENUM_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, ENUM );
  _gr->AddRhsSymbol( enum_specifier__ENUM_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, LEFT_CL_BRACKET );
  _gr->AddRhsSymbol( enum_specifier__ENUM_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, enumerator_list );
  _gr->AddRhsSymbol( enum_specifier__ENUM_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, RIGHT_CL_BRACKET );
  
  _gr->AddRule( enum_specifier__ENUM_identifier_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, "enum_specifier --> enum identifier { enumerator_list }" );
  _gr->AddLhsSymbol( enum_specifier__ENUM_identifier_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, enum_specifier );
  _gr->AddRhsSymbol( enum_specifier__ENUM_identifier_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, ENUM );
  _gr->AddRhsSymbol( enum_specifier__ENUM_identifier_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, identifier );
  _gr->AddRhsSymbol( enum_specifier__ENUM_identifier_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, LEFT_CL_BRACKET );
  _gr->AddRhsSymbol( enum_specifier__ENUM_identifier_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, enumerator_list );
  _gr->AddRhsSymbol( enum_specifier__ENUM_identifier_LEFT_CL_BRACKET_enumerator_list_RIGHT_CL_BRACKET, RIGHT_CL_BRACKET );
  
  _gr->AddRule( enum_specifier__ENUM_identifier, "enum_specifier --> enum identifier" );
  _gr->AddLhsSymbol( enum_specifier__ENUM_identifier, enum_specifier );
  _gr->AddRhsSymbol( enum_specifier__ENUM_identifier, ENUM );
  _gr->AddRhsSymbol( enum_specifier__ENUM_identifier, identifier );

  // enumerator_list
  // : enumerator
  // | enumerator_list ',' enumerator

  _gr->AddRule( enumerator_list__enumerator, "enumerator_list --> enumerator" );
  _gr->AddLhsSymbol( enumerator_list__enumerator, enumerator_list );
  _gr->AddRhsSymbol( enumerator_list__enumerator, enumerator );

  _gr->AddRule( enumerator_list__enumerator_list_COMMA_enumerator, "enumerator_list --> enumerator_list , enumerator" );
  _gr->AddLhsSymbol( enumerator_list__enumerator_list_COMMA_enumerator, enumerator_list );
  _gr->AddRhsSymbol( enumerator_list__enumerator_list_COMMA_enumerator, enumerator_list );
  _gr->AddRhsSymbol( enumerator_list__enumerator_list_COMMA_enumerator, COMMA );
  _gr->AddRhsSymbol( enumerator_list__enumerator_list_COMMA_enumerator, enumerator );
  
  // enumerator
  // : identifier
  // | identifier '=' constant_expr
  
  _gr->AddRule( enumerator__identifier, "enumerator --> identifier" );
  _gr->AddLhsSymbol( enumerator__identifier, enumerator );
  _gr->AddRhsSymbol( enumerator__identifier, identifier );

  _gr->AddRule( enumerator__identifier_EQUAL_constant_expr, "enumerator --> identifier = constant_expr" );
  _gr->AddLhsSymbol( enumerator__identifier_EQUAL_constant_expr, enumerator );
  _gr->AddRhsSymbol( enumerator__identifier_EQUAL_constant_expr, identifier );
  _gr->AddRhsSymbol( enumerator__identifier_EQUAL_constant_expr, EQUAL );
  _gr->AddRhsSymbol( enumerator__identifier_EQUAL_constant_expr, constant_expr );
  
  // declarator
  // : declarator2
  // | pointer declarator2
  
  _gr->AddRule( declarator__declarator2, "declarator --> declarator2" );
  _gr->AddLhsSymbol( declarator__declarator2, declarator );
  _gr->AddRhsSymbol( declarator__declarator2, declarator2 );

  _gr->AddRule( declarator__pointer_declarator2, "declarator --> pointer declarator2" );
  _gr->AddLhsSymbol( declarator__pointer_declarator2, declarator );
  _gr->AddRhsSymbol( declarator__pointer_declarator2, pointer );
  _gr->AddRhsSymbol( declarator__pointer_declarator2, declarator2 );
  
  // declarator2
  // : identifier
  // | '(' declarator ')'
  // | declarator2 '[' ']'
  // | declarator2 '[' constant_expr ']'
  // | declarator2 '(' ')'
  // | declarator2 '(' parameter_type_list ')'
  // | declarator2 '(' parameter_identifier_list ')'

  _gr->AddRule( declarator2__identifier, "declarator2 --> identifier" );
  _gr->AddLhsSymbol( declarator2__identifier, declarator2 );
  _gr->AddRhsSymbol( declarator2__identifier, identifier );

  _gr->AddRule( declarator2__LEFT_BRACE_declarator_RIGHT_BRACE, "declarator2 --> ( declarator )" );
  _gr->AddLhsSymbol( declarator2__LEFT_BRACE_declarator_RIGHT_BRACE, declarator2 );
  _gr->AddRhsSymbol( declarator2__LEFT_BRACE_declarator_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( declarator2__LEFT_BRACE_declarator_RIGHT_BRACE, declarator );
  _gr->AddRhsSymbol( declarator2__LEFT_BRACE_declarator_RIGHT_BRACE, RIGHT_BRACE );

  _gr->AddRule( declarator2__declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, "declarator2 --> declarator2 [ ]" );
  _gr->AddLhsSymbol( declarator2__declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, declarator2 );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, declarator2 );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, LEFT_SQ_BRACKET );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, RIGHT_SQ_BRACKET );

  _gr->AddRule( declarator2__declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, "declarator2 --> declarator2 [ constant_expr ]" );
  _gr->AddLhsSymbol( declarator2__declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, declarator2 );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, declarator2 );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, LEFT_SQ_BRACKET );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, constant_expr );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, RIGHT_SQ_BRACKET );

  _gr->AddRule( declarator2__declarator2_LEFT_BRACE_RIGHT_BRACE, "declarator2 --> declarator2 ( )" );
  _gr->AddLhsSymbol( declarator2__declarator2_LEFT_BRACE_RIGHT_BRACE, declarator2 );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_RIGHT_BRACE, declarator2 );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_RIGHT_BRACE, RIGHT_BRACE );

  _gr->AddRule( declarator2__declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, "declarator2 --> declarator2 ( parameter_type_list )" );
  _gr->AddLhsSymbol( declarator2__declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, declarator2 );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, declarator2 );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, parameter_type_list );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, RIGHT_BRACE );

  _gr->AddRule( declarator2__declarator2_LEFT_BRACE_parameter_identifier_list_RIGHT_BRACE, "declarator2 --> declarator2 ( parameter_identifier_list )" );
  _gr->AddLhsSymbol( declarator2__declarator2_LEFT_BRACE_parameter_identifier_list_RIGHT_BRACE, declarator2 );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_parameter_identifier_list_RIGHT_BRACE, declarator2 );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_parameter_identifier_list_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_parameter_identifier_list_RIGHT_BRACE, parameter_identifier_list );
  _gr->AddRhsSymbol( declarator2__declarator2_LEFT_BRACE_parameter_identifier_list_RIGHT_BRACE, RIGHT_BRACE );
  
  // pointer
  // : '*'
  // | '*' type_specifier_list
  // | '*' pointer
  // | '*' type_specifier_list pointer
  
  _gr->AddRule( pointer__STAR, "pointer --> *" );
  _gr->AddLhsSymbol( pointer__STAR, pointer );
  _gr->AddRhsSymbol( pointer__STAR, STAR );

  _gr->AddRule( pointer__STAR_type_specifier_list, "pointer --> * type_specifier_list" );
  _gr->AddLhsSymbol( pointer__STAR_type_specifier_list, pointer );
  _gr->AddRhsSymbol( pointer__STAR_type_specifier_list, STAR );
  _gr->AddRhsSymbol( pointer__STAR_type_specifier_list, type_specifier_list );

  _gr->AddRule( pointer__STAR_pointer, "pointer --> * pointer" );
  _gr->AddLhsSymbol( pointer__STAR_pointer, pointer );
  _gr->AddRhsSymbol( pointer__STAR_pointer, STAR );
  _gr->AddRhsSymbol( pointer__STAR_pointer, pointer );

  _gr->AddRule( pointer__STAR_type_specifier_list_pointer, "pointer --> * type_specifier_list pointer" );
  _gr->AddLhsSymbol( pointer__STAR_type_specifier_list_pointer, pointer );
  _gr->AddRhsSymbol( pointer__STAR_type_specifier_list_pointer, STAR );
  _gr->AddRhsSymbol( pointer__STAR_type_specifier_list_pointer, type_specifier_list );
  _gr->AddRhsSymbol( pointer__STAR_type_specifier_list_pointer, pointer );
  
  // type_specifier_list
  // : type_specifier
  // | type_specifier_list type_specifier
  
  _gr->AddRule( type_specifier_list__type_specifier, "type_specifier_list --> type_specifier" );
  _gr->AddLhsSymbol( type_specifier_list__type_specifier, type_specifier_list );
  _gr->AddRhsSymbol( type_specifier_list__type_specifier, type_specifier );

  _gr->AddRule( type_specifier_list__type_specifier_list_type_specifier, "type_specifier_list --> type_specifier_list type_specifier" );
  _gr->AddLhsSymbol( type_specifier_list__type_specifier_list_type_specifier, type_specifier_list );
  _gr->AddRhsSymbol( type_specifier_list__type_specifier_list_type_specifier, type_specifier_list );
  _gr->AddRhsSymbol( type_specifier_list__type_specifier_list_type_specifier, type_specifier );
  
  // parameter_identifier_list
  // : identifier_list
  // | identifier_list ',' ELIPSIS
  
  _gr->AddRule( parameter_identifier_list__identifier_list, "parameter_identifier_list --> identifier_list" );
  _gr->AddLhsSymbol( parameter_identifier_list__identifier_list, parameter_identifier_list );
  _gr->AddRhsSymbol( parameter_identifier_list__identifier_list, identifier_list );
  
  _gr->AddRule( parameter_identifier_list__identifier_list_COMMA_ELIPSIS, "parameter_identifier_list --> identifier_list , ..." );
  _gr->AddLhsSymbol( parameter_identifier_list__identifier_list_COMMA_ELIPSIS, parameter_identifier_list );
  _gr->AddRhsSymbol( parameter_identifier_list__identifier_list_COMMA_ELIPSIS, identifier_list );
  _gr->AddRhsSymbol( parameter_identifier_list__identifier_list_COMMA_ELIPSIS, COMMA );
  _gr->AddRhsSymbol( parameter_identifier_list__identifier_list_COMMA_ELIPSIS, ELIPSIS );
  
  // identifier_list
  // : identifier
  // | identifier_list ',' identifier
  
  _gr->AddRule( identifier_list__identifier, "identifier_list --> identifier" );
  _gr->AddLhsSymbol( identifier_list__identifier, identifier_list );
  _gr->AddRhsSymbol( identifier_list__identifier, identifier );

  _gr->AddRule( identifier_list__identifier_list_COMMA_identifier, "identifier_list --> identifier_list , identifier" );
  _gr->AddLhsSymbol( identifier_list__identifier_list_COMMA_identifier, identifier_list );
  _gr->AddRhsSymbol( identifier_list__identifier_list_COMMA_identifier, identifier_list );
  _gr->AddRhsSymbol( identifier_list__identifier_list_COMMA_identifier, COMMA );
  _gr->AddRhsSymbol( identifier_list__identifier_list_COMMA_identifier, identifier );
  
  // parameter_type_list
  // : parameter_list
  // | parameter_list ',' ELIPSIS
  
  _gr->AddRule( parameter_type_list__parameter_list, "parameter_type_list --> parameter_list" );
  _gr->AddLhsSymbol( parameter_type_list__parameter_list, parameter_type_list );
  _gr->AddRhsSymbol( parameter_type_list__parameter_list, parameter_list );

  _gr->AddRule( parameter_type_list__parameter_list_COMMA_ELIPSIS, "parameter_type_list --> parameter_list , ..." );
  _gr->AddLhsSymbol( parameter_type_list__parameter_list_COMMA_ELIPSIS, parameter_type_list );
  _gr->AddRhsSymbol( parameter_type_list__parameter_list_COMMA_ELIPSIS, parameter_list );
  _gr->AddRhsSymbol( parameter_type_list__parameter_list_COMMA_ELIPSIS, COMMA );
  _gr->AddRhsSymbol( parameter_type_list__parameter_list_COMMA_ELIPSIS, ELIPSIS );
  
  // parameter_list
  // : parameter_declaration
  // | parameter_list ',' parameter_declaration
  
  _gr->AddRule( parameter_list__parameter_declaration, "parameter_list --> parameter_declaration" );
  _gr->AddLhsSymbol( parameter_list__parameter_declaration, parameter_list );
  _gr->AddRhsSymbol( parameter_list__parameter_declaration, parameter_declaration );
  
  _gr->AddRule( parameter_list__parameter_list_COMMA_parameter_declaration, "parameter_list --> parameter_list , parameter_declaration" );
  _gr->AddLhsSymbol( parameter_list__parameter_list_COMMA_parameter_declaration, parameter_list );
  _gr->AddRhsSymbol( parameter_list__parameter_list_COMMA_parameter_declaration, parameter_list );
  _gr->AddRhsSymbol( parameter_list__parameter_list_COMMA_parameter_declaration, COMMA );
  _gr->AddRhsSymbol( parameter_list__parameter_list_COMMA_parameter_declaration, parameter_declaration );
  
  // parameter_declaration
  // : type_specifier_list declarator
  // | type_name
  
  _gr->AddRule( parameter_declaration__type_specifier_list_declarator, "parameter_declaration --> type_specifier_list declarator" );
  _gr->AddLhsSymbol( parameter_declaration__type_specifier_list_declarator, parameter_declaration );
  _gr->AddRhsSymbol( parameter_declaration__type_specifier_list_declarator, type_specifier_list );
  _gr->AddRhsSymbol( parameter_declaration__type_specifier_list_declarator, declarator );

  _gr->AddRule( parameter_declaration__type_name, "parameter_declaration --> type_name" );
  _gr->AddLhsSymbol( parameter_declaration__type_name, parameter_declaration );
  _gr->AddRhsSymbol( parameter_declaration__type_name, type_name );
  
  // type_name
  // : type_specifier_list
  // | type_specifier_list abstract_declarator
  
  _gr->AddRule( type_name__type_specifier_list, "type_name --> type_specifier_list" );
  _gr->AddLhsSymbol( type_name__type_specifier_list, type_name );
  _gr->AddRhsSymbol( type_name__type_specifier_list, type_specifier_list );

  _gr->AddRule( type_name__type_specifier_list_abstract_declarator, "type_name --> type_specifier_list abstract_declarator" );
  _gr->AddLhsSymbol( type_name__type_specifier_list_abstract_declarator, type_name );
  _gr->AddRhsSymbol( type_name__type_specifier_list_abstract_declarator, type_specifier_list );
  _gr->AddRhsSymbol( type_name__type_specifier_list_abstract_declarator, abstract_declarator );
  
  // abstract_declarator
  // : pointer
  // | abstract_declarator2
  // | pointer abstract_declarator2
  
  _gr->AddRule( abstract_declarator__pointer, "abstract_declarator --> pointer" );
  _gr->AddLhsSymbol( abstract_declarator__pointer, abstract_declarator );
  _gr->AddRhsSymbol( abstract_declarator__pointer, pointer );

  _gr->AddRule( abstract_declarator__abstract_declarator2, "abstract_declarator --> abstract_declarator2" );
  _gr->AddLhsSymbol( abstract_declarator__abstract_declarator2, abstract_declarator );
  _gr->AddRhsSymbol( abstract_declarator__abstract_declarator2, abstract_declarator2 );
  
  _gr->AddRule( abstract_declarator__pointer_abstract_declarator2, "abstract_declarator --> pointer abstract_declarator2" );
  _gr->AddLhsSymbol( abstract_declarator__pointer_abstract_declarator2, abstract_declarator );
  _gr->AddRhsSymbol( abstract_declarator__pointer_abstract_declarator2, pointer );
  _gr->AddRhsSymbol( abstract_declarator__pointer_abstract_declarator2, abstract_declarator2 );
  
  // abstract_declarator2
  // : '(' abstract_declarator ')'
  // | '[' ']'
  // | '[' constant_expr ']'
  // | abstract_declarator2 '[' ']'
  // | abstract_declarator2 '[' constant_expr ']'
  // | '(' ')'
  // | '(' parameter_type_list ')'
  // | abstract_declarator2 '(' ')'
  // | abstract_declarator2 '(' parameter_type_list ')'
  
  _gr->AddRule( abstract_declarator2__LEFT_BRACE_abstract_declarator_RIGHT_BRACE, "abstract_declarator2 --> ( abstract_declarator )" );
  _gr->AddLhsSymbol( abstract_declarator2__LEFT_BRACE_abstract_declarator_RIGHT_BRACE, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_BRACE_abstract_declarator_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_BRACE_abstract_declarator_RIGHT_BRACE, abstract_declarator );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_BRACE_abstract_declarator_RIGHT_BRACE, RIGHT_BRACE );
  
  _gr->AddRule( abstract_declarator2__LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, "abstract_declarator2 --> [ ]" );
  _gr->AddLhsSymbol( abstract_declarator2__LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, LEFT_BRACE );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, RIGHT_BRACE );
  
  _gr->AddRule( abstract_declarator2__LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, "abstract_declarator2 --> [ constant_expr ]" );
  _gr->AddLhsSymbol( abstract_declarator2__LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, LEFT_BRACE );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, constant_expr );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, RIGHT_BRACE );
  
  _gr->AddRule( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, "abstract_declarator2 --> abstract_declarator2 [ ]" );
  _gr->AddLhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, LEFT_BRACE );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_RIGHT_SQ_BRACKET, RIGHT_BRACE );
  
  _gr->AddRule( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, "abstract_declarator2 --> abstract_declarator2 [ constant_expr ]" );
  _gr->AddLhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, LEFT_BRACE );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, constant_expr );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_SQ_BRACKET_constant_expr_RIGHT_SQ_BRACKET, RIGHT_BRACE );
  
  _gr->AddRule( abstract_declarator2__LEFT_BRACE_RIGHT_BRACE, "abstract_declarator2 --> ( )" );
  _gr->AddLhsSymbol( abstract_declarator2__LEFT_BRACE_RIGHT_BRACE, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_BRACE_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_BRACE_RIGHT_BRACE, RIGHT_BRACE );
  
  _gr->AddRule( abstract_declarator2__LEFT_BRACE_parameter_type_list_RIGHT_BRACE, "abstract_declarator2 --> ( parameter_type_list )" );
  _gr->AddLhsSymbol( abstract_declarator2__LEFT_BRACE_parameter_type_list_RIGHT_BRACE, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_BRACE_parameter_type_list_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_BRACE_parameter_type_list_RIGHT_BRACE, parameter_type_list );
  _gr->AddRhsSymbol( abstract_declarator2__LEFT_BRACE_parameter_type_list_RIGHT_BRACE, RIGHT_BRACE );
  
  _gr->AddRule( abstract_declarator2__abstract_declarator2_LEFT_BRACE_RIGHT_BRACE, "abstract_declarator2 --> abstract_declarator2 ( )" );
  _gr->AddLhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_BRACE_RIGHT_BRACE, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_BRACE_RIGHT_BRACE, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_BRACE_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_BRACE_RIGHT_BRACE, RIGHT_BRACE );
  
  _gr->AddRule( abstract_declarator2__abstract_declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, "abstract_declarator2 --> abstract_declarator2 ( parameter_type_list )" );
  _gr->AddLhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, abstract_declarator2 );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, parameter_type_list );
  _gr->AddRhsSymbol( abstract_declarator2__abstract_declarator2_LEFT_BRACE_parameter_type_list_RIGHT_BRACE, RIGHT_BRACE );
  
  // initializer
  // : assignment_expr
  // | '{' initializer_list '}'
  // | '{' initializer_list ',' '}'
  
  _gr->AddRule( initializer__assignment_expr, "initializer --> assignment_expr" );
  _gr->AddLhsSymbol( initializer__assignment_expr, initializer );
  _gr->AddRhsSymbol( initializer__assignment_expr, assignment_expr );

  _gr->AddRule( initializer__LEFT_SQ_BRACKET_initializer_list_RIGHT_SQ_BRACKET, "initializer --> [ initializer_list ]" );
  _gr->AddLhsSymbol( initializer__LEFT_SQ_BRACKET_initializer_list_RIGHT_SQ_BRACKET, initializer );
  _gr->AddRhsSymbol( initializer__LEFT_SQ_BRACKET_initializer_list_RIGHT_SQ_BRACKET, LEFT_SQ_BRACKET );
  _gr->AddRhsSymbol( initializer__LEFT_SQ_BRACKET_initializer_list_RIGHT_SQ_BRACKET, initializer_list );
  _gr->AddRhsSymbol( initializer__LEFT_SQ_BRACKET_initializer_list_RIGHT_SQ_BRACKET, RIGHT_SQ_BRACKET );

  _gr->AddRule( initializer__LEFT_SQ_BRACKET_initializer_list_COMMA_RIGHT_SQ_BRACKET, "initializer --> [ initializer_list , ]" );
  _gr->AddLhsSymbol( initializer__LEFT_SQ_BRACKET_initializer_list_COMMA_RIGHT_SQ_BRACKET, initializer );
  _gr->AddRhsSymbol( initializer__LEFT_SQ_BRACKET_initializer_list_COMMA_RIGHT_SQ_BRACKET, LEFT_SQ_BRACKET );
  _gr->AddRhsSymbol( initializer__LEFT_SQ_BRACKET_initializer_list_COMMA_RIGHT_SQ_BRACKET, initializer_list );
  _gr->AddRhsSymbol( initializer__LEFT_SQ_BRACKET_initializer_list_COMMA_RIGHT_SQ_BRACKET, COMMA );
  _gr->AddRhsSymbol( initializer__LEFT_SQ_BRACKET_initializer_list_COMMA_RIGHT_SQ_BRACKET, RIGHT_SQ_BRACKET );
  
  // initializer_list
  // : initializer
  // | initializer_list ',' initializer
  
  _gr->AddRule( initializer_list__initializer, "initializer_list --> initializer" );
  _gr->AddLhsSymbol( initializer_list__initializer, initializer_list );
  _gr->AddRhsSymbol( initializer_list__initializer, initializer );

  _gr->AddRule( initializer_list__initializer_list_COMMA_initializer, "initializer_list --> initializer_list , initializer" );
  _gr->AddLhsSymbol( initializer_list__initializer_list_COMMA_initializer, initializer_list );
  _gr->AddRhsSymbol( initializer_list__initializer_list_COMMA_initializer, initializer_list );
  _gr->AddRhsSymbol( initializer_list__initializer_list_COMMA_initializer, COMMA );
  _gr->AddRhsSymbol( initializer_list__initializer_list_COMMA_initializer, initializer );
  
  // statement
  // : labeled_statement
  // | compound_statement
  // | expression_statement
  // | selection_statement
  // | iteration_statement
  // | jump_statement
  
  _gr->AddRule( statement__labeled_statement, "statement --> labeled_statement" );
  _gr->AddLhsSymbol( statement__labeled_statement, statement );
  _gr->AddRhsSymbol( statement__labeled_statement, labeled_statement );

  _gr->AddRule( statement__compound_statement, "statement --> compound_statement" );
  _gr->AddLhsSymbol( statement__compound_statement, statement );
  _gr->AddRhsSymbol( statement__compound_statement, compound_statement );

  _gr->AddRule( statement__expression_statement, "statement --> expression_statement" );
  _gr->AddLhsSymbol( statement__expression_statement, statement );
  _gr->AddRhsSymbol( statement__expression_statement, expression_statement );

  _gr->AddRule( statement__selection_statement, "statement --> selection_statement" );
  _gr->AddLhsSymbol( statement__selection_statement, statement );
  _gr->AddRhsSymbol( statement__selection_statement, selection_statement );

  _gr->AddRule( statement__iteration_statement, "statement --> iteration_statement" );
  _gr->AddLhsSymbol( statement__iteration_statement, statement );
  _gr->AddRhsSymbol( statement__iteration_statement, iteration_statement );

  _gr->AddRule( statement__jump_statement, "statement --> jump_statement" );
  _gr->AddLhsSymbol( statement__jump_statement, statement );
  _gr->AddRhsSymbol( statement__jump_statement, jump_statement );
  
  // labeled_statement
  // : identifier ':' statement
  // | CASE constant_expr ':' statement
  // | DEFAULT ':' statement
  
  _gr->AddRule( labeled_statement__identifier_COLON_statement, "labeled_statement --> identifier : statement" );
  _gr->AddLhsSymbol( labeled_statement__identifier_COLON_statement, labeled_statement );
  _gr->AddRhsSymbol( labeled_statement__identifier_COLON_statement, identifier );
  _gr->AddRhsSymbol( labeled_statement__identifier_COLON_statement, COLON );
  _gr->AddRhsSymbol( labeled_statement__identifier_COLON_statement, statement );
  
  _gr->AddRule( labeled_statement__CASE_constant_expr_COLON_statement, "labeled_statement --> case constant_expr : statement" );
  _gr->AddLhsSymbol( labeled_statement__CASE_constant_expr_COLON_statement, labeled_statement );
  _gr->AddRhsSymbol( labeled_statement__CASE_constant_expr_COLON_statement, CASE );
  _gr->AddRhsSymbol( labeled_statement__CASE_constant_expr_COLON_statement, constant_expr );
  _gr->AddRhsSymbol( labeled_statement__CASE_constant_expr_COLON_statement, COLON );
  _gr->AddRhsSymbol( labeled_statement__CASE_constant_expr_COLON_statement, statement );
  
  _gr->AddRule( labeled_statement__DEFAULT_COLON_statement, "labeled_statement --> default : statement" );
  _gr->AddLhsSymbol( labeled_statement__DEFAULT_COLON_statement, labeled_statement );
  _gr->AddRhsSymbol( labeled_statement__DEFAULT_COLON_statement, DEFAULT );
  _gr->AddRhsSymbol( labeled_statement__DEFAULT_COLON_statement, COLON );
  _gr->AddRhsSymbol( labeled_statement__DEFAULT_COLON_statement, statement );
  
  // compound_statement
  // : '{' '}'
  // | '{' statement_list '}'
  // | '{' declaration_list '}'
  // | '{' declaration_list statement_list '}'
  
  _gr->AddRule( compound_statement__LEFT_CL_BRACKET_RIGHT_CL_BRACKET, "compound_statement --> [ ]" );
  _gr->AddLhsSymbol( compound_statement__LEFT_CL_BRACKET_RIGHT_CL_BRACKET, compound_statement );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_RIGHT_CL_BRACKET, LEFT_CL_BRACKET );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_RIGHT_CL_BRACKET, RIGHT_CL_BRACKET );

  _gr->AddRule( compound_statement__LEFT_CL_BRACKET_statement_list_RIGHT_CL_BRACKET, "compound_statement --> [ statement_list ]" );
  _gr->AddLhsSymbol( compound_statement__LEFT_CL_BRACKET_statement_list_RIGHT_CL_BRACKET, compound_statement );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_statement_list_RIGHT_CL_BRACKET, LEFT_CL_BRACKET );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_statement_list_RIGHT_CL_BRACKET, statement_list );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_statement_list_RIGHT_CL_BRACKET, RIGHT_CL_BRACKET );

  _gr->AddRule( compound_statement__LEFT_CL_BRACKET_declaration_list_RIGHT_CL_BRACKET, "compound_statement --> [ declaration_list ]" );
  _gr->AddLhsSymbol( compound_statement__LEFT_CL_BRACKET_declaration_list_RIGHT_CL_BRACKET, compound_statement );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_declaration_list_RIGHT_CL_BRACKET, LEFT_CL_BRACKET );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_declaration_list_RIGHT_CL_BRACKET, declaration_list );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_declaration_list_RIGHT_CL_BRACKET, RIGHT_CL_BRACKET );

  _gr->AddRule( compound_statement__LEFT_CL_BRACKET_declaration_list_statement_list_RIGHT_CL_BRACKET, "compound_statement --> [ declaration_list statement_list ]" );
  _gr->AddLhsSymbol( compound_statement__LEFT_CL_BRACKET_declaration_list_statement_list_RIGHT_CL_BRACKET, compound_statement );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_declaration_list_statement_list_RIGHT_CL_BRACKET, LEFT_CL_BRACKET );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_declaration_list_statement_list_RIGHT_CL_BRACKET, declaration_list );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_declaration_list_statement_list_RIGHT_CL_BRACKET, statement_list );
  _gr->AddRhsSymbol( compound_statement__LEFT_CL_BRACKET_declaration_list_statement_list_RIGHT_CL_BRACKET, RIGHT_CL_BRACKET );
  
  // declaration_list
  // : declaration
  // | declaration_list declaration
  
  _gr->AddRule( declaration_list__declaration, "declaration_list --> declaration" );
  _gr->AddLhsSymbol( declaration_list__declaration, declaration_list );
  _gr->AddRhsSymbol( declaration_list__declaration, declaration );

  _gr->AddRule( declaration_list__declaration_list_declaration, "declaration_list --> declaration_list declaration" );
  _gr->AddLhsSymbol( declaration_list__declaration_list_declaration, declaration_list );
  _gr->AddRhsSymbol( declaration_list__declaration_list_declaration, declaration_list );
  _gr->AddRhsSymbol( declaration_list__declaration_list_declaration, declaration );
  
  // statement_list
  // : statement
  // | statement_list statement
  
  _gr->AddRule( statement_list__statement, "statement_list --> statement" );
  _gr->AddLhsSymbol( statement_list__statement, statement_list );
  _gr->AddRhsSymbol( statement_list__statement, statement );

  _gr->AddRule( statement_list__statement_list_statement, "statement_list --> statement_list statement" );
  _gr->AddLhsSymbol( statement_list__statement_list_statement, statement_list );
  _gr->AddRhsSymbol( statement_list__statement_list_statement, statement_list );
  _gr->AddRhsSymbol( statement_list__statement_list_statement, statement );
  
  // expression_statement
  // : ';'
  // | expr ';'
  
  _gr->AddRule( expression_statement__SEMICOLON, "expression_statement --> ;" );
  _gr->AddLhsSymbol( expression_statement__SEMICOLON, expression_statement );
  _gr->AddRhsSymbol( expression_statement__SEMICOLON, SEMICOLON );
  
  _gr->AddRule( expression_statement__expr_SEMICOLON, "expression_statement --> expr ;" );
  _gr->AddLhsSymbol( expression_statement__expr_SEMICOLON, expression_statement );
  _gr->AddRhsSymbol( expression_statement__expr_SEMICOLON, expr );
  _gr->AddRhsSymbol( expression_statement__expr_SEMICOLON, SEMICOLON );
  
  // selection_statement
  // : IF '(' expr ')' statement
  // | IF '(' expr ')' statement ELSE statement
  // | SWITCH '(' expr ')' statement
  
  _gr->AddRule( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement, "selection_statement --> if ( expr ) statement" );
  _gr->AddLhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement, selection_statement );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement, IF );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement, statement );

  _gr->AddRule( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement_ELSE_statement, "selection_statement --> if ( expr ) statement else statement" );
  _gr->AddLhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement_ELSE_statement, selection_statement );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement_ELSE_statement, IF );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement_ELSE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement_ELSE_statement, expr );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement_ELSE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement_ELSE_statement, statement );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement_ELSE_statement, ELSE );
  _gr->AddRhsSymbol( selection_statement__IF_LEFT_BRACE_expr_RIGHT_BRACE_statement_ELSE_statement, statement );

  _gr->AddRule( selection_statement__SWITCH_LEFT_BRACE_expr_RIGHT_BRACE_statement, "selection_statement --> switch ( expr ) statement" );
  _gr->AddLhsSymbol( selection_statement__SWITCH_LEFT_BRACE_expr_RIGHT_BRACE_statement, selection_statement );
  _gr->AddRhsSymbol( selection_statement__SWITCH_LEFT_BRACE_expr_RIGHT_BRACE_statement, SWITCH );
  _gr->AddRhsSymbol( selection_statement__SWITCH_LEFT_BRACE_expr_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( selection_statement__SWITCH_LEFT_BRACE_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( selection_statement__SWITCH_LEFT_BRACE_expr_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( selection_statement__SWITCH_LEFT_BRACE_expr_RIGHT_BRACE_statement, statement );
  
  // iteration_statement
  // : WHILE '(' expr ')' statement
  // | DO statement WHILE '(' expr ')' ';'
  // | FOR '(' ';' ';' ')' statement
  // | FOR '(' ';' ';' expr ')' statement
  // | FOR '(' ';' expr ';' ')' statement
  // | FOR '(' ';' expr ';' expr ')' statement
  // | FOR '(' expr ';' ';' ')' statement
  // | FOR '(' expr ';' ';' expr ')' statement
  // | FOR '(' expr ';' expr ';' ')' statement
  // | FOR '(' expr ';' expr ';' expr ')' statement
  
  _gr->AddRule( iteration_statement__WHILE_LEFT_BRACE_expr_RIGHT_BRACE_statement, "iteration_statement --> while ( expr ) statement" );
  _gr->AddLhsSymbol( iteration_statement__WHILE_LEFT_BRACE_expr_RIGHT_BRACE_statement, iteration_statement );
  _gr->AddRhsSymbol( iteration_statement__WHILE_LEFT_BRACE_expr_RIGHT_BRACE_statement, WHILE );
  _gr->AddRhsSymbol( iteration_statement__WHILE_LEFT_BRACE_expr_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__WHILE_LEFT_BRACE_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__WHILE_LEFT_BRACE_expr_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__WHILE_LEFT_BRACE_expr_RIGHT_BRACE_statement, statement );

  _gr->AddRule( iteration_statement__DO_statement_WHILE_LEFT_BRACE_expr_RIGHT_BRACE, "iteration_statement --> do statement while ( expr )" );
  _gr->AddLhsSymbol( iteration_statement__DO_statement_WHILE_LEFT_BRACE_expr_RIGHT_BRACE, iteration_statement );
  _gr->AddRhsSymbol( iteration_statement__DO_statement_WHILE_LEFT_BRACE_expr_RIGHT_BRACE, DO );
  _gr->AddRhsSymbol( iteration_statement__DO_statement_WHILE_LEFT_BRACE_expr_RIGHT_BRACE, statement );
  _gr->AddRhsSymbol( iteration_statement__DO_statement_WHILE_LEFT_BRACE_expr_RIGHT_BRACE, WHILE );
  _gr->AddRhsSymbol( iteration_statement__DO_statement_WHILE_LEFT_BRACE_expr_RIGHT_BRACE, LEFT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__DO_statement_WHILE_LEFT_BRACE_expr_RIGHT_BRACE, expr );
  _gr->AddRhsSymbol( iteration_statement__DO_statement_WHILE_LEFT_BRACE_expr_RIGHT_BRACE, RIGHT_BRACE );

  _gr->AddRule( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, "iteration_statement --> for ( ; ; ) statement" );
  _gr->AddLhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, iteration_statement );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, FOR );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, statement );

  _gr->AddRule( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, "iteration_statement --> for ( ; ; expr ) statement" );
  _gr->AddLhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, iteration_statement );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, FOR );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, statement );

  _gr->AddRule( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, "iteration_statement --> for ( ; expr ; ) statement" );
  _gr->AddLhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, iteration_statement );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, FOR );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, statement );

  _gr->AddRule( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, "iteration_statement --> for ( ; expr ; expr ) statement" );
  _gr->AddLhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, iteration_statement );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, FOR );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, statement );

  _gr->AddRule( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, "iteration_statement --> for ( expr ; ; ) statement" );
  _gr->AddLhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, iteration_statement );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, FOR );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_RIGHT_BRACE_statement, statement );

  _gr->AddRule( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, "iteration_statement --> for ( expr ; ; expr ) statement" );
  _gr->AddLhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, iteration_statement );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, FOR );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_SEMICOLON_expr_RIGHT_BRACE_statement, statement );

  _gr->AddRule( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, "iteration_statement --> for ( expr ; expr ; ) statement" );
  _gr->AddLhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, iteration_statement );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, FOR );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_RIGHT_BRACE_statement, statement );

  _gr->AddRule( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, "iteration_statement --> for ( expr ; expr ; expr ) statement" );
  _gr->AddLhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, iteration_statement );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, FOR );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, LEFT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, SEMICOLON );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, expr );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, RIGHT_BRACE );
  _gr->AddRhsSymbol( iteration_statement__FOR_LEFT_BRACE_expr_SEMICOLON_expr_SEMICOLON_expr_RIGHT_BRACE_statement, statement );
  
  // jump_statement
  // : GOTO identifier ';'
  // | CONTINUE ';'
  // | BREAK ';'
  // | RETURN ';'
  // | RETURN expr ';'
  
  _gr->AddRule( jump_statement__GOTO_identifier_SEMICOLON, "jump_statement -->  goto identifier ;" );
  _gr->AddLhsSymbol( jump_statement__GOTO_identifier_SEMICOLON, jump_statement );
  _gr->AddRhsSymbol( jump_statement__GOTO_identifier_SEMICOLON, GOTO );
  _gr->AddRhsSymbol( jump_statement__GOTO_identifier_SEMICOLON, identifier );
  _gr->AddRhsSymbol( jump_statement__GOTO_identifier_SEMICOLON, SEMICOLON );

  _gr->AddRule( jump_statement__CONTINUE_SEMICOLON, "jump_statement --> continue ;" );
  _gr->AddLhsSymbol( jump_statement__CONTINUE_SEMICOLON, jump_statement );
  _gr->AddRhsSymbol( jump_statement__CONTINUE_SEMICOLON, CONTINUE );
  _gr->AddRhsSymbol( jump_statement__CONTINUE_SEMICOLON, SEMICOLON );

  _gr->AddRule( jump_statement__BREAK_SEMICOLON, "jump_statement --> break ;" );
  _gr->AddLhsSymbol( jump_statement__BREAK_SEMICOLON, jump_statement );
  _gr->AddRhsSymbol( jump_statement__BREAK_SEMICOLON, BREAK );
  _gr->AddRhsSymbol( jump_statement__BREAK_SEMICOLON, SEMICOLON );

  _gr->AddRule( jump_statement__RETURN_SEMICOLON, "jump_statement --> return ;" );
  _gr->AddLhsSymbol( jump_statement__RETURN_SEMICOLON, jump_statement );
  _gr->AddRhsSymbol( jump_statement__RETURN_SEMICOLON, RETURN );
  _gr->AddRhsSymbol( jump_statement__RETURN_SEMICOLON, SEMICOLON );

  _gr->AddRule( jump_statement__RETURN_expr_SEMICOLON, "jump_statement --> return expr ;" );
  _gr->AddLhsSymbol( jump_statement__RETURN_expr_SEMICOLON, jump_statement );
  _gr->AddRhsSymbol( jump_statement__RETURN_expr_SEMICOLON, RETURN );
  _gr->AddRhsSymbol( jump_statement__RETURN_expr_SEMICOLON, expr );
  _gr->AddRhsSymbol( jump_statement__RETURN_expr_SEMICOLON, SEMICOLON );
  
  // file
  // : external_definition
  // | file external_definition
  
  _gr->AddRule( file__external_definition, "file --> external_definition" );
  _gr->AddLhsSymbol( file__external_definition, file );
  _gr->AddRhsSymbol( file__external_definition, external_definition );

  _gr->AddRule( file__file_external_definition, "file --> file external_definition" );
  _gr->AddLhsSymbol( file__file_external_definition, file );
  _gr->AddRhsSymbol( file__file_external_definition, file );
  _gr->AddRhsSymbol( file__file_external_definition, external_definition );
  
  // external_definition
  // : function_definition
  // | declaration
  
  _gr->AddRule( external_definition__function_definition, "external_definition --> function_definition" );
  _gr->AddLhsSymbol( external_definition__function_definition, external_definition );
  _gr->AddRhsSymbol( external_definition__function_definition, function_definition );

  _gr->AddRule( external_definition__declaration, "external_definition --> declaration" );
  _gr->AddLhsSymbol( external_definition__declaration, external_definition );
  _gr->AddRhsSymbol( external_definition__declaration, declaration );
  
  // function_definition
  // : declarator function_body
  // | declaration_specifiers declarator function_body
  
  _gr->AddRule( function_definition__declarator_function_body, "function_definition --> declarator function_body" );
  _gr->AddLhsSymbol( function_definition__declarator_function_body, function_definition );
  _gr->AddRhsSymbol( function_definition__declarator_function_body, declarator );
  _gr->AddRhsSymbol( function_definition__declarator_function_body, function_body );

  _gr->AddRule( function_definition__declaration_specifiers_declarator_function_body, "function_definition --> declaration_specifiers declarator function_body" );
  _gr->AddLhsSymbol( function_definition__declaration_specifiers_declarator_function_body, function_definition );
  _gr->AddRhsSymbol( function_definition__declaration_specifiers_declarator_function_body, declaration_specifiers );
  _gr->AddRhsSymbol( function_definition__declaration_specifiers_declarator_function_body, declarator );
  _gr->AddRhsSymbol( function_definition__declaration_specifiers_declarator_function_body, function_body );
  
  // function_body
  // : compound_statement
  // | declaration_list compound_statement
  
  _gr->AddRule( function_body__compound_statement, "function_body --> compound_statement" );
  _gr->AddLhsSymbol( function_body__compound_statement, function_body );
  _gr->AddRhsSymbol( function_body__compound_statement, compound_statement );
  
  _gr->AddRule( function_body__declaration_list_compound_statement, "function_body --> declaration_list compound_statement" );
  _gr->AddLhsSymbol( function_body__declaration_list_compound_statement, function_body );
  _gr->AddRhsSymbol( function_body__declaration_list_compound_statement, declaration_list );
  _gr->AddRhsSymbol( function_body__declaration_list_compound_statement, compound_statement );
  
  // identifier
  // : IDENTIFIER
  
  _gr->AddRule( identifier__IDENTIFIER, "identifier --> IDENTIFIER" );
  _gr->AddLhsSymbol( identifier__IDENTIFIER, identifier );
  _gr->AddRhsSymbol( identifier__IDENTIFIER, IDENTIFIER );
  
  // constant
  // : HEX
  // | OCTAL
  // | INTEGER
  // | REAL
  // | CHARACTER_LITERAL
  
  _gr->AddRule( constant__HEX, "constant --> HEX" );
  _gr->AddLhsSymbol( constant__HEX, constant );
  _gr->AddRhsSymbol( constant__HEX, HEX );

  _gr->AddRule( constant__OCTAL, "constant --> OCTAL" );
  _gr->AddLhsSymbol( constant__OCTAL, constant );
  _gr->AddRhsSymbol( constant__OCTAL, OCTAL );

  _gr->AddRule( constant__INTEGER, "constant --> INTEGER" );
  _gr->AddLhsSymbol( constant__INTEGER, constant );
  _gr->AddRhsSymbol( constant__INTEGER, INTEGER );

  _gr->AddRule( constant__REAL, "constant --> REAL" );
  _gr->AddLhsSymbol( constant__REAL, constant );
  _gr->AddRhsSymbol( constant__REAL, REAL );

  _gr->AddRule( constant__CHARACTER_LITERAL, "constant --> CHARACTER_LITERAL" );
  _gr->AddLhsSymbol( constant__CHARACTER_LITERAL, constant );
  _gr->AddRhsSymbol( constant__CHARACTER_LITERAL, CHARACTER_LITERAL );
}




