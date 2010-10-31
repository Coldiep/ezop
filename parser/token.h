

#ifndef TOKEN_H__
#define TOKEN_H__

#include <iostream>

namespace parser{

struct token{
  int          type_;            // the token type
  int          line_pos_;          // the line number
  int          col_pos_;          // column number
  
  std::string      str_value_;          // the value
  
  token( int _type = 0 )
  :
  type_(_type)
  {}
  
  token( int _type, int _col_pos, int _line_pos, const std::string& _value )
  :
  type_(_type),
  line_pos_(_line_pos),
  col_pos_(_col_pos),
  str_value_(_value)
  {}
  
  operator int(){return type_;}
  
  // print token to the ostream
  void            print( std::ostream& _str );

};

} // namespace parser


#endif // TOKEN_H__
