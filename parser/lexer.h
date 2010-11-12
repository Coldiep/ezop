

#ifndef LEXER_H__
#define LEXER_H__

#include <vector>
#include "token.h"

namespace parser {

struct lexer{

  // return the current token
  virtual token        get_token() = 0;
  
  // is th end of input?
  virtual bool        is_end() = 0;

  virtual ~lexer(){}
};

} // parser

#endif // LEXER_H__

