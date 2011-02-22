

#ifndef LEXER_H__
#define LEXER_H__

#include <vector>
#include "token.h"

namespace parser {

struct Lexer {
  //! Тип списка токенов.
  typedef std::vector<Token::Ptr> TokenList;

  //! Возврат списка токенов, следующих за переданным в качестве параметра.
  virtual TokenList GetTokens(Token::Ptr token) = 0;

  //! Возвращает true, если достигнут конец потока.
  virtual bool IsEnd() = 0;

  //! Тип абстрактный.
  virtual ~Lexer() {
  }
};

} // parser

#endif // LEXER_H__

