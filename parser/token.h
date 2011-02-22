

#ifndef TOKEN_H__
#define TOKEN_H__

#include <boost/shared_ptr.hpp>
#include "grammar.h"

namespace parser {

//! Определение класса токена.
struct Token {
  //! Тип умного указателя на объект класса.
  typedef boost::shared_ptr<Token> Ptr;

  Grammar::SymbolId type_;      //!< Символ грамматики, связанный с данным токеном (лексический тип).
  unsigned          line_pos_;  //!< Номер строки.
  unsigned          col_pos_;   //!< Номер позиции в строке.
  unsigned          abs_pos_;   //!< Абсолютная позиция токена в исходном коде.
  unsigned          length_;    //!< Длина токена в символах.
  std::string       text_;      //!< Текст токена.

  /*!
   * \brief Конструктор для инициализации всех полей класса.
   */
  Token(const Grammar::SymbolId& type, const unsigned& col_pos, const unsigned& line_pos, const unsigned& abs_pos, const unsigned& length, const std::string& text)
    : type_(type)
    , line_pos_(line_pos)
    , col_pos_(col_pos)
    , abs_pos_(abs_pos)
    , length_(length)
    , text_(text) {
  }

  //! Инициализация по умолчанию -- пустой токен.
  Token()
    : type_(Grammar::kBadSymbolId)
    , line_pos_(0)
    , col_pos_(0)
    , abs_pos_(0)
    , length_(0) {
  }
};

} // namespace parser

#endif // TOKEN_H__
