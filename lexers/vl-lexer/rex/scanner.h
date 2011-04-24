
#pragma once

#include <boost/shared_ptr.hpp>
#include <string>
#include <deque>

namespace rexp {

/*!
 * \brief Класс-лексический анализатор для парсер регулярных выражений.
 */
struct Scanner {
  // возвращаемые типы лексем
  enum TokenType {
    ALTER = '|',
    STAR = '*',
    FOR_SLASH = '/',
    QM = '\?',
    LEFT_PAR = '(',
    RIGHT_PAR = ')',
    PLUS = '+',
    BRACES_EXPR = 1, // { n, } n или больше символов
    BRACES1_EXPR,    // { n } ровно n символов
    BRACES2_EXPR,    // { n, m } от n до m символов
    INC_SYMBOLS,     // некоторое множество символов
    END              // конец потока
  };

  // количество символов
  enum {
    SYM_QUENT = 1 << sizeof(char) * 8
  };

  //! Данные, возвращаемые сканером.
  class Token {
    //! Тип лексемы.
    TokenType type_;

    //! Буфер для значения лексемы.
    std::string buf_;

    //! Здесь хранится n если тип лексемы есть BRACES_EXPR, BRACES1_EXPR или BRACES2_EXPR.
    unsigned first_num_;

    //! Здесь хранится m если тип лексемы есть BRACES_EXPR, BRACES1_EXPR или BRACES2_EXPR.
    unsigned second_num_;

  public:
    //! Конструктор только с типом, без данных.
    Token(TokenType type)
      : type_(type) {
    }

    //! Конструктор со строковыми данными.
    Token(TokenType type, const std::string& str)
      : type_(type)
      , buf_(str) {
    }

    //! конструктор с типом BRACES1_EXPR.
    Token(unsigned num)
      : type_(BRACES1_EXPR)
      , first_num_(num) {
    }

    //! конструктор с типом BRACES2_EXPR.
    Token(unsigned fnum, unsigned snum)
      : type_(BRACES2_EXPR)
      , first_num_(fnum)
      , second_num_(snum) {
    }

    //! Конструктор с типом BRACES_EXPR.
    Token(unsigned num, bool, bool)
      : type_(BRACES_EXPR)
      , first_num_(num) {
    }

    //! Возвращает тип лексемы.
    TokenType Type() const {
      return type_;
    }

    //! Возвращает данные лексемы.
    std::string Data() const {
      return buf_;
    }

    //! Возвращает n если тип лексемы есть BRACES_EXPR, BRACES1_EXPR или BRACES2_EXPR.
    unsigned First() const {
      return first_num_;
    }

    //! Возвращает m если тип лексемы есть BRACES_EXPR, BRACES1_EXPR или BRACES2_EXPR.
    unsigned Second() const {
      return second_num_;
    }

    //! Умный указатель на токен.
    typedef boost::shared_ptr<Token> Ptr;
  };

private:
  //! Начало буфера.
  const char* begin_;

  //! Конец буфера.
  const char* end_;

  //! Позиция в потоке.
  unsigned pos_;

  //! Список распознанных токенов.
  typedef std::deque<Token::Ptr> TokenList;
  TokenList tok_list_;

  //! Позиция в списке токенов.
  unsigned tok_pos_;

public:
  /*!
   * \brief Получает указатель на конец и начало потока.
   *
   * \param begin Указатель на начало потока.
   * \param end   Указателт на конец потока.
   */
  Scanner(const char* begin, const char* end)
    : begin_(begin)
    , end_(end)
    , pos_(0)
    , tok_pos_(0) {
  }

  //! Возвращает лексему из потока ввода.
  Token::Ptr GetToken();

  //! Возврат к предыдущему токену.
  void Back() {
    if (tok_pos_) {
      --tok_pos_;
    }
  }

private:
  Token::Ptr ParseBracketsExpr();
  Token::Ptr ParseDblApostrExpr();
  Token::Ptr ParseBracesExpr();
  Token::Ptr FormEscapeExpr(char);
  Token::Ptr FormDotExpr();

  //! Сохранение в кэше.
  Token::Ptr Cached(Token* ptr) {
    Token::Ptr tok_ptr(ptr);
    tok_list_.push_back(tok_ptr);
    return tok_ptr;
  }

  //! Возврат на позицию назад.
  void StreamBack() {
    if (pos_) {
      --pos_;
    }
  }

  //! Достигнут ли конец потока?
  bool IsEnd() const {
    return (begin_ + pos_) == end_;
  }

  //! Получение следующиего символа из потока.
  char Next() {
    if (not IsEnd()) {
      return begin_[pos_++];
    }
    return '\0';
  }
};

}  // namespace rexp

