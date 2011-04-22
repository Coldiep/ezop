
#pragma once

namespace rexp {

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
    INC_SYMBOLS,  // некоторое множество символов
    END              // конец потока
  };

  // количество символов
  enum {
    SYM_QUENT = 1 << sizeof(char) * 8
  };

  // данные, возвращаемые сканером
  class Token {
    // тип лексемы
    TokenType type_;

    // буфер, где лежит значение лексемы
    std::string buf_;

    // здесь хранится n если тип лексемы есть BRACES_EXPR, BRACES1_EXPR или BRACES2_EXPR
    unsigned first_num_;

    // здесь хранится m если тип лексемы есть BRACES_EXPR, BRACES1_EXPR или BRACES2_EXPR
    unsigned second_num_;

  public:
    // конструктор только с типом, без данных
    Token(TokenType type)
      : type_(type) {
    }

    // конструктор со строковыми данными
    Token(TokenType type, const std::string& str)
      : type_(type)
      , buf_(str) {
    }

    // конструктор с типом BRACES1_EXPR type
    Token(unsigned num)
      : type_(BRACES1_EXPR)
      , first_num_(num) {
    }

    // конструктор с типом BRACES2_EXPR type
    Token(unsigned fnum, unsigned snum)
      : type_(BRACES2_EXPR)
      , first_num_(fnum)
      , second_num_(snum) {
    }

    // конструктор с типом BRACES_EXPR type
    Token(unsigned num, bool, bool)
      : type_(BRACES_EXPR)
      , first_num_(num) {
    }

    // возвращает тип лексемы
    TokenType Type() const {
      return type_;
    }

    // возвращает данные лексемы
    std::string Data() const {
      return buf_;
    }

    // возвращает n если тип лексемы есть BRACES_EXPR, BRACES1_EXPR или BRACES2_EXPR
    unsigned First() const {
      return first_num_;
    }

    // возвращает m если тип лексемы есть BRACES_EXPR, BRACES1_EXPR или BRACES2_EXPR
    unsigned Second() const {
      return second_num_;
    }
  };

private:
  // поток для чтения
  stream stm_;

  // позиция в потоке
  unsigned pos_;

  // вектор возвращенных токенов
  typedef std::vector<Token> TokenList;
  TokenList token_buf_;

  // позиция в векторе возвращенных токенов
  unsigned tok_pos_;

public:
  // конструктор берет объект потока в качестве параметра
  Scanner(const stream& strm)
    : stm_(strm)
    , pos_(stm_.get_pos())
    , tok_pos_(0) {
  }

  // переход на позицию назад
  void Back() {
    if (tok_pos_ > 0) {
      --tok_pos_;
    }
  }

  // возвращает лексему из потока ввода
  Token GetToken();

private:
  Token ParseBracketsExpr();
  Token ParseDblApostrExpr();
  Token ParseBracesExpr();
  Token FormEscapeExpr( char_type_t  );
  Token FormDotExpr();

  // утилиты...
  std::string GetStr(const char* str);

  // кладем токен в кеш перед возвращением
  Token put2cache( Token tok )
  {
    Token_buf_.push_back( tok );
    tok_pos_ = Token_buf_.size();
    return tok;
  }
};

}

#endif // Scanner_H__
