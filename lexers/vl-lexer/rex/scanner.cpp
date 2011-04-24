
#include <rex/scanner.h>
using rexp::Scanner;

#include <stdexcept>
#include <sstream>
#include <set>

// возвращает лексему из потока ввода
Scanner::Token::Ptr Scanner::GetToken() {
  if (tok_pos_ < tok_list_.size()) {
    return tok_list_[tok_pos_++];
  }

  if (IsEnd()) {
    return Cached(new Token(END));
  }

  char cur = Next();
  switch (cur) {
    case ALTER:       return Cached(new Token(ALTER));
    case STAR:        return Cached(new Token(STAR));
    case FOR_SLASH:   return Cached(new Token(FOR_SLASH));
    case QM:          return Cached(new Token(QM));
    case PLUS:        return Cached(new Token(PLUS));
    case LEFT_PAR:    return Cached(new Token(LEFT_PAR));
    case RIGHT_PAR:   return Cached(new Token(RIGHT_PAR));

    // формируем выражение "точка" - любой символ за исключением '\n'
    case '.': return FormDotExpr();

    // выражение в квадратных скобках
    case '[':  return ParseBracketsExpr();

    // выражение в фигурных скобках
    case '{':  return ParseBracesExpr();

    // выражение в двойных кавычках
    case '"':  return ParseDblApostrExpr();

    // специальные символы
    case '\\':
      if (IsEnd()) {
        throw std::invalid_argument("Слэш не может быть в конце регулярного выражения");
      }
      cur = Next();
      if (cur == 's' or cur == 'S' or cur == 'w' or cur == 'W' or cur == 'd' or cur == 'D') {
        return FormEscapeExpr(cur);
      }
      break;

    // эти символы не могут появляться в данном контексте
    case '}': {
      std::stringstream st;
      st << "Символ } не может быть в данном регулярном выражении на позиции " << pos_;
      throw std::invalid_argument(st.str());
    }
    case ']': {
      std::stringstream st;
      st << "Символ ] не может быть в данном регулярном выражении на позиции " << pos_;
      throw std::invalid_argument(st.str());
    }
  }

  // любой символ не обработанный выше
  return Cached(new Token(INC_SYMBOLS, std::string(&cur, 1)));
}

// обрабатывает выражение вида [ ... ]
Scanner::Token::Ptr Scanner::ParseBracketsExpr() {
  // читаем символ из потока
  char cur = Next();
  unsigned br_pos = pos_;

  if (IsEnd()) {
    std::stringstream st;
    st << "Ошибка на позиции " << pos_ << ". Регулярное выражение содержит незавершенную квадратную скобку";
    throw std::invalid_argument(st.str().c_str());
  }

  // символы в выражении будут исключаться
  bool is_exclude = false;
  if (cur == '^') {
    is_exclude = true;
    if (IsEnd()) {
      std::stringstream st;
      st << "Ошибка на позиции " << pos_ << ". Регулярное выражение содержит незавершенную квадратную скобку";
      throw std::invalid_argument(st.str().c_str());
    }
    cur = Next();
    br_pos = pos_;
  }

  // если символ это двоеточие, то может быть одно их следующих сокращений
  // [:alnum:] - '_' U {A-Z} U {a-z} U {0-9}
  // [:alpha:] - '_' U {A-Z} U {a-z}
  // [:blank:] - ' ' U '\n' U '\v' U '\r\ U '\t'
  // [:cntrl:] -  все, меньшие 32 = 0x20
  // [:digit:] -  {0-9}
  // [:lower:] -  {a-z}
  // [:print:] -  все, большие 32 = 0x20
  // [:punct:] -  знаки пунктуации: '.' ',' ';' ':' '!' '\?'
  // [:space:] -  пробел ' '
  // [:upper:] -  {A-Z}
  // [:xdigit:] -  {A-F} U {0-9}
  else if (cur == ':') {
    std::string keyword;
    for (cur = Next(); cur != ':'; cur = Next()) {
      // Конец потока.
      if (IsEnd()) {
        std::stringstream st;
        st << "Ошибка на позиции " << pos_ << ". Регулярное выражение содержит незавершенную квадратную скобку";
        throw std::invalid_argument(st.str().c_str());
      }
      if (cur == ']') {
        goto MAIN_LOOP;
      }
      keyword += cur;
    }

    // читаем следующий символ. Он должен быть ']'
    if ((cur = Next()) != ']') {
      std::stringstream st;
      st << "Ошибка на позиции " << pos_ << ". Регулярное выражение вида [:" << keyword << ":] не содержит символа ']'";
      throw std::invalid_argument(st.str());
    }

    if (keyword == "alnum") {
      std::string res;
      for (unsigned tmp = '0'; tmp <= '9'; ++tmp) res += tmp;
      for (unsigned tmp = 'a'; tmp <= 'z'; ++tmp) res += tmp;
      for (unsigned tmp = 'A'; tmp <= 'Z'; ++tmp) res += tmp;
      res += '_';
      return Cached(new Token(INC_SYMBOLS, res));
    } else if (keyword == "alpha") {
      std::string res;
      for (unsigned tmp = 'a'; tmp <= 'z'; ++tmp) res += tmp;
      for (unsigned tmp = 'A'; tmp <= 'Z'; ++tmp) res += tmp;
      res += '_';
      return Cached(new Token(INC_SYMBOLS, res));
    } else if (keyword == "blank") {
      std::string res;
      res += ' ';
      res += '\n';
      res += '\r';
      res += '\t';
      res += '\v';
      return Cached(new Token(INC_SYMBOLS, res));
    } else if (keyword == "cntrl") {
      std::string res;
      for (unsigned tmp = 1; tmp < 32; ++tmp) res += tmp;
      return Cached(new Token(INC_SYMBOLS, res));
    } else if (keyword == "digit") {
      std::string res;
      for (unsigned tmp = '0'; tmp <= '9'; ++tmp) res += tmp;
      return Cached(new Token(INC_SYMBOLS, res));
    } else if (keyword == "lower") {
      std::string res;
      for (unsigned tmp = 'a'; tmp <= 'z'; ++tmp) res += tmp;
      return Cached(new Token(INC_SYMBOLS, res));
    } else if (keyword == "print") {
      std::string res;
      for (unsigned tmp = 32; tmp < SYM_QUENT; ++tmp) res += char(tmp);
      return Cached(new Token(INC_SYMBOLS, res));
    } else if (keyword == "punct") {
      std::string res;
      res += '.';
      res += ',';
      res += ';';
      res += ':';
      res += '!';
      res += '\?';
      return Cached(new Token(INC_SYMBOLS, res));
    } else if (keyword == "space") {
      std::string res;
      res += ' ';
      return Cached(new Token(INC_SYMBOLS, res));
    } else if (keyword =="upper") {
      std::string res;
      for (unsigned tmp = 'A'; tmp <= 'Z'; ++tmp) res += tmp;
      return Cached(new Token(INC_SYMBOLS, res));
    } else if (keyword == "xdigit") {
      std::string res;
      for (unsigned tmp = 'A'; tmp <= 'F'; ++tmp) res += tmp;
      for (unsigned tmp = 'a'; tmp <= 'f'; ++tmp) res += tmp;
      for (unsigned tmp = '0'; tmp <= '9'; ++tmp) res += tmp;
      return Cached(new Token(INC_SYMBOLS, res));
    }
  }

MAIN_LOOP:

  // Устанавливаем позицию на начало скобок.
  pos_ = br_pos;

  // читаем содержимое квадратных скобок
  std::set<char> st;
  for (; cur != ']'; cur = Next()) {
    // дошли до конца потока, это ошибка. Имеем: [... EOF
    if (IsEnd()) {
      std::stringstream st;
      st << "Ошибка на позиции " << pos_ << ". Регулярное выражение содержит незавершенную квадратную скобку";
      throw std::invalid_argument(st.str().c_str());
    }

    // символ '-' быть в этом контексте не может. Имеем [... --
    if (cur == '-') {
      std::stringstream st;
      st << "Ошибка на позиции " << pos_ << ". Символ '-' не может быть в данном регулярном выражении";
      throw std::invalid_argument(st.str());
    }

    // читаем следующий символ...
    char next = Next();

    // в данном контексте символ '-' значит последовательность символов
    if (next == '-') {
      // читаем следующий символ. Он не должен быть ']' или '-'
      next = Next();
      if (next == ']' or next == '-') {
        std::stringstream st;
        st << "В данном регулярном выражении на позиции " << pos_ << " должен быть символ '-' или ']'";
        throw std::invalid_argument(st.str());
      }

      // дошли до конца потока, это ошибка. Имеем: [... EOF
      if (IsEnd()) {
        std::stringstream st;
        st << "Ошибка на позиции " << pos_ << ". Регулярное выражение содержит незавершенную квадратную скобку";
        throw std::invalid_argument(st.str().c_str());
      }

      // правый символ не может быть меньше левого в диапазоне
      if (next < cur) {
        std::stringstream st;
        st << "Ошибка на позиции " << pos_
          << ". В регулярном выражении вида [left_symbol-rigth_symbol] left_symbol не может быть больше чем rigth_symbol";
        throw std::invalid_argument(st.str().c_str());
      }

      // добавляем в str все символы между cur_ и next_
      for (char tmp = cur; tmp <= next; ++tmp) {
        st.insert(tmp);
      }
    } else {
      st.insert(cur);
      StreamBack();
    }
  }

  std::string res_str;
  if (is_exclude) {
    for (unsigned cnt = 1; cnt < SYM_QUENT; ++cnt) {
      if (st.find(cnt) == st.end()) {
        res_str += char(cnt);
      }
    }
  } else {
    for (std::set<char>::const_iterator it = st.begin(); it != st.end(); ++it) {
      res_str += *it;
    }
  }

  return Cached(new Token(INC_SYMBOLS, res_str));
}

// обрабатываем выражение двойные кавычки
Scanner::Token::Ptr Scanner::ParseDblApostrExpr() {
  std::set<char> char_set;
  for (char cur = Next(); cur != '"'; cur = Next()) {
    if (IsEnd()) {
      std::stringstream st;
      st << "Ошибка на позиции " << pos_ << ". Регулярное выражение содержит незавершенный двойной апостроф";
      throw std::invalid_argument(st.str().c_str());
    }
    char_set.insert(cur);
  }

  std::string res_str;
  for (std::set<char>::const_iterator it = char_set.begin(); it != char_set.end(); ++it) {
    res_str += *it;
  }
  return Cached(new Token(INC_SYMBOLS, res_str));
}

// обрабатываем выражение в фигурных скобках
Scanner::Token::Ptr Scanner::ParseBracesExpr() {
  // читаем первый символ
  char cur = Next();

  // это должна быть цифра
  if (not isdigit(cur)) {
    std::stringstream st;
    st << "В данном регулярном выражении на позиции " << pos_ << " должна быть цифра";
    throw std::invalid_argument(st.str());
  }

  // читаем последовательность цифр
  unsigned first = 0;
  for (; isdigit(cur); cur = Next()) {
    first = first * 10 + cur - '0';
  }

  // должно быть выражение вида {n,...} или {n}
  if (cur != ',' and cur != '}') {
    std::stringstream st;
    st << "В данном регулярном выражении на позиции " << pos_ << " должны быть символы ',' или '}'";
    throw std::invalid_argument(st.str());
  }

  // если следующий символ это фигурная скобка т.е. имеем {n}, выходим
  if (cur == '}') {
    return Token::Ptr(new Token(first));
  }

  // читаем следующий символ, это может быть либо '}' либо цифра в случае {n,m}
  cur = Next();

  // если следующий символ это '}' т.е. мы имеем {n,} выходим
  if (cur == '}') {
    return Cached(new Token(first, true));
  }

  // читаем последовательность цифр
  unsigned second = 0;
  for (; isdigit(cur); cur = Next()) {
    second = second * 10 + cur - '0';
  }

  // сейчас в любом случае должна быть '}'
  if (cur != '}') {
    std::stringstream st;
    st << "В данном регулярном выражении на позиции " << pos_ << " должен быть символ '}'";
    throw std::invalid_argument(st.str());
  }
  return Cached(new Token(first, second));
}

// обработка специальных символов
Scanner::Token::Ptr Scanner::FormEscapeExpr(char cur) {
  std::string str;
  switch (cur) {
    case 's':
    str += ' ';
    return Cached(new Token(INC_SYMBOLS, str));

    case 'S':
    str += ' ';
    {
      std::string ex_str;
      for (unsigned cnt = 1; cnt < SYM_QUENT; ++cnt) {
        if (str.find(cnt) == std::string::npos) {
          ex_str += cnt;
        }
      }
      return Cached(new Token(INC_SYMBOLS, ex_str));
    }

    case 'w':
    str += '\n';
    str += '\r';
    str += '\t';
    str += '\v';
    return Cached(new Token(INC_SYMBOLS, str));

    case 'W':
    str += '\n';
    str += '\r';
    str += '\t';
    str += '\v';
    {
      std::string ex_str;
      for (unsigned cnt = 1; cnt < SYM_QUENT; ++cnt) {
        if (str.find(cnt) == std::string::npos) {
          ex_str += cnt;
        }
      }
      return Cached(new Token(INC_SYMBOLS, ex_str));
    }

    case 'd':
      for (char cnt = '0'; cnt <= '9'; ++cnt) {
        str += cnt;
      }
      return Cached(new Token(INC_SYMBOLS, str));

    case 'D':
    {
      for (char cnt = '0'; cnt <= '9'; ++cnt) {
        str += cnt;
      }
      std::string ex_str;
      for (unsigned cnt1 = 1; cnt1 < SYM_QUENT; ++cnt1) {
        if (str.find(cnt1) == std::string::npos) {
          ex_str += char(cnt1);
        }
      }
      return Cached(new Token(INC_SYMBOLS, ex_str));
    }
  }

  return Cached(new Token(INC_SYMBOLS, str));
}

// выражение "точка" обозначает любой символ кроме символа конца строки
Scanner::Token::Ptr Scanner::FormDotExpr() {
  std::string sym;
  for (unsigned cnt = 1; cnt < SYM_QUENT; ++cnt) {
    if (cnt != '\n') {
      sym += cnt;
    }
  }
  return Cached(new Token(INC_SYMBOLS, sym));
}

