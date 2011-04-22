
#include "Scanner.h"
using namespace rexp_;

// возвращает лексему из потока ввода
Scanner::Token Scanner::GetToken() {
  // если мы держим токен в кеше, то берем его оттуда
  if (tok_pos_ > 0 and tok_pos_ < token_buf_.size()) {
    return token_buf_[tok_pos_++];
  }

  // Запоминаем текущую позицию.
  pos_ = stm_.get_pos();

  char cur = stm_.next();

  switch (cur) {
    case ALTER:    return put2cache( Token( ALTER ) );
    case STAR:    return put2cache( Token( STAR ) );
    case FOR_SLASH:  return put2cache( Token( FOR_SLASH ) );
    case QM:      return put2cache( Token( QM ) );
    case PLUS:    return put2cache( Token( PLUS ) );
    case LEFT_PAR:  return put2cache( Token( LEFT_PAR ) );
    case RIGHT_PAR:  return put2cache( Token( RIGHT_PAR ) );

    // формируем выражение "точка" - любой символ за исключением '\n'
    case '.': return FormDotExpr();

    // конец потока
    case stream::eos_en: return put2cache( Token( END ) );

    // выражение в квадратных скобках
    case '[':  return put2cache( ParseBracketsExpr() );

    // выражение в фигурных скобках
    case '{':  return put2cache( ParseBracesExpr() );

    // выражение в двойных кавычках
    case '"':  return put2cache( ParseDblApostrExpr() );

    // специальные символы
    case '\\':
      cur_ = stm_.next();
      if( cur_ == 's' || cur_ == 'S' || cur_ == 'w' || cur_ == 'W' || cur_ == 'd' || cur_ == 'D' )
        return put2cache( FormEscapeExpr( cur_ ) );
      break;

    // эти символы не могут появляться в данном контексте
    case '}':
      {
        std::stringstream st;
        st << "Символ } не может быть в данном регулярном выражении на позиции " << stm_.get_pos();
        throw std::logic_error( st.str() );
      }
    case ']':
      {
        std::stringstream st;
        st << "Символ ] не может быть в данном регулярном выражении на позиции " << stm_.get_pos();
        throw std::logic_error( st.str() );
      }
  }

  // любой символ не обработанный выше
  std::string sym_;
  sym_ += cur_;

  return put2cache( Token( INC_SYMBOLS, sym_ ) );
}

// обрабатывает выражение вида [ ... ]
Scanner::Token Scanner::ParseBracketsExpr() {
  // запоминаем позицию в потоке
  stm_.remember_state();

  // читаем символ из потока
  char_type_t cur = stm_.next();

  // конец потока- ошибка
  if( cur == stream::eos_en ) {
    std::stringstream st;
    st << "Ошибка на позиции " << stm_.get_pos() << ". Регулярное выражение содержит незавершенную квадратную скобку";
    throw std::logic_error( st.str().c_str() );
  }

  // символы в выражении будут исключаться
  bool is_exclude = false;
  if (cur == '^') {
    is_exclude = true;
    cur = stm_.next();
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
    for (cur = stm_.next(); cur != ':'; cur = stm_.next()) {
      // if it is end of stream...
      if (cur == stream::eos_en) {
        std::stringstream st;
        st << "Ошибка на позиции " << stm_.get_pos() << ". Регулярное выражение содержит незавершенную квадратную скобку";
        throw std::logic_error( st.str().c_str() );
      }

      // if it is ']'...
      if (cur == ']') goto MAIN_LOOP;

      keyword += cur;
    }

    // читаем следующий символ. Он должен быть ']'
    if ((cur = stm_.next()) != ']') {
      std::stringstream st;
      st << "Ошибка на позиции " << stm_.get_pos() << ". Регулярное выражение вида [:" << keyword << ":] не содержит символа ']'";
      throw std::logic_error( st.str() );
    }

    // look up the given string
    if (keyword == GetStr("alnum")) {
      std::string res;
      for (unsigned tmp = '0'; tmp <= '9'; ++tmp) res += tmp;
      for (unsigned tmp = 'a'; tmp <= 'z'; ++tmp) res += tmp;
      for (unsigned tmp = 'A'; tmp <= 'Z'; ++tmp) res += tmp;
      res += '_';

      stm_.release_state();
      return Token(INC_SYMBOLS, res);
    } else if (keyword == GetStr("alpha")) {
      std::string res;
      for (unsigned tmp = 'a'; tmp <= 'z'; ++tmp) res += tmp;
      for (unsigned tmp = 'A'; tmp <= 'Z'; ++tmp) res += tmp;
      res += '_';

      stm_.release_state();
      return Token(INC_SYMBOLS, res);
    } else if (keyword == GetStr("blank")) {
      std::string res;
      res += ' ';
      res += '\n';
      res += '\r';
      res += '\t';
      res += '\v';

      stm_.release_state();
      return Token(INC_SYMBOLS, res);
    } else if (keyword == GetStr("cntrl")) {
      std::string res;
      for (unsigned tmp = 1; tmp < 32; ++tmp) res += char(tmp);

      stm_.release_state();
      return Token(INC_SYMBOLS, res);
    } else if (keyword == GetStr("digit")) {
      std::string res;
      for (unsigned tmp = '0'; tmp <= '9'; ++tmp) res += tmp;

      stm_.release_state();
      return Token(INC_SYMBOLS, res);
    } else if (keyword == GetStr("lower")) {
      std::string res;
      for (unsigned tmp = 'a'; tmp <= 'z'; ++tmp) res += tmp;

      stm_.release_state();
      return Token(INC_SYMBOLS, res);
    } else if (keyword == GetStr("print")) {
      std::string res;
      for (unsigned tmp = 32; tmp < SYM_QUENT; ++tmp) res += char(tmp);

      stm_.release_state();
      return Token( INC_SYMBOLS, res );
    } else if (keyword == GetStr("punct")) {
      std::string res;
      res += '.';
      res += ',';
      res += ';';
      res += ':';
      res += '!';
      res += '\?';

      stm_.release_state();
      return Token(INC_SYMBOLS, res);
    } else if (keyword == GetStr("space")) {
      std::string res;
      res += ' ';

      stm_.release_state();
      return Token(INC_SYMBOLS, res);
    } else if (keyword == GetStr("upper")) {
      std::string res;
      for (unsigned tmp = 'A'; tmp <= 'Z'; ++tmp) res += tmp;

      stm_.release_state();
      return Token(INC_SYMBOLS, res);
    } else if (keyword == GetStr("xdigit")) {
      std::string res;
      for (unsigned tmp = 'A'; tmp <= 'F'; ++tmp) res += tmp;
      for (unsigned tmp = 'a'; tmp <= 'f'; ++tmp) res += tmp;
      for (unsigned tmp = '0'; tmp <= '9'; ++tmp) res += tmp;

      stm_.release_state();
      return Token(INC_SYMBOLS, res);
    }
  }

MAIN_LOOP:

  if (not is_exclude) {
    // восстанавливаем позицию
    stm_.restore_state();
    cur = stm_.next();
  } else {
    stm_.release_state();
  }

  // читаем содержимое квадратных скобок
  std::set<char> st;
  for (; cur != ']'; cur = stm_.next()) {
    // дошли до конца потока, это ошибка. Имеем: [... EOF
    if (cur == stream::eos_en) {
      std::stringstream st;
      st << "Ошибка на позиции " << stm_.get_pos() << ". Регулярное выражение содержит незавершенную квадратную скобку";
      throw std::logic_error( st.str().c_str() );
    }

    // символ '-' быть в этом контексте не может. Имеем [... --
    if (cur == '-') {
      std::stringstream st;
      st << "Ошибка на позиции " << stm_.get_pos() << ". Символ '-' не может быть в данном регулярном выражении";
      throw std::logic_error(st.str());
    }

    // читаем следующий символ...
    char next = stm_.next();

    // в данном контексте символ '-' значит последовательность символов
    if (next == '-') {
      // читаем следующий символ. Он не должен быть ']' или '-'
      next = stm_.next();
      if (next == ']' or next == '-') {
        std::stringstream st;
        st << "В данном регулярном выражении на позиции " << stm_.get_pos() << " должен быть символ '-' или ']'";
        throw std::logic_error(st.str());
      }

      // дошли до конца потока, это ошибка. Имеем: [... EOF
      if (next == stream::eos_en) {
        std::stringstream st;
        st << "Ошибка на позиции " << stm_.get_pos() << ". Регулярное выражение содержит незавершенную квадратную скобку";
        throw std::logic_error(st.str().c_str());
      }

      // правый символ не может быть меньше левого в диапазоне
      if (next < cur) {
        std::stringstream st;
        st << "Ошибка на позиции " << stm_.get_pos()
          << ". В регулярном выражении вида [left_symbol-rigth_symbol] left_symbol не может быть больше чем rigth_symbol";
        throw std::logic_error(st.str().c_str());
      }

      // добавляем в str все символы между cur_ и next_
      for (char tmp = cur; tmp <= next; ++tmp) st.insert( tmp );
    } else {
      st.insert(cur);
      stm_.back();
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

  return Token(INC_SYMBOLS, res_str);
}

// обрабатываем выражение двойные кавычки
Scanner::Token Scanner::ParseDblApostrExpr()
{
  set< char_type_t > set_;
  for( char_type_t cur_ = stm_.next(); cur_ != '"'; cur_ = stm_.next() )
  {
    if( cur_ == stream::eos_en )
    {
      std::stringstream st;
      st << "Ошибка на позиции " << stm_.get_pos() << ". Регулярное выражение содержит незавершенный двойной апостроф";
      throw std::logic_error( st.str().c_str() );
    }

    set_.insert( cur_ );
  }

  std::string res_str_;
  for(  set< char_type_t >::const_iterator it_ = set_.begin(); it_ != set_.end(); ++ it_ )
    res_str_ += *it_;

  return Token( INC_SYMBOLS, res_str_ );
}

// обрабатываем выражение в фигурных скобках
Scanner::Token Scanner::ParseBracesExpr()
{
  // читаем первый символ
  char_type_t cur_ = stm_.next();

  // это должна быть цифра
  if( ! isdigit( cur_ ) )
  {
    std::stringstream st;
    st << "В данном регулярном выражении на позиции " << stm_.get_pos() << " должна быть цифра";
    throw std::logic_error( st.str() );
  }

  // читаем последовательность цифр
  unsigned int first_ = 0;
  for( ; isdigit( cur_ ); cur_ = stm_.next() ) first_ = first_*10 + cur_ - '0';

  // должно быть выражение вида {n,...} или {n}
  if( cur_ != ',' && cur_ != '}' )
  {
    std::stringstream st;
    st << "В данном регулярном выражении на позиции " << stm_.get_pos() << " должны быть символы ',' или '}'";
    throw std::logic_error( st.str() );
  }

  // если следующий символ это фигурная скобка т.е. имеем {n}, выходим
  if( cur_ == '}' ) return Token( first_ );

  // читаем следующий символ, это может быть либо '}' либо цифра в случае {n,m}
  cur_ = stm_.next();

  // если следующий символ это '}' т.е. мы имеем {n,} выходим
  if( cur_ == '}' ) return Token( first_, true );

  // читаем последовательность цифр
  unsigned int second_ = 0;
  for( ; isdigit( cur_ ); cur_ = stm_.next() ) second_ = second_*10 + cur_ - '0';

  // сейчас в любом случае должна быть '}'
  if( cur_ != '}' )
  {
    std::stringstream st;
    st << "В данном регулярном выражении на позиции " << stm_.get_pos() << " должен быть символ '}'";
    throw std::logic_error( st.str() );
  }

  return Token( first_, second_ );
}

// обработка специальных символов
Scanner::Token Scanner::FormEscapeExpr( char_type_t cur )
{
  std::string str;

  switch( cur )
  {
    case 's':
    str += ' ';
    return Token( INC_SYMBOLS, str );

    case 'S':
    str += ' ';
    {
      std::string ex_str;
      for( unsigned int cnt = 1; cnt < SYM_QUENT; ++ cnt )
        if( str.find( cnt ) == std::string::npos ) ex_str += char_type_t(cnt);

      return Token( INC_SYMBOLS, ex_str );
    }

    case 'w':
    str += '\n';
    str += '\r';
    str += '\t';
    str += '\v';
    return Token( INC_SYMBOLS, str );

    case 'W':
    str += '\n';
    str += '\r';
    str += '\t';
    str += '\v';
    {
      std::string ex_str;
      for( unsigned int cnt = 1; cnt < SYM_QUENT; ++ cnt )
        if( str.find( cnt ) == std::string::npos ) ex_str += char_type_t(cnt);

      return Token( INC_SYMBOLS, ex_str );
    }

    case 'd':
    {
      for( char_type_t cnt = '0'; cnt <= '9'; ++ cnt ) str += cnt;
    }
    return Token( INC_SYMBOLS, str );

    case 'D':
    {
      for( char_type_t cnt = '0'; cnt <= '9'; ++ cnt ) str += cnt;
      std::string ex_str;
      for( unsigned int cnt1 = 1; cnt1 < SYM_QUENT; ++ cnt1 )
        if( str.find( cnt1 ) == std::string::npos ) ex_str += char_type_t(cnt1);

      return Token( INC_SYMBOLS, ex_str );
    }
  }

  return Token( INC_SYMBOLS, str );
}

// выражение "точка" обозначает любой символ кроме символа конца строки
Scanner::Token Scanner::FormDotExpr()
{
  std::string sym;
  for( unsigned int cnt = 1; cnt < SYM_QUENT; ++ cnt )
    if( cnt != '\n' ) sym += char_type_t(cnt);

  return Token( INC_SYMBOLS, sym );
}

// возвращает строку из char*
std::string Scanner::GetStr( const char* str )
{
  std::string st;
  const char* p = str;
  while( *p ) st += *p ++;

  return st;
}
