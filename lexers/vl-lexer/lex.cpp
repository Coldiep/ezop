
#include <lex.h>
using lexer::Lexer;

size_t Lexer::GetTokens(size_t start_pos, parser::Lexer::TokenList& tokens) {
  // Инициализируем множество автоматов, по которым будут производиться переходы.
  std::set<unsigned> lex_ids;
  for (LexTypeSet::iterator it = lex_types_.begin(), end = lex_types_.end(); it != end; ++it) {
    it->second->Reset();
    lex_ids.insert(it->first);
  }

  // Пары <id, position> используются для хранения последней позиции,
  // в которой автомат с данным id допустил входную цепочку.
  std::map<unsigned, size_t> accepted_types;
  for (size_t pos = start_pos; begin_ + pos < end_ and not lex_ids.empty(); ++pos) {
    // Читаем символ.
    char sym = *(begin_ + pos);

    // Производим переход по данному символу для каждого автомата.
    std::set<unsigned> lex_ids_tmp_;
    for (std::set<unsigned>::iterator it = lex_ids.begin(); it != lex_ids.end(); ++it) {
      lexer::LexType::Ptr lex_type = lex_types_[*it];
      if (lex_type->Move(sym)){
        if (lex_type->IsAccepted()) {
          accepted_types[*it] = pos;
        }
        lex_ids_tmp_.insert(*it);
      }
    }

    // Запоминаем множество типов для следующей итерации.
    lex_ids = lex_ids_tmp_;
  }

  // Заполняем список токенов.
  size_t ws_pos = start_pos;
  for (std::map<unsigned, size_t>::iterator it = accepted_types.begin(); it != accepted_types.end(); ++it) {
    lexer::LexType::Ptr lex_type = lex_types_[it->first];
    if (not lex_type->IsSpace()) {
      std::string text(begin_ + start_pos, begin_ + it->second);
      tokens.push_back(parser::Token::Ptr(new parser::Token(lex_type->GetId(), start_pos, text)));
    } else {
      ws_pos = it->second;
    }
  }

  return ws_pos;

}

parser::Lexer::TokenList Lexer::GetTokens(parser::Token::Ptr token) {
  parser::Lexer::TokenList tokens;
  size_t pos = token->abs_pos_ + token->length_;
  while (not IsEnd()) {
    pos = GetTokens(pos, tokens);
    if (not tokens.empty()) {
      break;
    }
  }
  return tokens;
}

