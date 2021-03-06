
#include <lex.h>
using lexer::Lexer;

void Lexer::GetTokens(size_t start_pos, TokenList& tokens) {
  // Инициализируем множество автоматов, по которым будут производиться переходы.
  std::set<unsigned> lex_ids;
  for (LexTypeSet::iterator it = lex_types_.begin(), end = lex_types_.end(); it != end; ++it) {
    it->second->Reset();
    lex_ids.insert(it->first);
  }

  // Пары <id, position> используются для хранения последней позиции,
  // в которой автомат с данным id допустил входную цепочку.
  std::map<unsigned, size_t> accepted_types;
  for (it_.SetPos(start_pos); it_ != SymbolIterator() and not lex_ids.empty(); ++it_) {
    // Читаем символ.
    char sym = it_->cp1251_;

    // Производим переход по данному символу для каждого автомата.
    std::set<unsigned> lex_ids_tmp_;
    for (std::set<unsigned>::iterator it = lex_ids.begin(); it != lex_ids.end(); ++it) {
      lexer::LexType::Ptr lex_type = lex_types_[*it];
      if (lex_type->Move(sym)){
        if (lex_type->IsAccepted()) {
          accepted_types[*it] = it_.GetPos();
        }
        lex_ids_tmp_.insert(*it);
      }
    }

    // Запоминаем множество типов для следующей итерации.
    lex_ids = lex_ids_tmp_;
  }

  // Заполняем список токенов.
  for (std::map<unsigned, size_t>::iterator it = accepted_types.begin(); it != accepted_types.end(); ++it) {
    lexer::LexType::Ptr lex_type = lex_types_[it->first];
    std::string text(it_.GetStartIter() + start_pos, it_.GetStartIter() + it->second + 1);
    tokens.push_back(std::make_pair(parser::Token::Ptr(new parser::Token(lex_type->GetId(), start_pos, text)), lex_type->IsSpace()));
  }
}

parser::Lexer::TokenList Lexer::GetTokens(parser::Token::Ptr token) {
  parser::Lexer::TokenList tokens;
  size_t pos = token->abs_pos_ + token->length_;
  TokenList toks;
  GetTokens(pos, toks);
  for (TokenList::iterator it = toks.begin(); it != toks.end(); ++it) {
    if (it->second) {
      parser::Lexer::TokenList space_toks = GetTokens(it->first);
      tokens.insert(tokens.end(), space_toks.begin(), space_toks.end());
    } else {
      tokens.push_back(it->first);
    }

  }
  return tokens;
}

