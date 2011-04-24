
#pragma once

#include <lex_type.h>
#include <parser/lexer.h>

namespace lexer {

/*!
 * Осуществляет лексический анализ на основе множества типов лексем, содержащихся в анализаторе.
 * Анализ осуществляется параллельно, для каждого символа запускаются автоматы лексем. Каждый раз
 * выбирается самая длинная строка, соответствующая некоторой лексеме. Более подробно см.
 * определение метода GetTokens.
 */
class Lexer : public parser::Lexer {
  //! Тип множества лексических типов.
  typedef std::map<unsigned, lexer::LexType::Ptr> LexTypeSet;

  //! Множество лексических типов.
  LexTypeSet  lex_types_;

  //! Указатель на начало потока.
  const char* begin_;

  //! Указатель на конец потока.
  const char* end_;

  //! Внутренняя реализация получения списка токенов.
  size_t GetTokens(size_t pos, parser::Lexer::TokenList& tokens);

public:
  //! Инициализация по умолчанию.
  Lexer()
    : begin_(NULL)
    , end_(NULL) {
  }

  //! Возврат списка токенов, следующих за переданным в качестве параметра.
  parser::Lexer::TokenList GetTokens(parser::Token::Ptr token);

  //! Возвращает true, если достигнут конец потока.
  bool IsEnd() {
    return begin_ == end_;
  }

  /*!
   * \brief Добавляет лексический тип, заданный регулярным выражением.
   *
   * \param id      Идентификатор лексического типа.
   * \param re      Регулярное выражение.
   * \param name    Имя лексического типа.
   * \param ret     Возвращаемый тип или нет.
   * \return
   */
  void AddLexType(const unsigned& id, const std::string& re, const std::string& name, bool ret) {
    lex_types_[id] = LexType::Ptr(new LexType(id, re, name, ret));
  }

  /*!
   * \brief Добавляет лексический тип, заданный последовательностью символов.
   *
   * \param id      Идентификатор лексического типа.
   * \param name    Последовательность, задающая слово.
   * \return
   */
  void AddLexType(const unsigned& id, const std::string& word) {
    lex_types_[id] = LexType::Ptr(new LexType(id, word));
  }

  //! Удаляет лексический тип из спска лексем данного анализатора.
  void RemoveLexType(const unsigned& id) {
    lex_types_.erase(id);
  }

  //! Возвращает тип лексемы, соответствующий переданному идентификатору.
  LexType::Ptr GetLexType(const unsigned& id) const {
    LexTypeSet::const_iterator it = lex_types_.find(id);
    if (it != lex_types_.end()) {
      return it->second;
    }
    return LexType::Ptr();
  }

  //! Инициализирует лексический анализатор входным потоком.
  void SetInputStream(const char* begin, const char* end) {
    begin_ = begin;
    end_   = end;
  }
};

} // namespace lexer

