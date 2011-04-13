

#ifndef SEMANTICS_H__
#define SEMANTICS_H__

#include <parser/earley_parser.h>
#include <parser/token.h>

namespace parser {

//! Базовый класс для всех семантических контекстов.
struct SemanticContext {
  //! Виртуальный деструктор.
  virtual inline ~SemanticContext() = 0;
};

virtual inline SemanticContext::~SemanticContext() {
}

struct Semantics {
  /*!
   * \brief Обработка терминального символа.
   *
   * \return true если терминал необходимо обрабатывать и false в противном случае.
   */
  virtual bool HandleTerminal() = 0;
  virtual ast_node_t*        handle_before_nonterminal( private_::item*, ast_node_t*, bool ) = 0;
  virtual ast_node_t*        handle_after_nonterminal( private_::item*, ast_node_t* ) = 0;

};


} // namespace parser

#endif // SEMANTICS_H__
