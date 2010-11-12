
#include <stdexcept>
#include <sstream>

#include "public_grammar.h"
using parser::PublicGrammar;

/*!
 * /brief Инициализация грамматики.
 *
 * \param desc Описание грамматики в удобном для человека виде.
 */
PublicGrammar::PublicGrammar( const char* desc )
  : grammar_name_(desc)
  , max_sym_id_(kUnknownMapId)
  , max_rule_id_(kUnknownMapId)
  , min_sym_id_(kUnknownMapId)
  , min_rule_id_(kUnknownMapId)
  , num_of_terms_(0)
  , num_of_nonterms_(0)
{}

/*!
 * \brief Добавление терминального символа.
 *
 * \param id    Идентификатор символа.
 * \param name  Имя символа для человека.
 */
void PublicGrammar::AddTerminal( MapId id, const char* name ) {
  // Проверяем, присутствует ли символ уже в грамматике или нет.
  SymbolTable::iterator it = symbols_.find(id);
  if (it != symbols_.end()) {
    std::stringstream st;
    st << "The symbol with id = \"" << id
       << "\" is already in the grammar's symbol set under the name \"" << it->second.name_ << "\"";
    throw std::invalid_argument(st.str().c_str());
  }

  // Добавляем символ в грамматику.
  symbols_[id] = Symbol(name, false);

  // Меняем минимальных и максимальные значения идентификаторов, если необходимо.
  if (max_sym_id_ < id) max_sym_id_ = id;
  if (min_sym_id_ == kUnknownMapId) min_sym_id_ = id;
  else if (min_sym_id_ > id) min_sym_id_ = id;

  // Увеличиваем количество терминальных символов.
  ++num_of_terms_;
}

/*!
 * \brief Добавление нетерминального символа.
 *
 * \param id    Идентификатор символа.
 * \param name  Имя символа для человека.
 */
void PublicGrammar::AddNonterminal( MapId id, const char* name ) {
  // Проверяем, присутствует ли символ уже в грамматике или нет.
  SymbolTable::iterator it = symbols_.find(id);
  if (it != symbols_.end()) {
    std::stringstream st;
    st << "The symbol with id = \"" << id
       << "\" is already in the grammar's symbol set under the name \"" << it->second.name_ << "\"";
    throw std::invalid_argument(st.str().c_str());
  }

  // Добавляем символ в грамматику.
  symbols_[id] = Symbol(name, true);

  // Меняем минимальных и максимальные значения идентификаторов, если необходимо.
  if (max_sym_id_ < id) max_sym_id_ = id;
  if (min_sym_id_ == kUnknownMapId) min_sym_id_ = id;
  else if (min_sym_id_ > id) min_sym_id_ = id;

  // Увеличиваем количество нетерминальных символов.
  ++num_of_nonterms_;
}

/*!
 * \brief Добавление правила.
 *
 * \param id    Идентификатор правила.
 * \param name  Имя символа для человека.
 */
void PublicGrammar::AddRule( MapId id, const char* name ) {
  // Проверяем, не занят ли уже переданный идентификатор под некоторое правило.
  RuleTable::iterator it = rules_.find(id);
  if (it != rules_.end()) {
    std::stringstream st;
    st << "The rule with id = \"" << id 
       << "\" is already in the grammar's rule set under the name \"" << it->second.name_ << "\"";
    throw std::invalid_argument(st.str().c_str());
  }

  // Добавляем правило.
  rules_[id] = Rule(name);

  // Меняем минимальное и максимальное значения идентификаторов правил, если необходимо.
  if (max_rule_id_ < id) max_rule_id_ = id;  
  if (min_rule_id_ == kUnknownMapId) min_rule_id_ = id;
  else if (min_rule_id_ > id) min_rule_id_ = id;
}

/*!
 * \brief Добавление символа в левой части правила.
 *
 * \param rule_id Идентификатор правила.
 * \param sym_id  Идентификатор символа.
 */
void PublicGrammar::AddLhsSymbol( MapId rule_id, MapId sym_id ) {
  // Проверяем наличие правила в грамматике.
  RuleTable::iterator rule_it = rules_.find(rule_id);
  if (rule_it == rules_.end()) {
    std::stringstream st;
    st << "The rule with id = \"" << rule_id << "\" does not exist in the grammar's rule set";
    throw std::invalid_argument(st.str().c_str());
  }

  // Проверяем наличие добавляемого символа в грамматике.
  SymbolTable::iterator symbol_it = symbols_.find(sym_id);
  if (symbol_it == symbols_.end()) {
    std::stringstream st;
    st << "The symbol with id = \"" << sym_id << "\" does not exist in the grammar's symbol set";
    throw std::runtime_error( st.str().c_str() );
  }

  // Необходимо проверить, является ли символ нетерминальным.
  if (not symbol_it->second.nonterminal_) {
    std::stringstream st;
    st << "The symbol with id = \"" << sym_id << "\" is not nonterminal";
    throw std::invalid_argument( st.str().c_str() );
  }

  // Устанавливаем символ в левую часть правила.
  rule_it->second.lhs_symbol_ = sym_id;
}

/*!
 * \brief Добавление символа в правой части правила.
 *
 * \param rule_id Идентификатор правила.
 * \param sym_id  Идентификатор символа.
 */
void PublicGrammar::AddRhsSymbol( MapId rule_id, MapId sym_id ) {
  // Проверяем наличие правила в грамматике.
  RuleTable::iterator rule_it = rules_.find(rule_id);
  if (rule_it == rules_.end()) {
    std::stringstream st;
    st << "The rule with id = \"" << rule_id << "\" does not exist in the grammar's rule set";
    throw std::invalid_argument(st.str().c_str());
  }

  // Проверяем наличие добавляемого символа в грамматике.
  SymbolTable::iterator symbol_it = symbols_.find(sym_id);
  if (symbol_it == symbols_.end()) {
    std::stringstream st;
    st << "The symbol with id = \"" << sym_id << "\" does not exist in the grammar's symbol set";
    throw std::runtime_error( st.str().c_str() );
  }

  // Добавляем символ в правую часть правила.
  rule_it->second.rhs_list_.push_back(sym_id);
}

/*!
 * \brief Печать содержимого грамматики.
 *
 * \param out Поток, в который будет производиться печать.
 */
void PublicGrammar::Print( std::ostream& out ) {
  out << "**************************************************************************\n";
  out << "********* Grammar \"" << grammar_name_ << std::endl;
  out << "**************************************************************************\n";

  RuleTable::iterator rule_it = rules_.begin(), rules_end = rules_.end();
  for (; rule_it != rules_end; ++rule_it) {
    out << "(" << rule_it->second.name_ << ", " << rule_it->first << ") [";

    SymbolTable::iterator symbol_it = symbols_.find(rule_it->second.lhs_symbol_);
    out << symbol_it->second.name_ << " -->";

    MapIdList::iterator rhs_symbol_it = rule_it->second.rhs_list_.begin(), rhs_symbol_end = rule_it->second.rhs_list_.end();
    for (; rhs_symbol_it != rhs_symbol_end; ++rhs_symbol_it) {
      SymbolTable::iterator symbol_it = symbols_.find(*rhs_symbol_it);
      out << " " << symbol_it->second.name_;
    }

    out << " ]\n";
  }
}

