
#include <fstream>

#include "grammar.h"
using parser::Grammar;

/*!
 * \brief Конструктор инициализируются объектом PublicGrammar.
 *
 * \param[in] public_grammar Указатель на объект PublicGrammar.
 */
Grammar::Grammar( const PublicGrammar* public_grammar )
  : start_symbol_index_(kBadSymbolId)
  , max_symbol_id_(kBadSymbolId)
  , max_rule_id_(kBadSymbolId)
  , num_of_terminals_(kBadSymbolId)
  , num_of_nonterminals_(kBadSymbolId)
  , num_of_rules_(kBadSymbolId)
  , rules_space_(kBadSymbolId)
  , public_grammar_(public_grammar)
{
    Initialize();
}


//! Инициалиизация грамматики -- преобразование из PublicGrammar.
void Grammar::Initialize() {
  // Берем необходимую конфигурацию из PublicGrammar.
  max_symbol_id_        = public_grammar_->GetMaxSymbolId();
  max_rule_id_          = public_grammar_->GetMaxRuleId();
  min_symbol_id_        = public_grammar_->GetMinSymbolId();
  min_rule_id_          = public_grammar_->GetMinRuleId();
  num_of_terminals_     = public_grammar_->GetNumOfTerminals();
  num_of_nonterminals_  = public_grammar_->GetNumOfNonterminals();

  // Запоняем таблицу символов...

  // Выделяем память для хранения символов грамматики. Нулевой элемент таблицы
  // зарезирвирован и не используется в качестве идентификатора.
  symbols_.resize(num_of_terminals_ + num_of_nonterminals_ + 1);

  // Выделяем память для соответствия:
  //   внешний идентификатор символа (PublicGrammar) -- > индекс в массиве symbols_.
  external_to_internal_symbols_map_.resize(public_grammar_->GetSymbolIdInterval() + 1);

  // Заполняем таблицу символов терминалами. Первый элемент таблицы под индексом 0 зарезервирован.
  SymbolId cur_sym_index = 1;

  // Проходим по таблице символов и получаем идентификаторы терминалов.
  const PublicGrammar::SymbolTable& sym_table = public_grammar_->GetSymbolTable();
  for (PublicGrammar::SymbolTable::const_iterator sym_it = sym_table.begin(); sym_it != sym_table.end(); ++sym_it, ++cur_sym_index) {
    // Добавляем только терминалы.
    if (not sym_it->second.nonterminal_) {
      symbols_[cur_sym_index] = sym_it->first;
      external_to_internal_symbols_map_[sym_it->first - min_symbol_id_ ] = cur_sym_index;
    }
  }

  // Проходим по таблице символов и получаем идентификаторы нетерминальных символов.
  for (PublicGrammar::SymbolTable::const_iterator sym_it = sym_table.begin(); sym_it != sym_table.end(); ++sym_it, ++cur_sym_index) {
    // Добавляем только нетерминальные символы.
    if (sym_it->second.nonterminal_) {
      symbols_[cur_sym_index] = sym_it->first;
      external_to_internal_symbols_map_[sym_it->first - min_symbol_id_ ] = cur_sym_index;
    }
  }

  // Заполняем таблицу правил...
  const PublicGrammar::RuleTable& rules_table = public_grammar_->GetRuleTable();
  num_of_rules_ = rules_table.size();

  // Сначала подсчитываем память, необходимую для вектора правил.
  rules_space_ = 0;
  for (PublicGrammar::RuleTable::const_iterator rule_it = rules_table.begin(); rule_it != rules_table.end(); ++rule_it) {
    // Обязательно подсчитываем память для символа в левой части правила и нулевого символа в конце правила.
    rules_space_ += 2;

    // Подситываем количество памяти, необходимое для символов в правой части правила.
    rules_space_ += rule_it->second.rhs_list_.size();
  }

  // Теперь мы знаем, сколько памяти необходимо для вектора правил, заполняем таблицы соответствий.
  rules_.resize(rules_space_);
  rule_to_offset_map_.resize(num_of_rules_);
  offset_to_rule_map_.resize(rules_space_);
  internal_rule_to_id_map_.resize(num_of_rules_);
  id_to_internal_rule_map_.resize(public_grammar_->GetRuleIdInterval() + 1);

  // Не забываем инициализировать кэш Predictor.
  predict_cache_.Init(num_of_nonterminals_);

  RuleId cur_rule_id = kBadSymbolId; // Идентифкатор правила.
  size_t cur_rule_offset = 0; // Смещение от начала массива правил.
  for (PublicGrammar::RuleTable::const_iterator rule_it = rules_table.begin(); rule_it != rules_table.end(); ++rule_it, ++cur_rule_id) {
    // Соответствие смещение --> идентификатор правила и обратное.
    rule_to_offset_map_[cur_rule_id] = cur_rule_offset;
    offset_to_rule_map_[cur_rule_offset] = cur_rule_id;

    // Добавляем символ в левой части правил и заполняем кэш предиктора для этого символа.
    rules_[cur_rule_offset] = GetInternalSymbolByExtrernalId(rule_it->second.lhs_symbol_);
    predict_cache_.AddSymRule(rules_[cur_rule_offset] - GetNumOfTerminals(), cur_rule_id);
    ++cur_rule_offset;

    // Заполняем отношение внутренний идентификатор правил --> идентфикатор правила в PublicGrammar и обратное.
    internal_rule_to_id_map_[cur_rule_id] = rule_it->first;
    id_to_internal_rule_map_[rule_it->first - min_rule_id_] = cur_rule_id;

    // Проходим по правой части правила и добавляем соответствующие индексы в таблицу.
    for (PublicGrammar::MapIdList::const_iterator ls_it = rule_it->second.rhs_list_.begin(); ls_it != rule_it->second.rhs_list_.end(); ++ls_it, ++cur_rule_id) {
      rules_[cur_rule_offset] = GetInternalSymbolByExtrernalId(*ls_it);
    }

    // Необходимо отметить конец правила.
    rules_[cur_rule_offset] = kBadSymbolId;
  }

  // Задаем идентификатор начального нетерминала грамматики.
  start_symbol_index_ = GetInternalSymbolByExtrernalId(public_grammar_->GetStartSymbolId());
}
