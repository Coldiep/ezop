

#include "grammar.h"
using parser::Grammar;

#include <fstream>


Grammar::Grammar()
  : start_symbol_index_(kBadSymbolId)
  , max_symbol_id_(kBadSymbolId)
  , max_rule_id_(kBadSymbolId)
  , num_of_terminals_(kBadSymbolId)
  , num_of_nonterminals_(kBadSymbolId)
  , num_of_rules_(kBadSymbolId)
  , rules_space_(kBadSymbolId)
  , public_grammar_(NULL)
{}

void grammar::initialize( PublicGrammar* public_grammar )
{
  public_grammar_ = public_grammar;

  // get configuration
  max_symbol_id_ = public_grammar_->GetMaxSymbolId();
  max_rule_id_ = public_grammar_->GetMaxRuleId();
  min_symbol_id_ = public_grammar_->GetMinSymbolId();
  min_rule_id_ = public_grammar_->GetMinRuleId();
  num_of_terminals_ = public_grammar_->GetNumOfTerminals();
  num_of_nonterminals_ = public_grammar_->GetNumOfNonterminals();

  // fill symbols_...

  // allocate memory to keep grammar symbols and ids
  symbols_.resize(num_of_terminals_ + num_of_nonterminals_);
  sym_id_to_symbols_map_.resize(public_grammar_->GetSymbolIdInterval() + 1);

  // fill symbols_ by terminals
  int cur_sym_index = 0;

  const PublicGrammar::SymbolTable& sym_table = public_grammar_->GetSymbolTable();
  PublicGrammar::SymbolTable::const_iterator sym_it = sym_table.begin(), sym_end = sym_table.end();
  for (; sym_it != sym_end; ++sym_it) {
    if (not sym_it->second.nonterminal_) {
      symbols_[cur_sym_index] = sym_it->first;
      sym_id_to_symbols_map_[sym_it->first - min_symbol_id_ ] = cur_sym_index;
      ++cur_sym_index;
    }
  }

  // fill symbols_ by nonterminals
  sym_it = sym_table.begin(), sym_end = sym_table.end();
  for (; sym_it != sym_end; ++ sym_it) {
    if (sym_it->second.nonterminal_) {
      symbols_[cur_sym_index] = sym_it->first;
      sym_id_to_symbols_map_[sym_it->first - min_symbol_id_ ] = cur_sym_index;
      ++cur_sym_index;
    }
  }

  // fill rules_...

  const PublicGrammar::RuleTable& rules_table = public_grammar_->GetRuleTable();

  rules_space_ = 0;
  num_of_rules_ = (int)rules_table.size();

  PublicGrammar::RuleTable::const_iterator rule_it = rules_table.begin(), rules_end = rules_table.end();
  for (; rule_it != rules_end; ++rule_it) {
    // for leh and -1 at the end of the rule
    rules_space_ += 2;

    // for rhs
    rules_space_ += (int)rule_it->second.rhs_list_.size();
  }

  rule_to_index_map_.resize(num_of_rules_);
  internal_rule_to_id_map_.resize(num_of_rules_);
  id_to_internal_rule_map_.resize(public_grammar_->GetRuleIdInterval() + 1);
  index_to_rule_map_.resize(rules_space_);
  rules_.resize(rules_space_);

  predict_cache_.init(num_of_nonterminals_);

  int cur_rule_num = 0;
  int cur_rule_index = 0;

  rule_it = rules_table.begin(), rules_end = rules_table.end();
  for (; rule_it != rules_end; ++rule_it, ++cur_rule_num) {
    rule_to_index_map_[cur_rule_num] = cur_rule_index;
    index_to_rule_map_[cur_rule_index] = cur_rule_num;

    rules_[cur_rule_index] = get_symbol_by_id(rule_it->second.lhs_symbol_);
    predict_cache_.add_sym_rule(rules_[cur_rule_index] - get_num_of_terminals(), cur_rule_num);
    ++cur_rule_index;

    internal_rule_to_id_map_[cur_rule_num] = rule_it->first;
    id_to_internal_rule_map_[rule_it->first - min_rule_id_] = cur_rule_num;

    // for rhs
    PublicGrammar::MapIdList::const_iterator ls_it = rule_it->second.rhs_list_.begin(), ls_end = rule_it->second.rhs_list_.end();
    for (; ls_it != ls_end; ++ls_it, ++cur_rule_index) {
      rules_[cur_rule_index] = get_symbol_by_id(*ls_it);
    }

    rules_[cur_rule_index] = -1;
    ++cur_rule_index;
  }

  // get start symbol number
  start_symbol_index_ = get_symbol_by_id(public_grammar_->GetStartSymbolId());
}
