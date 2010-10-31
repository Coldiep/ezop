

#include "grammar.h"
using namespace parser;

#include <fstream>

void private_::predict_cache::init( int _num_of_syms )
{
    //file << "predict_cache::init, _num_of_syms: " << _num_of_syms << std::endl;
  nonterm_vector_.resize( _num_of_syms );
}

private_::rule_list_t& private_::predict_cache::get_sym_rules( int _sym_index )
{
  return nonterm_vector_[ _sym_index ];
}

void private_::predict_cache::add_sym_rule( int _sym_index, int _rule_num )
{
  nonterm_vector_[ _sym_index ].push_back( _rule_num );
}


void grammar::initialize( public_grammar* _public_grammar )
{
  public_grammar_ = _public_grammar;
  
  // get configuration
  max_symbol_id_ = public_grammar_->get_max_symbol_id();
  max_rule_id_ = public_grammar_->get_max_rule_id();
  min_symbol_id_ = public_grammar_->get_min_symbol_id();
  min_rule_id_ = public_grammar_->get_min_rule_id();
  num_of_terminals_ = public_grammar_->get_num_of_terms();
  num_of_nonterminals_ = public_grammar_->get_num_of_nonterms();
  
  // fill symbols_...
  
  // allocate memory to keep grammar symbols and ids
  symbols_.resize( num_of_terminals_ + num_of_nonterminals_ );
  sym_id_to_symbols_map_.resize( public_grammar_->get_symbol_id_interval()+1 );
  
  // fill symbols_ by terminals
  int cur_sym_index = 0;
  
  const public_grammar::symbol_table_t& sym_table = public_grammar_->get_symbol_table();
  public_grammar::symbol_table_t::const_iterator sym_it = sym_table.begin(), sym_end = sym_table.end();
  for (; sym_it != sym_end; ++ sym_it) {
    if (not sym_it->second.nonterminal_) {
      symbols_[cur_sym_index] = sym_it->first;
      sym_id_to_symbols_map_[sym_it->first - min_symbol_id_ ] = cur_sym_index;

      ++ cur_sym_index;
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
  
  const public_grammar::rule_table_t& rules_table = public_grammar_->get_rule_table();
  
  rules_space_ = 0;
  num_of_rules_ = (int)rules_table.size();
  
  public_grammar::rule_table_t::const_iterator rule_it = rules_table.begin(), rules_end = rules_table.end();
  for( ; rule_it != rules_end; ++ rule_it ) {
    // for leh and -1 at the end of the rule
    rules_space_ += 2;
    
    // for rhs
    rules_space_ += (int)rule_it->second.rhs_list_.size();
  }
  
  rule_to_index_map_.resize( num_of_rules_ );
  internal_rule_to_id_map_.resize( num_of_rules_ );
  id_to_internal_rule_map_.resize( public_grammar_->get_rule_id_interval()+1 );
  index_to_rule_map_.resize( rules_space_ );
  rules_.resize( rules_space_ );
  
  predict_cache_.init( num_of_nonterminals_ );
  
  int cur_rule_num = 0;
  int cur_rule_index = 0;
  
  
  rule_it = rules_table.begin(), rules_end = rules_table.end();
  for( ; rule_it != rules_end; ++ rule_it, ++ cur_rule_num )
  {
    rule_to_index_map_[ cur_rule_num ] = cur_rule_index;
    index_to_rule_map_[ cur_rule_index ] = cur_rule_num;
    
    rules_[ cur_rule_index ] = get_symbol_by_id( rule_it->second.left_symbol_ );
    predict_cache_.add_sym_rule( rules_[ cur_rule_index ] - get_num_of_terminals(), cur_rule_num );
    ++ cur_rule_index;
      
    internal_rule_to_id_map_[ cur_rule_num ] = rule_it->first;
    id_to_internal_rule_map_[rule_it->first - min_rule_id_ ] = cur_rule_num;
    
    // for rhs
    private_::list_t::const_iterator ls_it = rule_it->second.rhs_list_.begin(), ls_end = rule_it->second.rhs_list_.end();
    for( ; ls_it != ls_end; ++ ls_it, ++ cur_rule_index )
    {
      rules_[ cur_rule_index ] = get_symbol_by_id(*ls_it);
    }
    
    rules_[ cur_rule_index ] = -1;
    ++ cur_rule_index;
  }
  
  // get start symbol number
  start_symbol_index_ = get_symbol_by_id( public_grammar_->get_start_symbol() );
}

grammar::grammar()
  :
  start_symbol_index_(-1),
  max_symbol_id_(-1),
  max_rule_id_(-1),
  num_of_terminals_(-1),
  num_of_nonterminals_(-1),
  num_of_rules_(-1),
  rules_space_(-1),
  public_grammar_(0)
{}
