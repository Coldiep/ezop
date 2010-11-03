

#ifndef GRAMMAR_H__
#define GRAMMAR_H__

#include <vector>

#include "public_grammar.h"
#include "list.h"

namespace parser {

namespace private_{

  typedef parser::list< int > rule_list_t;
  typedef std::vector< rule_list_t > nonterm_vector_t;

  struct predict_cache{
  
    nonterm_vector_t    nonterm_vector_;
    
    void          init( int );
    rule_list_t&      get_sym_rules( int );
    void          add_sym_rule( int, int );
  };

}


class grammar{

  typedef std::vector< int > int_table_t;

  int        start_symbol_index_;      // start symbol index of grammar
  int        max_symbol_id_;          // max symbol id (it can be more then number of symbols)
  int        max_rule_id_;          // max rule id (it can be more then number of rules)
  int        min_symbol_id_;          // min symbol id
  int        min_rule_id_;          // min rule id
  int        num_of_terminals_;        // the number of terminals in the grammar
  int        num_of_nonterminals_;      // the number of nonterminals in the grammar
  int        num_of_rules_;          // the number of rules in the grammar
  int        rules_space_;          // the number of bytes neeeded to keep grammar rules as it is
                          // described in rules_ variable's comments
  
  int_table_t    symbols_;            // the vector contains grammar symbols ids (firstly go terminals
                          // after nonterminals. The first_nonterminal_number_ contains
                          // the index of the first nonterminal in the vector)

  int_table_t    rules_;              // the vector contains grammar rules (each rule in the vector 
                          // consists of lh symbol and list of rh symbols and -1 at the
                          // end as delimiter). To read the rule it is needed to remember
                          // the the index of lhs and read till the element of the vector
                          // is not -1.
                          
  int_table_t    sym_id_to_symbols_map_;      // map symbol ids to grammar symbols
  int_table_t    rule_to_index_map_;        // map rules to index of vector of rules
  int_table_t    index_to_rule_map_;        // map index of vector of rules to rules
  int_table_t    id_to_internal_rule_map_;    // map internal rules id to rules
  int_table_t    internal_rule_to_id_map_;    // map rules to internal rule ids

  PublicGrammar*  public_grammar_;        // the pointer to public grammar
  
  private_::predict_cache  predict_cache_;      // the prediction cache

public:
  grammar();
  
  bool      is_nonterminal( int _symbol_id ) const { return _symbol_id >= num_of_terminals_; }
  int        get_start_symbol() const { return start_symbol_index_; }
  
  int        get_symbol_by_id( int _id ) const { return sym_id_to_symbols_map_[ _id-min_symbol_id_ ]; }
  int        get_id_by_symbol( int _sym_num ) const { return symbols_[ _sym_num ]; }
  int        get_index_by_rule( int _rule_num ) const { return rule_to_index_map_[ _rule_num ]; }
  int        get_rule_by_index( int _index ) const { return index_to_rule_map_[ _index ]; }
  int        get_rule_by_internal_id( int _id ) const { return id_to_internal_rule_map_[ _id-min_rule_id_ ]; }
  int        get_internal_id_by_rule( int _rule ) const { return internal_rule_to_id_map_[ _rule ]; }
  
  int        get_num_of_terminals() const { return num_of_terminals_; }
  int        get_num_of_nonterminals() const { return num_of_nonterminals_; }
  
  int        get_lhs_of_rule( int _rule_num ) const { return rules_[ get_index_by_rule( _rule_num ) ]; }
  int        get_rhs_of_rule( int _rule_num, int _sym_ind ) const { return rules_[ get_index_by_rule(_rule_num ) + _sym_ind + 1 ]; }


  private_::rule_list_t&  get_sym_rules( int _sym_index ) { return predict_cache_.get_sym_rules( _sym_index ); }

  const char* get_symbol_name( int _sym_num ) const {
    return (public_grammar_->GetSymbolTable().find(symbols_[_sym_num ]))->second.name_;
  }

  void      initialize( PublicGrammar* public_grammar );
  

};

} // namespace parser

#endif // GRAMMAR_H__
