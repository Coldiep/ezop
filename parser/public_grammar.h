

#ifndef PUBLIC_GRAMMAR_H__
#define PUBLIC_GRAMMAR_H__

#include <list>
#include <map>
#include <iostream>

namespace parser{

namespace private_{

  struct symbol_{
    const char*  name_;      // the pointer to the name of the symbol
    bool    nonterminal_;  // it the symbol nonterminal?
  
    symbol_()
      : name_(0)
    {}

    symbol_( const char* _name, bool _nonterminal )
      : name_(_name)
      , nonterminal_(_nonterminal)
    {}
  };
  
  typedef std::list<int> list_t;

  struct rule_{
    const char*  name_;      // the pointer to the name of the symbol
    int      left_symbol_;  // the left symbol enum id
    list_t    rhs_list_;    // the right hand list of grammar symbols of the rule

    rule_()
      : name_(0)
      , left_symbol_(-1)
    {}

    rule_( const char* _name )
      : name_(_name)
      , left_symbol_(-1)
    {}
  };
}

class public_grammar{
public:
  typedef std::map<int, private_::symbol_>    symbol_table_t;
  typedef std::map<int, private_::rule_>      rule_table_t;

private:  
  symbol_table_t      symbols_;    // the grammar symbols
  rule_table_t      rules_;      // the grammar rules
  
  int            num_of_terms_;  // the number of the grammar's terminals
  int            num_of_nonterms_;  // the number of the grammar's nonterminals
  int            max_sym_id_;  // the maximum of symbol's id
  int            max_rule_id_;  // the maximum of rule's id
  int            min_sym_id_;  // the minimum of symbol's id
  int            min_rule_id_;  // the minimum of rule's id
  
  const char*        grammar_name_;  // the grammar's name
  
  int            start_symbol_;  // the start symbol number

public:
  public_grammar( const char* );

  // methods to add symbols and rules
  void          add_terminal( int _id, const char* _name );
  void          add_nonterminal( int _id, const char* _name );
  void          add_rule( int _id, const char* _name );
  void          add_lhs_symbol( int _rule_id, int _sym_id );
  void          add_rhs_symbol( int _rule_id, int _sym_id );
  
  // methods to get symbols and rules
  
  int            get_num_of_terms() const { return num_of_terms_; }
  int            get_num_of_nonterms() const { return num_of_nonterms_; }
  int            get_max_symbol_id() const { return max_sym_id_; }
  int            get_max_rule_id() const { return max_rule_id_; }
  int            get_min_symbol_id() const { return min_sym_id_; }
  int            get_min_rule_id() const { return min_rule_id_; }
  unsigned int      get_symbol_id_interval() const { return max_sym_id_-min_sym_id_; }
  unsigned int      get_rule_id_interval() const { return max_rule_id_-min_rule_id_; }
  
  const symbol_table_t&  get_symbol_table() const { return symbols_; }
  const rule_table_t&    get_rule_table() const { return rules_; }

  // print grammar
  void          print( std::ostream& out );
  
  // start symbol manipulation
  void          set_start_symbol( int _start_symbol ) { start_symbol_ = _start_symbol; }
  int            get_start_symbol() const { return start_symbol_; }
};

} // namespace parser


#endif // PUBLIC_GRAMMAR_H__
