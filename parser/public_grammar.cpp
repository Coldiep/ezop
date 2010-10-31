

#include <stdexcept>
#include <sstream>

#include "public_grammar.h"
using namespace parser;
using namespace private_;

//////////////////////////////////////////////////////////////////////////
// public_grammar constructor
//////////////////////////////////////////////////////////////////////////

public_grammar::public_grammar( const char* _grammar_name )
  :
  grammar_name_(_grammar_name),
  max_sym_id_(-1),
  max_rule_id_(-1),
  min_sym_id_(1<<sizeof(int)),
  min_rule_id_(1<<sizeof(int)),
  num_of_terms_(0),
  num_of_nonterms_(0)
{}


//////////////////////////////////////////////////////////////////////////
// methods to add symbols ang rules
//////////////////////////////////////////////////////////////////////////

void public_grammar::add_terminal( int _id, const char* _name )
{
  symbol_table_t::iterator it = symbols_.find(_id);
  if (it != symbols_.end()) {
    std::stringstream st;
    st << "public_grammar::add_terminal:\n";
    st << "The symbol with id = \"" << _id << "\" is already in the grammar's symbol set under the name \""
        << it->second.name_ << "\"";
        
    throw std::runtime_error( st.str().c_str() );
  }
  
  symbols_[_id] = symbol_(_name, false);
  
  if( max_sym_id_ < _id ) {
    max_sym_id_ = _id;
  }
  
  if( min_sym_id_ > _id )  {
    min_sym_id_ = _id;
  }
  
  ++ num_of_terms_;
}

void public_grammar::add_nonterminal( int _id, const char* _name ) {
  symbol_table_t::iterator it = symbols_.find(_id);
  if (it != symbols_.end()) {
    std::stringstream st;
    st << "public_grammar::add_nonterminal:\n";
    st << "The symbol with id = \"" << _id << "\" is already in the grammar's symbol set under the name \""
      << it->second.name_ << "\"";

    throw std::runtime_error( st.str().c_str() );
  }

  symbols_[_id] = symbol_(_name, true);
  
  if( max_sym_id_ < _id ) {
    max_sym_id_ = _id;
  }
  
  if( min_sym_id_ > _id ) {
    min_sym_id_ = _id;
  }
  
  ++ num_of_nonterms_;
}

void public_grammar::add_rule( int _id, const char* _name ) {
  rule_table_t::iterator it = rules_.find(_id);
  if (it != rules_.end()) {
    std::stringstream st;
    st << "public_grammar::add_rule:\n";
    st << "The rule with id = \"" << _id << "\" is already in the grammar's rule set under the name \""
      << it->second.name_ << "\"";

    throw std::runtime_error( st.str().c_str() );
  }

  rules_[_id] = rule_(_name);
  
  if( max_rule_id_ < _id ) {
    max_rule_id_ = _id;
  }
  
  if( min_rule_id_ > _id ) {
    min_rule_id_ = _id;
  }
}

void public_grammar::add_lhs_symbol( int _rule_id, int _sym_id ) {
  rule_table_t::iterator rule_it = rules_.find(_rule_id);
  if (rule_it == rules_.end()) {
    std::stringstream st;
    st << "public_grammar::add_lhs_symbol:\n";
    st << "The rule with id = \"" << _rule_id << "\" does not exist in the grammar's rule set";

    throw std::runtime_error( st.str().c_str() );
  }
  
  symbol_table_t::iterator symbol_it = symbols_.find(_sym_id);
  if (symbol_it == symbols_.end()) {
    std::stringstream st;
    st << "public_grammar::add_lhs_symbol:\n";
    st << "The symbol with id = \"" << _sym_id << "\" does not exist in the grammar's symbol set";

    throw std::runtime_error( st.str().c_str() );
  }

  if (not symbol_it->second.nonterminal_) {
    std::stringstream st;
    st << "public_grammar::add_lhs_symbol:\n";
    st << "The symbol with id = \"" << _sym_id << "\" is not nonterminal";
  
    throw std::runtime_error( st.str().c_str() );
  }

  rule_it->second.left_symbol_ = _sym_id;
}

void public_grammar::add_rhs_symbol( int _rule_id, int _sym_id ) {
  rule_table_t::iterator rule_it = rules_.find(_rule_id);
  if (rule_it == rules_.end()) {
    std::stringstream st;
    st << "public_grammar::add_rhs_symbol:\n";
    st << "The rule with id = \"" << _rule_id << "\" does not exist in the grammar's rule set";

    throw std::runtime_error( st.str().c_str() );
  }

  symbol_table_t::iterator symbol_it = symbols_.find(_sym_id);
  if (symbol_it == symbols_.end()) {
    std::stringstream st;
    st << "public_grammar::add_rhs_symbol:\n";
    st << "The symbol with id = \"" << _sym_id << "\" does not exist in the grammar's symbol set";

    throw std::runtime_error( st.str().c_str() );
  }

  rule_it->second.rhs_list_.push_back(_sym_id);
}

//////////////////////////////////////////////////////////////////////////
// printing grammar
//////////////////////////////////////////////////////////////////////////

void public_grammar::print( std::ostream& out )
{
  out << "**************************************************************************\n";
  out << "********* Grammar \"" << grammar_name_ << std::endl;
  out << "**************************************************************************\n";

  rule_table_t::iterator rule_it = rules_.begin(), rules_end = rules_.end();
  for (; rule_it != rules_end; ++ rule_it ) {
    out << "(" << rule_it->second.name_ << ", " << rule_it->first << ") [";

    symbol_table_t::iterator symbol_it = symbols_.find(rule_it->second.left_symbol_);
    out << symbol_it->second.name_ << " -->";

    list_t::iterator rhs_symbol_it = rule_it->second.rhs_list_.begin(), rhs_symbol_end = rule_it->second.rhs_list_.end();
    for (; rhs_symbol_it != rhs_symbol_end; ++rhs_symbol_it) {
      symbol_table_t::iterator symbol_it = symbols_.find(*rhs_symbol_it);
      out << " " << symbol_it->second.name_;
    }

    out << " ]\n";
  }
}

