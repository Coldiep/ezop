

#ifndef SEMANTICS_H__
#define SEMANTICS_H__

#include "earley_parser.h"
#include "ast.h"
#include "tree.h"
#include "token.h"

namespace parser{

typedef tree_node< ast_data >    ast_node_t;
typedef parser::list< int >      error_cost_list_t;

struct semantics{

  virtual error_cost_list_t*    handle_error( private_::item_list_t*, token ) = 0;
  
  virtual void          handle_terminal( int ) = 0;
  virtual ast_node_t*        handle_before_nonterminal( private_::item*, ast_node_t*, bool ) = 0;
  virtual ast_node_t*        handle_after_nonterminal( private_::item*, ast_node_t* ) = 0;

};


} // namespace parser

#endif // SEMANTICS_H__
