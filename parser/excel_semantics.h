

#ifndef EXCEL_SEMANTICS_H__
#define EXCEL_SEMANTICS_H__

#include "semantics.h"
#include <iostream>

namespace parser{

struct excel_semantics : public semantics{

  error_cost_list_t*  handle_error( private_::item_list_t*, token );

  void        handle_terminal( int );
  ast_node_t*      handle_before_nonterminal( private_::item*, ast_node_t*, bool );
  ast_node_t*      handle_after_nonterminal( private_::item*, ast_node_t* );
};

} // namespace parser

#endif // EXCEL_SEMANTICS_H__
