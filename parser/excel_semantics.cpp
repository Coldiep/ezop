

#include "excel_semantics.h"
using namespace parser;

error_cost_list_t* excel_semantics::handle_error( private_::item_list_t* _item, token _token )
{
  std::cout << "error!\n";

  return new error_cost_list_t();
}

void excel_semantics::handle_terminal( int _symbol )
{

}

ast_node_t* excel_semantics::handle_before_nonterminal( private_::item* _item, ast_node_t* _ast_node, bool ambiguous )
{
  return 0;
}

ast_node_t* excel_semantics::handle_after_nonterminal( private_::item* _item, ast_node_t* _ast_node )
{
  return 0;
}
