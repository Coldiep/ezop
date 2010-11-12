
/**************************************************************************************************************
  Earley's algorithm implementation declarations.
**************************************************************************************************************/


#ifndef EARLEY_PARSER_H__
#define EARLEY_PARSER_H__

#include "grammar.h"
#include "list.h"
#include "queue.h"
#include "stack.h"
#include "tree.h"
#include "lexer.h"
#include "allocator.h"
#include "ast.h"

#include <vector>
#include <deque>
#include <iostream>

//#define PRINT_STATS

namespace parser{


class earley_parser;

namespace private_{

// the list of Earley's items
typedef list< struct item* >    item_list_t;

// Earley's algorithm item.
// each item consists of dotted rule, origin number, left pointer, list of right pointers and
// some other specific members
struct item{

  // the Earley's algorithm base memebers 
  int        rule_num_;    // the rule's number
  int        rhs_pos_;    // the dot position
  int        origin_;    // the origin state number
  
  // the extended memebers for adapted algorithm
  item*      lptr_;      // the pointer to the item with dot on symbol on the left
  item_list_t    rptrs_;      // the list of pointers to "rigtht" items, used when ambiguity is occured
  
  // some specific members for optimization purpose
  unsigned char  error_;      // the error level
  bool      handled_;    // has the item been handled in parse tree building?
  int        order_number_;  // the order number of the item in the state
  int        state_number_;  // the order number of the item in the state

  void      print( Grammar*, std::ostream& );  // print the item
};

// the operation needed to 
bool        operator == ( const item& _left, const item& _right );

// the Earley's state - the list of items
struct state{

  // the list of items in the state. It is created for each symbol of the grammar and one symbol more
  // to be for each symbol after the dot on rhs of the rule.
  struct item_list{
    item_list_t    elems_;            // the items list
    bool      handled_by_predictor_;    // set to true when the nonteminal is handled by Predictor

    item_list();

    // uninitialization procedure needed to do uninitialization but not free the memory
    void uninit( earley_parser* );
  };
  
  // the vector of items. It's needed to be vector to be indexed
  typedef std::vector< item_list >  item_list_vector_t;

  item_list_vector_t      items_;            // the list of items of the state
  item_list_vector_t      items_with_empty_rules_;  // the list of items with empty rules
  
  item_list_t          state_items_;        // the list of items of the state in adding order

  int              num_of_items_;        // the number of items in the state
  int              state_number_;        // the state number
  int              is_completed_;        // does the state contain item [S--> alpha *, 0, ...]?
                              // S - start nonterminal of the grammar
                                
  token            token_;            // the state token
  
  Grammar*          grammar_;          // the CF grammar
  earley_parser*        parser_;          // the parser reference

  // intialization/uninitialization
  
  state();
  ~state();

  void            init( earley_parser*, Grammar*, int, token );
  void            uninit();

  // the method adds a new member to the state
  inline item*        add_item( int, int, int, item*, item*, int = 0 );

  // print the state to the stream passed
  void            print( std::ostream& );
};

// the stack to keep elemens of parse tree's level
struct rhs_stack_element{

  // the element may be item, the list of references or the symbol
  enum rhs_stack_type{ eItem = 1, eRptrs, eSymbol };
  
  // the stack element type
  rhs_stack_type    type_;
  
  // the element' data
  union{
    item*      item_;
    item_list_t*  rptrs_;
    int        symbol_;
  };

};

// parse tree element's class
struct parse_tree_element{

  // typedefs
  typedef parser::tree_node< int >      tree_node_t;
  typedef parser::tree< int, tree_node_t >  tree_t;

  tree_t*                    tree_; // the pointer to the
  tree_node_t*                node_;
  
  parse_tree_element( tree_t* _tree, tree_node_t* _node )
  :
  tree_(_tree),
  node_(_node)
  {}
  
  parse_tree_element()
  :
  tree_(0),
  node_(0)
  {}
};


} // namespace private_

typedef queue< private_::item* >                          item_queue_t;
typedef parser::stack< private_::rhs_stack_element >                rhs_stack_t;
typedef std::deque< private_::state* >                        state_vector_t;
typedef tree_node< int >                              parse_tree_node_t;
typedef parser::tree< parse_tree_node_t::tree_node_element_t, parse_tree_node_t >  parse_tree_t; 
typedef parser::list< parse_tree_t* >                        parse_tree_list_t;
typedef list< private_::parse_tree_element >                    parse_tree_node_stack_t;

using namespace private_;

class earley_parser{

//////////////////////////////////////////////////////////////////////////
// friend declarations
//////////////////////////////////////////////////////////////////////////

  friend struct private_::state;
  friend struct private_::state::item_list;
  friend struct private_::item;

//////////////////////////////////////////////////////////////////////////
// variables
//////////////////////////////////////////////////////////////////////////

  item_queue_t        nonhandled_items_;      // the queue of unhandled items
  state_vector_t        states_;          // the states of the algorithm
  
  Grammar*          grammar_;          // the CF grammar
  lexer*            lexer_;            // the lexical analyzer
  
  parser::allocator< item >  items_pool_;        // the pool of items
  
  parse_tree_list_t      parse_tree_list_;      // the list of parse trees
  
  int              max_error_value_;      // the maximum error value
  
  struct semantics*      semantics_;          // the opointer to semantic interface
  
  token            cur_token_;          // the current token
  
  
//////////////////////////////////////////////////////////////////////////
// methods
//////////////////////////////////////////////////////////////////////////

  inline void          completer( item* );      // the Completer algorithm operation
  inline void          predictor( item* );      // the Predictor algorithm operation
  inline bool          scanner();          // the Scanner algorithm operation
  
  inline bool          error_scanner();      // the function runs when scanner method is erred

  inline void          closure();          // the Closure algorithm operation
  inline bool          init_first_set();      // the first set initialization
  
  inline void          put_item_to_nonhandled_list( item*, bool );  // put the item to the unhandled items list
                                      // if it is not there already
  inline bool          is_item_in_list(  state::item_list&, item*, item* );
  
  inline void          fill_rhs_stack( item*, rhs_stack_t& );  // fill rhs stack ot the passed item
  
  void            build_parse_trees( state& );
  inline void          build_parse_trees( item*, parse_tree_node_t*, parse_tree_t& );
  
  void            build_ast_trees( state& );
  inline void          build_ast_trees_recursive( item*, parse_tree_node_t*, parse_tree_t& );
  void            build_ast_trees_iterative( item*, parse_tree_node_t*, parse_tree_t& );
  
  
  void            print_tree( parse_tree_node_t*, int, std::ostream& );
  void            print_tree( parse_tree_t*, std::ostream& );

public:
  earley_parser( Grammar*, lexer*, struct semantics*, int = 3 );
  ~earley_parser();

  bool            parse();          // the parsing of the input string
  void            reset();          // reset parser object to init state 
  
  void            print( std::ostream& );      // print the items
  void            print_trees( std::ostream& );  // print parse trees
};

} // namespace parser

#endif // EARLEY_PARSER_H__

