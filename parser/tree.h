
#ifndef TREE_H__
#define TREE_H__

#include "list.h"
#include "stack.h"

namespace parser{

template < class Element >
struct tree_node{
  
  typedef Element                tree_node_element_t;
  typedef tree_node< tree_node_element_t >  tree_node_t;
  typedef parser::list< tree_node_t* >    tree_node_list_t;


  tree_node_element_t    element_;
  tree_node*        parent_;
  tree_node_list_t    level_;
  
  tree_node_t* add_child( tree_node_element_t _elem )
  {
    tree_node_t* new_node = new tree_node_t();
    
    new_node->element_ = _elem;
    new_node->parent_ = this;

    level_.push_back( new_node );
    return new_node;
  }

  tree_node_t* add_child( tree_node_t* _node )
  {
    _node->parent_ = this;

    level_.push_back( _node );
    return _node;
  }
  
  virtual ~tree_node()
  {
    for( tree_node_t* cur_node = level_.get_first(); cur_node; cur_node = level_.get_next() )
    {
      delete cur_node;
    }
  }

};

template< class TreeNode >
struct stack_element{
  TreeNode*    parent_;
  TreeNode*    node_;
};

template < class Element, class TreeNode >
class tree{
public:
  typedef TreeNode                tree_node_t;
  typedef Element                  tree_element_t;
  typedef stack_element< tree_node_t >      stack_element_t;
  typedef tree< tree_element_t, tree_node_t >    tree_t;
  
private:
  tree_node_t*    head_;

public:
  tree() : head_(0){}
  ~tree()
  {
    delete head_;
  }
  
  tree_node_t* get_head() const { return head_; }
  
  tree_node_t* add_head( tree_node_t* _node )
  {
    head_ = _node;
    return head_;
  }
  
  tree_node_t* add_head( const tree_element_t& _elem )
  {
    tree_node_t* new_node = new tree_node_t();
    new_node->element_ = _elem;
    new_node->parent_ = 0;
    
    return add_head( new_node );
  }
  
  tree_t* clone( tree_node_t* old_node, tree_node_t*& new_node )
  {
    // the tree is empty
    if( ! head_ ) return 0;
    
    tree_t* new_tree = new tree_t();
    tree_node_t* new_parent = new_tree->add_head( head_->element_ );
    
    
    stack< stack_element_t > node_stack;
    tree_node_t* _node = head_->level_.get_last();
    if( head_ == old_node )
    {
      if( _node ) _node = head_->level_.get_prev();
      new_node = new_parent;
    }
    
    for( ; _node; _node = head_->level_.get_prev() )
    {
      stack_element_t elem;
      elem.parent_ = new_parent;
      elem.node_ = _node;
      
      node_stack.push( elem );
    }
    
    while( ! node_stack.empty() )
    {
      stack_element_t elem = node_stack.pop();
      tree_node_t* cur_node = elem.node_;
      
      // copy current node
      tree_node_t* copy_cur_node = elem.parent_->add_child( cur_node->element_ );
      
      tree_node_t* iter_node = cur_node->level_.get_last();
      
      // we do not copy last child node of the node passed as old_node
      if( cur_node == old_node )
      {
        if( cur_node ) iter_node = cur_node->level_.get_prev();
        new_node = copy_cur_node;
      }
      
      for( ; iter_node; iter_node = cur_node->level_.get_prev() )
      {
        stack_element_t elem;
        elem.parent_ = copy_cur_node;
        elem.node_ = iter_node;

        node_stack.push( elem );
      }
    }
  
    return new_tree;
  }
};


} // namespace parser


#endif // TREE_H__
