
#ifndef STACK_H__
#define STACK_H__

#include "list.h"

namespace parser{

template < class Element >
class stack{

  typedef list< Element > list_t;

  list_t    list_;

public:
  void push( Element _elem )
  {
    list_.push_front( _elem );
  }

  Element pop()
  {
    return list_.pop_front();  
  }

  Element& top()
  {
    return list_.front();
  }

  bool find( Element elem )
  {
    return list_.find( elem );
  }

  bool empty()
  {
    return list_.empty();
    }
};


  } // namespace parser


#endif // STACK_H__
