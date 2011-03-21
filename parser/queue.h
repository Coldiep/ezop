

#ifndef QUEUE_H__
#define QUEUE_H__

#include "list.h"

namespace parser{

template < class Element >
class queue{

  typedef list< Element > list_t;
  
  list_t    list_;
  
public:
  void push( Element _elem )
  {
    list_.push_back( _elem );
  }
  
  Element pop()
  {
    return list_.pop_front();  
  }
  
  Element& front()
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


#endif // QUEUE_H__
