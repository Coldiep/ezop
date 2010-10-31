

#ifndef LIST_H__
#define LIST_H__

namespace parser{

template < class Element >
class list{
  struct list_node{
    Element        elem_;
    list_node*      next_;
    list_node*      prev_;

    list_node():next_(0), prev_(0){}
  };

  list_node*        head_;
  list_node*        last_;
  
  list_node*        iter_;
  
  int            size_;

public:
  list():head_(0), last_(0), size_(0), iter_(0) {}

  void push_front( Element elem )
  {
    list_node* new_node = new list_node();
    new_node->elem_ = elem;
    new_node->prev_ = 0;
    new_node->next_ = head_;

    if( head_ )
    {
      head_->prev_ = new_node;
    }
    else
    {
      last_ = new_node;
    }

    head_ = new_node;
    
    ++ size_;
  }

  void push_back( Element elem )
  {
    list_node* new_node = new list_node();
    new_node->elem_ = elem;
    new_node->next_ = 0;
    new_node->prev_ = last_;

    if( last_ )
    {
      last_->next_ = new_node;
    }
    else
    {
      head_ = new_node;
    }

    last_ = new_node;
    
    ++ size_;
  }
  
  Element pop_front()
  {
    if( head_ )
    {
      Element tmp = head_->elem_;
      list_node* tmp_node = head_;

      head_ = head_->next_;
      if(head_)
      {
        head_->prev_ = 0;
      }
      else
      {
        last_ = 0;
      }

      delete tmp_node;
      
      -- size_;
      
      return tmp;
    }

    return Element();
  }

  Element pop_back()
  {
    if( last_ )
    {
      Element tmp = last_->elem_;
      list_node* tmp_node = last_;

      last_ = last_->prev_;
      if( last_ )
      {
        last_->next_ = 0;
      }
      else
      {
        head_ = 0;
      }

      delete tmp_node;
      
      -- size_;
      
      return tmp;
    }

    return Element();
  }


  Element& front()
  {
    return head_->elem_;
  }

  Element& back()
  {
    return last_->elem_;
  }
  
  bool empty()
  {
    return head_ == 0;
  }

  bool find( Element elem )
  {
    for( list_node* node = head_; node; node = node->next_)
    {
      if( node->elem_ == elem ) return true;
    }

    return false;
  }
  
  int size() const { return size_; }
  
  void reset()
  {
    while( ! empty() )
    {
      pop_back();
    }
  
  }
  
  Element get_first()
  {
    if( head_ )
    {
      iter_ = head_;
      return iter_->elem_;
    }
    else
    {
        iter_ = 0;
    }
    
    return Element();
  }
  
  Element get_next()
  {
    if( iter_ )
    {
      iter_ = iter_->next_;
      
      if( iter_ )
      {
        return iter_->elem_;
      }
    }

    return Element();
  }
  
  Element get_last()
  {
    if( last_ )
    {
      iter_ = last_;
      return iter_->elem_;
    }
    else
    {
        iter_ = 0;
    }

    return Element();
  }

  Element get_prev()
  {
    if( iter_ )
    {
      iter_ = iter_->prev_;

      if( iter_ )
      {
        return iter_->elem_;
      }
    }

    return Element();
  }

  
  bool is_end()
  {
    return iter_ == 0;
  }

};

} // namespace parser

#endif // LIST_H__
