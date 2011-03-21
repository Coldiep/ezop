

#ifndef ALLOCATOR_H__
#define ALLOCATOR_H__

#include <deque>

namespace parser{

template < class elem >
struct allocator
{
  struct elems_pool{
    elem      elem_;
    bool      free_;

    elems_pool() : free_(true){}
  };

  typedef std::deque< elems_pool* > elem_pools_t;

  elem_pools_t    elem_pools_;
  int          elem_free_;
  int          block_size_;
  int          cur_block_number_;
  bool        mem_need_;

  public:
    allocator() : elem_free_(0), block_size_(0), cur_block_number_(0), mem_need_(false){}
    ~allocator()
    {
      for( int i = 0; i < (int)elem_pools_.size(); ++ i )
      {
        delete[] elem_pools_[i];
      }
    }

  void init( size_t _size )
  {
    block_size_ = (int)_size;
    elems_pool* elems = new elems_pool[ _size ];
    elem_pools_.push_back( elems );
    cur_block_number_ = 0;
    mem_need_ = false;
  }

  elem* elem_alloc()
  {
    int block_number = cur_block_number_;
    
    if( elem_free_ >= block_size_ )
    {
      elem_free_ = 0;
      ++ block_number;
    }

    for(  ; block_number < (int)elem_pools_.size(); ++ block_number )
    {
      for( ; elem_free_ < block_size_; ++ elem_free_ )
      {
        if( ((elem_pools_[ block_number ])[ elem_free_ ]).free_ )
        {
          cur_block_number_ = block_number;
          
          elem_pools_[ block_number ][ elem_free_ ].free_ = false;
          return &(elem_pools_[ block_number ][ elem_free_ ++ ].elem_);
        }
      }
      
      elem_free_ = 0;
    }
    
    block_number = 0;
    for(  ; block_number <= cur_block_number_; ++ block_number )
    {
      for( elem_free_ = 0; elem_free_ < block_size_; ++ elem_free_ )
      {
        if( ((elem_pools_[ cur_block_number_ ])[ elem_free_ ]).free_ )
        {
          cur_block_number_ = block_number;
          
          elem_pools_[ cur_block_number_ ][ elem_free_ ].free_ = false;
          return &(elem_pools_[ cur_block_number_ ][ elem_free_ ++ ].elem_);
        }
      }
    }
    
    elems_pool* elems = new elems_pool[ block_size_ ];
    elem_pools_.push_back( elems );
    elem_free_ = 0;
    cur_block_number_ = (int)elem_pools_.size()-1;
    
    return &(elem_pools_[ cur_block_number_ ][ elem_free_ ++ ].elem_);
  }

  void elem_free( elem* _item )
  {
    elems_pool* ip = (elems_pool*)_item;
    ip->free_ = true;
  }
};

} // namespace parser

#endif // ALLOCATOR_H__

