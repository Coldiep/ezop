

#include "earley_parser.h"
#include "semantics.h"
using parser::EarleyParser;

bool EarleyParser::Item::operator==( const Item& rhs ) {
  return rule_id_ == rhs.rule_id_ and rhs_pos_ == rhs.rhs_pos_ and origin_ == rhs.origin_ and lptr_ == rhs.lptr_;
}

void EarleyParser::Item::Print( Grammar* grammar, std::ostream& out ) {
  bool dot_printed = false;
  out << state_number_ << "." << order_number_ << " ";
  out << "[ " << grammar->GetSymbolName(grammar->GetLhsOfRule(rule_num_)) << " --> ";
  for (unsigned rule_pos = 0; grammar->GetRhsOfRule(rule_id_, rule_pos) != Grammar::kBadSymbolId; ++rule_pos) {
    if (rhs_pos_ == rule_pos) {
      out << "* ";
      dot_printed = true;
    }
    out << grammar->GetSymbolName(grammar->GetRhsOfRule(rule_num_, rule_pos));
    out << " ";
  }

  if (not dot_printed) out << " * ";
  out << ", " << origin_ << ", ";

  if (lptr_) out << lptr_->state_number_ << "." << lptr_->order_number_;
  else out << "null";

  out << ", ";
  if (not rptrs_.empty()) {
    out << "<";
    for (item* cur = rptrs_.get_first(); cur;) {
      out << cur->state_number_ << "." << cur->order_number_;
      if (cur = rptrs_.get_next())  out << ",";
    }
    out << ">";
  } else {
    out << "<null>";
  }

  out << ", ";
  out << (unsigned)error_ <<  " ]\n";
}

EarleyParser::State::SymbolItemList::SymbolItemList()
  : handled_by_predictor_(false)
{}

void EarleyParser::State::SymbolItemList::Uninit( EarleyParser* parser ) {
  while (not elems_.empty()) {
    parser->items_pool_.elem_free(elems_.pop_back());
  }
  handled_by_predictor_ = false;
}

EarleyParser::State::State()
  : num_of_items_(0)
  , state_number_(0)
  , grammar_(NULL)
  , parser_(NULL)
  , is_completed_(false)
{}

void EarleyParser::State::Uninit() {
  for (size_t i = 0; i < items_.size(); ++i) {
    items_[i].Uninit(parser_);
  }

  for (size_t i = 0; i < items_with_empty_rules_.size(); ++i) {
    items_with_empty_rules_[i].uninit(parser_);
  }

  num_of_items_ = 0;
  state_number_ = 0;
  grammar_      = NULL;
  parser_       = NULL;
  token_        = Token();
  is_completed_ = false;
}

EarleyParser::State::~State() {
  Uninit();
}

void EarleyParser::State::Init( EarleyParser* parser, Grammar* grammar, size_t id, Token token ) {
  grammar_      = grammar;
  parser_       = parser;
  state_number_ = id;
  num_of_items_ = 0;
  token_        = token;
  is_completed_ = false;

  // Число списков для символов -- это число символов + 1 для метки в конце правила. Для этого
  // специального случая используется список с нулевым индексом.
  items_.resize(grammar_->GetNumOfTerminals() + grammar_->GetNumOfNonterminals() + 1);

  // Ситуаций для правил с пустой правой частью ровно столько, сколько нетерминальных символов в грамматике.
  items_with_empty_rules_.resize(grammar_->GetNumOfNonterminals());
}

inline EarleyParser::State::Item* EarleyParser::State::AddItem( Grammar::RuleId rule_id, unsigned dot, size_t origin, Item* lptr, Item* rptr, unsigned char error = 0 ) {
  // Получаем идентификатор символа в правой части правила. Если метка стоит в конце правила, то
  // будет возвращен 0, который используется как индекс для меток в конце правила.
  Grammar::SymbolId symbol_id = grammar_->GetRhsOfRule(rule_id, dot);

  // Инициализируем ситуацию.
  Item* item = parser_->items_pool_.elem_alloc();
  item->rule_num_   = rule_id;
  item->rhs_pos_    = dot;
  item->origin_     = origin;
  item->lptr_       = lptr;
  item->error_      = error;
  item->handled_    = false;
  if (rptr) item->rptrs_.push_back(rptr);
  item->order_number_ = num_of_items_;
  item->state_number_ = state_number_;

  // И добавляем ее в соответствующий список.
  SymbolItemList& sym_list = items_[symbol_id];
  items_[symbol_id].elems_.push_back(item);
  state_items_.push_back(item);
  ++num_of_items_;

  // Если символ в левой части правила -- начальный и метка в конце правила, то выставляем соответствующий флаг.
  if (grammar_->GetLhsOfRule(item->rule_id_) == grammar_->GetStartSymbol() and item->origin_ == 0)
      is_completed_ = true;

  // Проверка на правило вида A --> epsilon.
  if (dot == 0 and symbol_id == Grammar::kBadSymbolId) {
    SymbolItemList& er_item_list = items_with_empty_rules_[grammar_->GetLhsOfRule(rule_id) - grammar_->GetNumOfTerminals()];
    for (Item* cur = er_item_list.elems_.get_first(); cur; cur = er_item_list.elems_.get_next()) {
      if (*item == *cur) {
        return item;
      }
    }
    er_item_list.elems_.push_back(item);
  }

  // Правило -- это правило вида A --> alpha * B beta. Надо добавить ситуацию для правила B --> epsilon в список
  // необработанных ситуаций.
  else if (symbol_id != Grammar::kBadSymbolId and grammar_->IsNonterminal(symbol_id)) {
    SymboltemList& er_item_list = items_with_empty_rules_[symbol_id - grammar_->GetNumOfTerminals()];
    for (Item* cur = er_item_list.elems_.get_first(); cur; cur = er_item_list.elems_.get_next()) {
      parser_->PutItemToNonhandledList(cur, true);
    }
  }

  return item;
}

void EarleyParser::State::::Print( std::ostream& out ) {
  out << "\n****** State number = " << state_number_ << " *** Number of items = " << num_of_items_ << " ******\n";
  for (Item* item = state_items_.get_first(); item; item = state_items_.get_next()) {
    item->Print(grammar_, out);
  }
}

EarleyParser::EarleyParser( Grammar* grammar, Lexer* lexer, semantics* semantics, unsigned max_error_value )
  : grammar_(grammar)
  , lexer_(lexer)
  , max_error_value_(max_error_value)
  , semantics_(semantics)
{
  items_pool_.init(1024*1024);
}

inline void parser::EarleyParser::PutItemToNonhandledList( Item* item, bool check ) {
  if (not check or not nonhandled_items_.find(item)) nonhandled_items_.push(item);
}

inline bool parser::EarleyParser::IsItemInList( State::SymbolItemList& item_list, Item* item, Item* rptr ) {
  Item tmp_item;
  tmp_item.rhs_pos_   = item->rhs_pos_ + 1;
  tmp_item.rule_num_  = item->rule_num_;
  tmp_item.origin_    = item->origin_;
  tmp_item.lptr_      = item;

  for (Item* cur = item_list.elems_.get_first(); cur; cur = item_list.elems_.get_next()) {
    // we already have this item in the set
    if (tmp_item == *cur) {
      cur->rptrs_.push_back(rptr);
      return true;
    }
  }

  return false;
}

inline void EarleyParser::Completer( Item* item ) {
  State& cur_state = *states_[states_.size() - 1];
  State& origin_state = *states_[item->origin_];
  State::SymbolItemList& or_item_list = origin_state.items_[grammar_->GetLhsOfRule(item->rule_num_) + 1];

  for (Item* cur = or_item_list.elems_.get_first(); cur; cur = or_item_list.elems_.get_next()) {
    if (not IsItemInList(cur_state.items_[grammar_->GetRhsOfRule(cur->rule_num_, cur->rhs_pos_ + 1) + 1], cur, item)) {
      Item* new_item = cur_state.AddItem(cur->rule_num_, cur->rhs_pos_ + 1, cur->origin_, cur, item, cur->error_);
      PutItemToNonhandledList(new_item, true);

#ifdef PRINT_ADDING
      new_item->Print(grammar_, std::cout);
#endif
    }
  }
}

inline void EarleyParser::Predictor( Item* item ) {
  int sym_after_dot = grammar_->GetRhsOfRule( _item->rule_num_, _item->rhs_pos_ );
  state& cur_state = *states_[states_.size() - 1];
  
  if( ! cur_state.items_[ sym_after_dot + 1 ].handled_by_predictor_ )
  {
    Grammar::RuleIdList& rules_list = grammar_->GetSymRules( sym_after_dot - grammar_->GetNumOfTerminals() );
    for( int cur = rules_list.get_first(); ! rules_list.is_end() ; cur = rules_list.get_next() )
    {
      item* new_item = cur_state.add_item( cur, 0, (int)states_.size()-1, 0, 0, _item->error_ );
      put_item_to_nonhandled_list( new_item, false );

  #ifdef PRINT_ADDING
      new_item->print( grammar_, std::cout );
  #endif
    }

    cur_state.items_[ sym_after_dot + 1 ].handled_by_predictor_ = true;
  }
}

inline bool earley_parser::scanner()
{
  cur_token_ = lexer_->get_token();
  if( lexer_->is_end() ) return false;
  
  int next_sym_id = cur_token_.type_;
  int cur_term = grammar_->GetInternalSymbolByExtrernalId( next_sym_id );

  state& cur_state = *states_[ states_.size() - 1 ];
  state::item_list& term_item_list = cur_state.items_[ cur_term + 1 ];
  if( term_item_list.elems_.size() > 0 )
  {
    states_.push_back( new state() );
    state& new_state = *states_[ states_.size() - 1 ];
    new_state.init( this, grammar_, (int)states_.size() - 1, cur_token_ );

    for( item* cur = term_item_list.elems_.get_first(); cur ; cur = term_item_list.elems_.get_next() )
    {
      item* new_item = new_state.add_item( cur->rule_num_, cur->rhs_pos_ + 1, cur->origin_, cur, 0, cur->error_ );
      put_item_to_nonhandled_list( new_item, false );

    #ifdef PRINT_ADDING
      new_item->print( grammar_, std::cout );
    #endif
    }
  }
  else
  {
    return false;
  }

  return true;
}


inline bool   earley_parser::error_scanner()
{
  state& cur_state = *states_[ states_.size() - 1 ];
  
  // items to pass to handle_error method
  private_::item_list_t term_items;
  
  for( int i = 1; i <= grammar_->GetNumOfTerminals(); ++ i )
  {
    state::item_list& term_item_list = cur_state.items_[ i ];
    for( item* cur = term_item_list.elems_.get_first(); cur ; cur = term_item_list.elems_.get_next() )
    {
      term_items.push_back( cur );
    }
  }
  
  error_cost_list_t* error_cost_list = semantics_->handle_error( &term_items, cur_token_ );
  
  states_.push_back( new state() );
  state& new_state = *states_[ states_.size() - 1 ];
  new_state.init( this, grammar_, (int)states_.size() - 1, cur_token_ );
  
  item* cur = term_items.get_first();
  int error_cost = error_cost_list->get_first();
  for( ; cur ; cur = term_items.get_next(), error_cost = error_cost_list->get_next() )
  {
    int new_error_cost = cur->error_ + error_cost;
    if( new_error_cost <= max_error_value_ )
    {
      // deletion
      item* new_item = new_state.add_item( cur->rule_num_, cur->rhs_pos_, cur->origin_, 0, 0, new_error_cost );
      put_item_to_nonhandled_list( new_item, false );
      
      // insertion
      new_item = new_state.add_item( cur->rule_num_, cur->rhs_pos_ + 1, cur->origin_, cur, 0, new_error_cost );
      put_item_to_nonhandled_list( new_item, false );
    }
  }
  
  delete error_cost_list;
  
  if( term_items.empty() ) return false;
  
  return true;
}

inline void earley_parser::closure() {
  while (not nonhandled_items_.empty()) {
    item* _item = nonhandled_items_.pop();
    int sym_index = grammar_->GetRhsOfRule(_item->rule_num_, _item->rhs_pos_);

    if (sym_index == Grammar::kBadSymbolId) {
      completer( _item );
    } else if (grammar_->IsNonterminal(sym_index)) {
      predictor(_item);
    }
  }
}

inline bool earley_parser::init_first_set()
{
  states_.push_back( new state() );
  state& new_state = *states_[ 0 ];
  new_state.init( this, grammar_, 0, cur_token_ );
  
  Grammar::RuleIdList& rules_list = grammar_->GetSymRules( grammar_->GetStartSymbol() - grammar_->GetNumOfTerminals() );
  
  if( rules_list.empty() ) return false;
  
  for( int cur = rules_list.get_first(); ! rules_list.is_end() ; cur = rules_list.get_next() )
  {
    item* new_item = new_state.add_item( cur, 0, 0, 0, 0, 0 );
    put_item_to_nonhandled_list( new_item, false );

#ifdef PRINT_ADDING
    new_item->print( grammar_, std::cout );
#endif
  }

  new_state.items_[ grammar_->GetStartSymbol() + 1 ].handled_by_predictor_ = true;
  
  return true;
}

bool earley_parser::parse()
{
  bool result = init_first_set();
  closure();

  bool parse_well_done = true;
  while( result )
  {  
    result = scanner();
    if( ! result )
    {
      if( lexer_->is_end() ) break;
      //result = error_scanner();
      parse_well_done = false;
      return false;
    }
    if( result ) closure();
  }

  state& last_state = *states_[ states_.size()-1 ];
  if( parse_well_done && last_state.is_completed_ )
  {
    build_parse_trees( last_state );
    return true;
  }
  
  return false;
}

void earley_parser::reset()
{
  for( int ind = 0; ind < (int)states_.size(); ++ ind )
  {
    delete states_[ ind ];
  }

  for( parse_tree_t* _tree = parse_tree_list_.get_first(); _tree; _tree = parse_tree_list_.get_next() )
  {
    delete _tree;
  }

  parse_tree_list_.reset();
  states_.resize( 0 );
}

earley_parser::~earley_parser()
{
  reset();
}


void earley_parser::print( std::ostream& _out )
{
  for( int i = 0; i < (int)states_.size(); ++ i )
  {
    states_[ i ]->print( _out );
  }
}

void earley_parser::print_trees( std::ostream& _out )
{
  for( parse_tree_t* _tree = parse_tree_list_.get_first(); _tree; _tree = parse_tree_list_.get_next() )
  {
    print_tree( _tree, _out );
  }
}

inline void earley_parser::fill_rhs_stack( item* _item, rhs_stack_t& _stack )
{
  for( item* cur_item = _item; cur_item->rhs_pos_ > 0; cur_item = cur_item->lptr_ )
  {
    private_::rhs_stack_element stack_elem;
    
    int symbol_index = grammar_->GetRhsOfRule( cur_item->rule_num_, cur_item->rhs_pos_-1 );
    if( grammar_->IsNonterminal( symbol_index ) )
    {
      if( cur_item->rptrs_.size() > 1 )
      {
        stack_elem.type_ = private_::rhs_stack_element::eRptrs;
        stack_elem.rptrs_ = &cur_item->rptrs_;
      }
      else
      {
        stack_elem.type_ = private_::rhs_stack_element::eItem;
        stack_elem.item_ = cur_item->rptrs_.front();
      }
    }
    else
    {
      stack_elem.type_ = private_::rhs_stack_element::eSymbol;
      stack_elem.symbol_ = symbol_index;
    }
    
    _stack.push( stack_elem );
  }
}

// parameters:
// item* _item - the item needed to be handled
// parse_tree_node_t* _parent - the parent node
// parse_tree_t& _tree - the parse tree
inline void  earley_parser::build_parse_trees( item* _item, parse_tree_node_t* _parent, parse_tree_t& _tree )
{
  // fill rhs stack for this item
  rhs_stack_t item_rhs_stack;
  fill_rhs_stack( _item, item_rhs_stack );
  
  parse_tree_node_stack_t parse_roots_;
  parse_roots_.push_back( parse_tree_element( &_tree, _parent ) );
  
  // go through the stack
  for( ; ! item_rhs_stack.empty(); item_rhs_stack.pop() )
  {
    rhs_stack_element cur_stack_element = item_rhs_stack.top();
    
    switch( cur_stack_element.type_ )
    {
      case rhs_stack_element::eSymbol:
      {
        parse_tree_element cur_elem = parse_roots_.get_first();
        for( ; ! parse_roots_.is_end(); cur_elem = parse_roots_.get_next() )
        {
          cur_elem.node_->add_child( cur_stack_element.symbol_ );
        }
        
        break;
      }
      case rhs_stack_element::eItem:
      {
        item* cur_item = cur_stack_element.item_;
        int symbol = grammar_->GetLhsOfRule( cur_item->rule_num_ );
        
        parse_tree_element cur_elem = parse_roots_.get_first();
        for( ; ! parse_roots_.is_end(); cur_elem = parse_roots_.get_next() )
        {
          parse_tree_node_t* new_tree_node = cur_elem.node_->add_child( symbol );

          cur_item->handled_ = true;
          build_parse_trees( cur_item, new_tree_node, _tree );
          cur_item->handled_ = false;
        }
        
        break;
      }
      case rhs_stack_element::eRptrs:
      {
        bool first = true;
        parse_tree_node_stack_t new_nodes;
        
        item_list_t* rptrs = cur_stack_element.rptrs_;
        for( item* cur_item = rptrs->get_first(); cur_item; cur_item = rptrs->get_next() )
        {
          if( ! cur_item->handled_ )
          {
            parse_tree_node_stack_t cur_nodes;
            
            if( first )
            {
              parse_tree_element cur_elem = parse_roots_.get_first();
              for( ; ! parse_roots_.is_end(); cur_elem = parse_roots_.get_next() )
              {
                cur_nodes.push_back( cur_elem );
              }

              first = false;
            }
            else
            {
              parse_tree_element cur_elem = parse_roots_.get_first();
              for( ; ! parse_roots_.is_end(); cur_elem = parse_roots_.get_next() )
              {
                parse_tree_element new_elem;
                new_elem.tree_ = cur_elem.tree_->clone( cur_elem.node_, new_elem.node_ );
                parse_tree_list_.push_back( new_elem.tree_ );
                
                cur_nodes.push_back( new_elem );
                new_nodes.push_back( new_elem );
              }
            }

            int symbol = grammar_->GetLhsOfRule( cur_item->rule_num_ );
            
            parse_tree_element cur_elem = cur_nodes.get_first();
            for( ; ! cur_nodes.is_end(); cur_elem = cur_nodes.get_next() )
            {
              parse_tree_node_t* new_tree_node = cur_elem.node_->add_child( symbol );

              cur_item->handled_ = true;
              build_parse_trees( cur_item, new_tree_node, *cur_elem.tree_ );
              cur_item->handled_ = false;
            }
          }
        }
        
        parse_tree_element cur_elem = new_nodes.get_first();
        for( ; ! new_nodes.is_end(); cur_elem = new_nodes.get_next() )
        {
          parse_roots_.push_back( cur_elem );
        }
        break;
      }
    }
  }
}

void earley_parser::build_parse_trees( state& _state )
{
  state::item_list& completed_items_list = _state.items_[ 0 ];
  
  item* cur_item = completed_items_list.elems_.get_first();
  for( ; cur_item ; cur_item = completed_items_list.elems_.get_next() )
  {
    // we have item kind of [S --> alpha *, 0, ...]
    if( grammar_->GetStartSymbol() == grammar_->GetLhsOfRule( cur_item->rule_num_ ) && cur_item->origin_ == 0 )
    {
      
      parse_tree_t* parse_tree = new parse_tree_t();
      parse_tree_node_t* tree_head = parse_tree->add_head( grammar_->GetStartSymbol() );
      parse_tree_list_.push_back( parse_tree );
      
      cur_item->handled_ = true;
      build_parse_trees( cur_item, tree_head, *parse_tree );
      cur_item->handled_ = false;
    }
  }
}

void earley_parser::print_tree( parse_tree_node_t* _node, int _level, std::ostream& _out )
{
  for( int i = 0; i < _level; ++ i )
  {
    _out << "   ";
  }

  _out << grammar_->GetSymbolName( _node->element_ ) << std::endl;

  parse_tree_node_t* iter_node = _node->level_.get_last();
  for( ; ! _node->level_.is_end(); iter_node = _node->level_.get_prev() )
  {
    print_tree( iter_node, _level + 1, _out );
  }
}

void earley_parser::print_tree( parse_tree_t* _tree, std::ostream& _out )
{
  _out << std::endl;
  if( _tree->get_head() )  print_tree( _tree->get_head(), 0, _out );
  _out << std::endl;
}

