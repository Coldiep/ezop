

#define DUMP_CONTENT

#include "earley_parser.h"
using parser::EarleyParser;

#ifdef DUMP_CONTENT
void EarleyParser::Item::Dump(Grammar* grammar, std::ostream& out) {
  bool dot_printed = false;
  out << state_number_ << "." << order_number_ << " ";
  out << "[ " << grammar->GetSymbolName(grammar->GetLhsOfRule(rule_id_)) << " --> ";
  for (unsigned rule_pos = 0; grammar->GetRhsOfRule(rule_id_, rule_pos) != Grammar::kBadSymbolId; ++rule_pos) {
    if (rhs_pos_ == rule_pos) {
      out << "* ";
      dot_printed = true;
    }
    out << grammar->GetSymbolName(grammar->GetRhsOfRule(rule_id_, rule_pos));
    out << " ";
  }

  if (not dot_printed) out << " * ";
  out << ", " << origin_ << ", ";

  if (lptr_) out << lptr_->state_number_ << "." << lptr_->order_number_;
  else out << "null";

  out << ", ";
#if 0
  if (not rptrs_.empty()) {
    out << "<";
    for (Rptr cur = rptrs_.get_first(); cur;) {
      out << cur->item_->state_number_ << "." << cur->item_->order_number_;
      if (cur = rptrs_.get_next()) out << ",";
    }
    out << ">";
  } else {
    out << "<null>";
  }
#endif
  out << "<null>";

  out << " ]\n";
}
#endif // DUMP_CONTENT

inline EarleyParser::Item* EarleyParser::State::AddItem(EarleyParser* parser, Grammar::RuleId rule_id, unsigned dot, size_t origin, Item* lptr, Item* rptr, Context* context) {
  // Получаем идентификатор символа в правой части правила. Если метка стоит в конце правила, то
  // будет возвращен 0, который используется как индекс для меток в конце правила.
  Grammar::SymbolId symbol_id = grammar_->GetRhsOfRule(rule_id, dot);

  // Инициализируем ситуацию.
  Item* item = disp_->GetItem(rule_id, dot, origin, lptr);
  if (rptr) item->rptrs_.push_back(Item::Rptr(context, rptr));
  item->order_number_ = num_of_items_;
  item->state_number_ = id_;

  // И добавляем ее в соответствующий список.
  items_[symbol_id].elems_.push_back(item);
  state_items_.push_back(item);
  ++num_of_items_;

  // Если символ в левой части правила -- начальный и метка в конце правила, то выставляем соответствующий флаг.
  if (grammar_->GetLhsOfRule(item->rule_id_) == grammar_->GetStartSymbol() and item->origin_ == 0) {
    is_completed_ = true;
  }

  // Проверка на правило вида A --> epsilon.
  if (dot == 0 and symbol_id == Grammar::kBadSymbolId) {
    SymbolItemList& er_item_list = items_with_empty_rules_[grammar_->GetLhsOfRule(rule_id) - grammar_->GetNumOfTerminals() - 1];
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
    SymbolItemList& er_item_list = items_with_empty_rules_[symbol_id - grammar_->GetNumOfTerminals() - 1];
    for (Item* cur = er_item_list.elems_.get_first(); cur; cur = er_item_list.elems_.get_next()) {
      parser->PutItemToNonhandledList(cur, true);
    }
  }

  return item;
}

inline void EarleyParser::Completer(size_t state_id, Item* item) {
  // Текущее состояние.
  State* cur_state = state_disp_.GetState(state_id);

  // Состояние, в котором была порождена ситуация.
  State* origin_state = state_disp_.GetState(item->origin_);

  // Начинаем обработку только, если определены текущее состояние и состояние, где была
  // порождена данная ситуация.
  if (cur_state and origin_state) {
    // Список ситуаций с точкой перед символом в левой части правила переданной ситуации.
    State::SymbolItemList& or_item_list = origin_state->items_[grammar_->GetLhsOfRule(item->rule_id_)];
    for (Item* cur = or_item_list.elems_.get_first(); cur; cur = or_item_list.elems_.get_next()) {
      // В случае неоднозначности одна и та же ситуация может обрабатываться несколько раз, проверяем это.
      if (not IsItemInList(cur_state->items_[grammar_->GetRhsOfRule(cur->rule_id_, cur->rhs_pos_ + 1)], cur, item)) {
        // Сдвигаем символ после точки в обрабатываемой ситуации и добавляем ее в текущее состояние.
        Item* new_item = cur_state->AddItem(this, cur->rule_id_, cur->rhs_pos_ + 1, cur->origin_, cur, item, NULL);
        PutItemToNonhandledList(new_item, true);
#       ifdef DUMP_CONTENT
        new_item->Dump(grammar_, std::cout);
#       endif
      }
    }
  }
}

inline void EarleyParser::Predictor(size_t state_id, Item* item) {
  // Текущее состояние.
  State* cur_state = state_disp_.GetState(state_id);

  // Символ после точки в правой части правила ситуации.
  unsigned sym_after_dot = grammar_->GetRhsOfRule(item->rule_id_, item->rhs_pos_);

  // Если текущая ситуация еще не была обработана операцией Predictor, то обрабатываем ее.
  if (cur_state and not cur_state->items_[sym_after_dot].handled_by_predictor_) {
    // Получаем список правил, в которых данный символ стоит в левой части.
    Grammar::RuleIdList& rules_list = grammar_->GetSymRules(sym_after_dot - grammar_->GetNumOfTerminals());
    for (unsigned cur = rules_list.get_first(); not rules_list.is_end(); cur = rules_list.get_next()) {
      // Добавляем ситуацию на основе этого правила.
      Item* new_item = cur_state->AddItem(this, cur, 0, cur_state->id_, NULL, NULL, NULL);
      PutItemToNonhandledList(new_item, false);

#     ifdef DUMP_CONTENT
      new_item->Dump(grammar_, std::cout);
#     endif
    }

    cur_state->items_[sym_after_dot].handled_by_predictor_ = true;
  }
}

inline bool EarleyParser::Scanner(size_t state_id, Token::Ptr token, size_t& new_state_id) {
  // Идентификатор символа, по которому будет производиться сдвиг.
  unsigned cur_symbol_id = grammar_->GetInternalSymbolByExtrernalId(token->type_);

  if (State* cur_state = state_disp_.GetState(state_id)) {
    // Получаем список ситуаций, у которых точка стоит перед данным символом.
    State::SymbolItemList& term_item_list = cur_state->items_[cur_symbol_id];
    if (term_item_list.elems_.size() > 0) {
      // Создаем новое состояние для данного символа.
      new_state_id = state_disp_.AddState(token);
      if (State* next_state = state_disp_.GetState(new_state_id)) {
        for (Item* cur = term_item_list.elems_.get_first(); cur; cur = term_item_list.elems_.get_next()) {
          // Добавляем новую ситуацию со сдвинутой точкой в новое состояние.
          Item* new_item = next_state->AddItem(this, cur->rule_id_, cur->rhs_pos_ + 1, cur->origin_, cur, NULL, NULL);
          PutItemToNonhandledList(new_item, false);

#       ifdef DUMP_CONTENT
          new_item->Dump(grammar_, std::cout);
#       endif
        }
        return true;
      }
    }
  }

  return false;
}

inline void EarleyParser::Closure(size_t state_id) {
  // Проходим по необработанным ситуациям и обрабатываем их операциями Completer или Predictor.
  while (not nonhandled_items_.empty()) {
    Item* item = nonhandled_items_.pop();
    unsigned sym_index = grammar_->GetRhsOfRule(item->rule_id_, item->rhs_pos_);
    // Если у ситуации точка в конце правила, то надо применить операцию Completer.
    if (sym_index == Grammar::kBadSymbolId) {
      Completer(state_id, item);
    // Если символ после точки нетерминал, то применяем операцию Predictor.
    } else if (grammar_->IsNonterminal(sym_index)) {
      Predictor(state_id, item);
    }
  }
}

inline bool EarleyParser::InitFirstState(size_t& state_id) {
  state_id = state_disp_.AddState(Token::Ptr(new Token()));
  State* next_state = state_disp_.GetState(state_id);
  Grammar::RuleIdList& rules_list = grammar_->GetSymRules(grammar_->GetStartSymbol() - grammar_->GetNumOfTerminals());

  if (rules_list.empty()) {
    return false;
  }

  for (unsigned cur = rules_list.get_first(); not rules_list.is_end(); cur = rules_list.get_next()) {
    Item* new_item = next_state->AddItem(this, cur, 0, state_id, NULL, NULL, NULL);
    PutItemToNonhandledList(new_item, false);

#   ifdef DUMP_CONTENT
    new_item->Dump(grammar_, std::cout );
#   endif
  }

  next_state->items_[grammar_->GetStartSymbol()].handled_by_predictor_ = true;

  return true;
}

bool EarleyParser::Parse() {
  // Инициализируем начальное состояние.
  size_t first_state_id = 0;
  if (not InitFirstState(first_state_id)) {
    return false;
  }
  Closure(first_state_id);

  // Проходим по цепочке (дереву при неоднозначности) терминалов, возвращаемой лексическим анализатором.
  StateList cur_gen_states;
  cur_gen_states.push_back(first_state_id);
  while (not lexer_->IsEnd()) {
    // Если мы не дошли до конца потока, но список состояний пуст, то значит, цепочка не разобрана, возвращаем false.
    if (cur_gen_states.empty()) {
      return false;
    }

    // Обрабатываем все состояния из текущего множества.
    StateList next_gen_states;
    while (not cur_gen_states.empty()) {
      size_t state_id = cur_gen_states.pop_front();
      State* state = state_disp_.GetState(state_id);

      // Получаем список токенов, идущих за данным.
      Lexer::TokenList tokens = lexer_->GetTokens(state->token_);
      for (size_t i = 0; i < tokens.size(); ++i) {
        // Для каждого токена осуществляем сдвиг и итеративно применяем операции Completer и Predictor.
        size_t new_state_id = 0;
        if (Scanner(state_id, tokens[i], new_state_id)) {
          Closure(new_state_id);
          next_gen_states.push_back(new_state_id);
        }
      }
    }

    // Проделываем следующую иетрацию над следующем множеством состояний.
    cur_gen_states = next_gen_states;
  }

  // Проходим по списку состояний, построенных для последних симовлов в потоке.
  bool parse_well = false;
  for (size_t state_id = cur_gen_states.get_first(); not cur_gen_states.is_end(); state_id = cur_gen_states.get_next()) {
    State* state = state_disp_.GetState(state_id);
    if (state->is_completed_) {
      parse_well = true;
    }
  }

  return parse_well;
}

void EarleyParser::Reset() {
}


#if 0
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
      item* new_item = new_state.add_item( cur->rule_id_, cur->rhs_pos_, cur->origin_, 0, 0, new_error_cost );
      put_item_to_nonhandled_list( new_item, false );
      
      // insertion
      new_item = new_state.add_item( cur->rule_id_, cur->rhs_pos_ + 1, cur->origin_, cur, 0, new_error_cost );
      put_item_to_nonhandled_list( new_item, false );
    }
  }
  
  delete error_cost_list;
  
  if( term_items.empty() ) return false;
  
  return true;
}
#endif
