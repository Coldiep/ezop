
#include <rex/nfa.h>
using rexp::Nfa;

#include <iostream>

void Nfa::TableRow::Print() const {
  std::cout << "{" << state_ << ";";
  for (CharMap::const_iterator cm_it = char_map_.begin(); cm_it != char_map_.end(); ++cm_it) {
    std::cout << "[" << cm_it->first << "; ";
    for (StateSet::const_iterator st_it = cm_it->second.begin(); st_it != cm_it->second.end(); ++st_it) {
      std::cout << *st_it << ",";
    }
    std::cout << "]";
  }
  std::cout << "}";
}

void Nfa::AddTransition(unsigned state_from, char symbol, unsigned state_to) {
  if (state_from != ZERO_STATE) {
    StateTable::iterator table_it = transitions_.find(state_from);
    if (table_it == transitions_.end()) {
        std::pair<StateTable::iterator, bool> res = transitions_.insert(std::make_pair(state_from, TableRow::Ptr(new TableRow(state_from))));
        table_it = res.first;
    }
    table_it->second->char_map_[symbol].insert(state_to);
  }
}

Nfa::StateSet Nfa::EpsilonClosure(const StateSet& states) const {
  StateSet res_set = states;

  // кладем все состояния в стек
  StateStack stck;
  for(StateSet::iterator it = res_set.begin(), end_it = res_set.end(); it != end_it; ++it) {
    stck.push(*it);
  }

  while (not stck.empty()) {
    // выталкиваем элемент из стека
    unsigned state_tmp = stck.top();
    stck.pop();

    // находим все состояния с эпсилон переходами из него
    StateSet set_tmp = Move(state_tmp, ZERO_STATE);

    // для каждого состояния U с переходом из state_tmp в U, помеченным как эпсилон...
    for (StateSet::iterator it = set_tmp.begin(); it != set_tmp.end(); ++it) {
      // если состояние не содержится во множестве...
      StateSet::iterator ttt_it = res_set.find(*it);
      if (ttt_it == res_set.end()) {
        // добавляем его
        res_set.insert(*it);

        // и запоминаем в стеке
        stck.push(*it);
      }
    }
  }

  return res_set;
}

Nfa::StateSet Nfa::Move(unsigned state, char symbol) const {
  // находим состояние
  StateTable::const_iterator table_it = transitions_.find(state);

  // если состояние найдено, находим переходы по данному символу
  if (table_it != transitions_.end()) {
    // находим состояния имеющие эпсилон переходы
    CharMap::const_iterator states_it = table_it->second->char_map_.find(symbol);
    if (states_it != table_it->second->char_map_.end()) {
      return states_it->second;
    }
  }

  return StateSet();
}

Nfa::StateSet Nfa::Move(const StateSet& states, char symbol) const {
  StateSet res_set;

  // берем множества состояний для каждого символа и добавляем его в результирующее множество
  StateSet::const_iterator it = states.begin(), end_it = states.end();
  for (StateSet::const_iterator it = states.begin(); it != states.end(); ++it) {
    StateSet tmp = Move(*it, symbol);
    res_set.insert(tmp.begin(), tmp.end());
  }

  return res_set;
}

Nfa::SymbolSet Nfa::GetSymSet() const {
  SymbolSet res;

  // проходим по таблице состояний...
  for (StateTable::const_iterator table_it = transitions_.begin(); table_it != transitions_.end(); ++table_it) {
    // берем символы для каждого состояния и добавляем их в результирующее множество
    CharMap::const_iterator state_it = table_it->second->char_map_.begin(),
      state_end_it = table_it->second->char_map_.end();
    for (CharMap::const_iterator state_it = table_it->second->char_map_.begin(); state_it != table_it->second->char_map_.end(); ++state_it) {
      if (state_it->first) {
        res.insert(state_it->first);
      }
    }
  }

  return res;
}

void Nfa::CloneFromOffset(Nfa& new_nfa, unsigned offset) const {
  // Крпируем состояния и переходы.
  for (StateTable::const_iterator it = transitions_.begin(); it != transitions_.end(); ++it) {
    unsigned new_state_number = it->second->state_ + offset;
    new_nfa.AddState(new_state_number);

    const CharMap& map_from = it->second->char_map_;
    for (CharMap::const_iterator map_it = map_from.begin(); map_it != map_from.end(); ++map_it) {
      char sym = map_it->first;
      StateSet sset = map_it->second;
      StateSet::iterator set_it = sset.begin(), set_end_it = sset.end();
      for (StateSet::iterator set_it = sset.begin(); set_it != sset.end(); ++set_it) {
        new_nfa.AddTransition(new_state_number, sym, (*set_it) + offset);
      }
    }
  }

  // Копируем начальное состояние.
  new_nfa.SetStartState(GetStartState() + offset);

  // Копируем допускающие состояния.
  new_nfa.CleanAcceptStates();
  for (StateSet::const_iterator it = accept_states_.begin(); it != accept_states_.end(); ++it) {
    new_nfa.AddToAcceptSet((*it) + offset);
  }
}

unsigned Nfa::GetMaxStateNum() const {
  unsigned max = 0;
  StateTable::const_iterator it = transitions_.begin(), end_it = transitions_.end();
  for (StateTable::const_iterator it = transitions_.begin(); it != transitions_.end(); ++it) {
    unsigned tmp = it->second->state_;
    if (tmp > max) {
      max = tmp;
    }
  }
  return max;
}

void Nfa::Print() const {
  for (StateTable::const_iterator it = transitions_.begin(); it != transitions_.end(); ++it) {
    it->second->Print();
    std::cout << "\n";
  }
}
