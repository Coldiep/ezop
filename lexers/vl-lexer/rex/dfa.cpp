
#include <rex/dfa.h>
using rexp::Dfa;

#include <symbols.h>

#include <iostream>

// Печать таблицы переходов.
void Dfa::TableRow::Print() const {
  for (unsigned i = 1; i < row_.size(); ++i) {
    if (row_[i]) {
      std::string ch_str;
      lexer::GetUtf8Sequence(lexer::GetUtf16FromCp1251((char)(uint8_t)i), ch_str);
      std::cout << "[" << ch_str << "; " << row_[i] << "] ";
    }
  }
}

void Dfa::AddTransition(unsigned state_from, uint8_t symbol, unsigned state_to) {
  if (state_from > transitions_.size() - 1) {
    transitions_.resize(state_from + 1);
  }
  transitions_[state_from].AddMove(symbol, state_to);
}

void Dfa::Print() const {
    for (unsigned i = 1; i < transitions_.size(); ++i) {
        std::cout << i << ": ";
        transitions_[i].Print();
        std::cout << "\n";
    }

    std::cout << "accepted states: ";
    StateSet::iterator it = accept_states_.begin(), end = accept_states_.end();
    for (StateSet::iterator it = accept_states_.begin(), end = accept_states_.end(); it != end; ++it) {
      std::cout << (*it) << " ";
    }
    std::cout << "\n";
}
