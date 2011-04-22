
#include <dfa.h>
using rexp::Dfa;

#include <iostream>

void Dfa::AddTransition(unsigned state_from, char symbol, unsigned state_to) {
  if (state_from > transitions_.size() - 1) {
    transitions_.resize(state_from + 1);
  }
  transitions_[state].AddMove(symbol, state_to);
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
