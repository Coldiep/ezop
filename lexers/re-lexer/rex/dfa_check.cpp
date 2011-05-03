
#include <rex/dfa_check.h>
using rexp::DfaEqualCheck;

#include <map>

bool DfaEqualCheck::Check(const Dfa& left, const Dfa& right) {
  // Сначала сверяем размеры автоматов.
  if (left.transitions_.size() != right.transitions_.size()) {
    return false;
  }

  // Множество раннее протестированных состояний автоматов.
  std::set<unsigned> tested;

  // Карта равных состояний автоматов.
  std::map<unsigned, unsigned> equals;

  // Цикл по состояниям.
  for (unsigned left_ind = 1; left_ind < left.transitions_.size(); ++left_ind) {
    bool equal_any = false;

    // проходим по состояниям второго автомата
    for (unsigned right_ind = 1; right_ind < right.transitions_.size(); ++right_ind) {
      // если состояние уже было протестировано, пропускаем его.
      if (tested.find(right_ind) != tested.end()) {
        continue;
      }

      // Осуществляем сравнение.
      bool equal = true;
      for (unsigned ch_ind = 0; ch_ind < (1 << sizeof(char)* 8); ++ch_ind) {
        if (left.transitions_[left_ind].row_[ch_ind] != right.transitions_[right_ind].row_[ch_ind]) {
          equal = false;
          break;
        }
      }

      if (equal) {
        tested.insert(right_ind);
        equals.insert(std::map<unsigned, unsigned>::value_type(left_ind, right_ind));
        equal_any = true;
        break;
      }
    }

    if (not equal_any) {
      break;
    }
  }

  // Все ли состояния равны?
  if ((left.transitions_.size() - 1) != tested.size()) {
    return false;
  }

  // Сравниваем начальные состояния.
  std::map<unsigned, unsigned>::iterator it = equals.find(left.start_state_);
  if ((*it).second != right.start_state_) {
    return false;
  }

  // Сравниваем допускающие состояния.
  for (Dfa::StateSet::const_iterator fit = left.accept_states_.begin(); fit != left.accept_states_.end(); ++fit) {
    it = equals.find(*fit);
    if (right.accept_states_.find((*it).second) == right.accept_states_.end()) {
      return false;
    }
  }

  return true;
}
