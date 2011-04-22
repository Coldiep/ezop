
#pragma once

#include <rex/nfa.h>
#include <rex/dfa.h>

namespace rexp {

//! Конвертирует НКА в ДКА используя метод построения подмножеств.
class Nfa2DfaTransformer {
  // typedefs...
  typedef std::set<unsigned>        StateSet;
  typedef std::set<char>            SymbolSet;
  typedef std::map<char, StateSet>  CharMap;

  //! Состояние ДКА.
  struct DfaState {
    //! номер состояния.
    unsigned state_number_;

    //! множество состояний НКА, соответствующих данному ДКА.
    StateSet nfa_states_;

    //! маркировано или нет данное состояние.
    bool     is_marked_;

    //! конструктор.
    DfaState(unsigned state_number, const StateSet& nfa_states)
      : state_number_(state_number)
      , nfa_states_(nfa_states)
      , is_marked_(false) {
    }

    bool operator==(const StateSet& ss) const {
      return nfa_states_ == ss;
    }
  };

  //! Тип таблицы состояний.
  typedef std::map<unsigned, DfaState> DfaStateMap;

  //! Проверка, есть ли хотя бы одно немаркированное состояние в таблице.
  static bool IsUnmarked(const DfaStateMap& dfa_states) {
    for (DfaStateMap::const_iterator it = dfa_states.begin(); it != dfa_states.end(); ++it) {
      if (not it->second.is_marked_) {
        return true;
      }
    }
    return false;
  }

  //! Возвращает первое найденное немаркированное состояние.
  static DfaStateMap::iterator GetUnmarkedState(DfaStateMap& dfa_states) {
    for (DfaStateMap::iterator it = dfa_states.begin(); it != dfa_states.end(); ++it) {
      if (not it->second.is_marked_) {
        return it;
      }
    }
    return dfa_states.end();
  }

  //! Находит состояние ДКА как множество состояний НКА.
  static DfaStateMap::const_iterator FindState(const DfaStateMap& dfa_states, const StateSet& ss) {
    for (DfaStateMap::const_iterator it = dfa_states.begin(); it != dfa_states.end(); ++it) {
      if (ss == it->second.nfa_states_) {
        return it;
      }
    }
    return dfa_states.end();
  }

public:
  //! Трансформирует данный НКА в ДКА.
  static void Transform(const Nfa& nfa, Dfa& dfa);
};

} // namespace rexp

