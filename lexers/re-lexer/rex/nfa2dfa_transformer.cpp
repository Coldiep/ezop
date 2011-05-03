
#include <rex/nfa2dfa_transformer.h>
using rexp::Nfa2DfaTransformer;

#include <stdint.h>

void Nfa2DfaTransformer::Transform(const Nfa& nfa, Dfa& dfa) {
  // таблица состояний ДКА
  DfaStateMap dfa_state_set;

  // счетчик числа состояний ДКА
  unsigned int state_cnt = 0;

  // множество символов НКА
  SymbolSet sym_set = nfa.GetSymSet();

  // создаем начальное состояние ДКА как эпсилон замыкание начального состояние НКА
  StateSet start_set = nfa.EpsilonClosure(nfa.GetStartState());
  ++state_cnt;
  dfa_state_set[state_cnt] = DfaState::Ptr(new DfaState(state_cnt, start_set));
  dfa.AddState(state_cnt);
  dfa.SetStartState(state_cnt);

  // главный цикл. Каждый раз мы берем одно немаркированное состояние из таблицы и обрабатываем его
  while (IsUnmarked(dfa_state_set)) {
    // берем немаркированное состояние и маркируем его
    DfaState::Ptr cur_state = GetUnmarkedState(dfa_state_set)->second;
    cur_state->is_marked_ = true;

    // для каждого символа в НКА...
    for (SymbolSet::const_iterator sym_it = sym_set.begin(); sym_it != sym_set.end(); ++sym_it) {
      // новое состояние получается перемещением из текущего по некоторому символу и последующего
      // эпсилон замыкания. При этом полученное состояние уже может быть в таблице
      StateSet tmp = nfa.EpsilonClosure(nfa.Move(cur_state->nfa_states_, *sym_it));
      if (tmp.size() == 0) {
        continue;
      }

      // строим переход из текущего состояния в state_to
      unsigned state_to = state_cnt;

      // проверяем, есть ли уже данное состояние в таблице. Если нет, то добавляем его...
      DfaStateMap::const_iterator tmp_it = FindState(dfa_state_set, tmp);
      if (tmp_it == dfa_state_set.end()) {
        // добавляем новое состояние в таблицу
        ++state_cnt;
        dfa_state_set[state_cnt] = DfaState::Ptr(new DfaState(state_cnt, tmp));
        dfa.AddState(state_cnt);

        // запоминаем его номер
        state_to = state_cnt;
      }  else {
        // берем номер состояния, оно уже присутствовало в таблице
        state_to = tmp_it->second->state_number_;
      }

      // добавляем переход из текущего состояния в новое
      dfa.AddTransition(cur_state->state_number_, *sym_it, state_to);
    }
  }

  // Устанавливаем допускающие состояния полученного ДКА
  for (DfaStateMap::iterator state_it = dfa_state_set.begin(); state_it != dfa_state_set.end(); ++state_it) {
    StateSet& tmp_set = state_it->second->nfa_states_;
    for (StateSet::const_iterator st_it = tmp_set.begin(); st_it != tmp_set.end(); ++st_it) {
      const StateSet& accept_states = nfa.GetAcceptStates();
      if (accept_states.find(*st_it) != accept_states.end()) {
        dfa.AddToAcceptSet(state_it->second->state_number_);
        break;
      }
    }
  }
}
