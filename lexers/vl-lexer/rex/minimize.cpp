
#include <rex/minimize.h>
using rexp::Minimization;

void Minimization::Minimize() {
  // 1. Разбиваем данный ДКА на две группы. В одной допускающие состояния, в другой все оставшиеся.
  GroupList old_groups;
  PrimaryDivide(old_groups);

  for (;;) {
    // 2. Проводим разбиение и получаем новое множество групп состояний.
    GroupList new_groups;
    for (GroupList::iterator it = old_groups.begin(), end = old_groups.end(); it != end; ++it) {
      DivideGroup(*it, new_groups, old_groups);
    }

    // 3. Сравниваем старое и новое множества групп. Если они равны - завершаем алгоритм.
    //    В противном случае делаем новое множество групп текущим и переходим к шагу 2.
    if (old_groups.size() == new_groups.size()) {
      bool equal = true;
      for (unsigned ind = 0; ind < old_groups.size(); ++ind) {
        if (not (old_groups[ind] == new_groups[ind])) {
          equal = false;
          break;
        }
      }

      if (equal) {
        break;
      }
    }

    old_groups.clear();
    for (GroupList::iterator it1 = new_groups.begin(), end1 = new_groups.end() ; it1 != end1; ++it1) {
      old_groups.push_back(*it1);
    }
  }

  // 4. В каждой группе выбираем одно состояние как представителя данной группы. Формируем
  //    новый ДКА из представителей.
  Dfa new_dfa;
  new_dfa.transitions_.resize(old_groups.size());
  GroupList::iterator it = old_groups.begin(), end = old_groups.end();
  for (unsigned st_ind = 1; it != end; ++it, ++st_ind) {
    // выбираем представителя данной группы
    unsigned st = it->Reset();

    // добавляем новое состояние соответствующее данной группе эквивалентности
    new_dfa.AddState(st_ind);

    for (unsigned gr_st = st; not it->End(); gr_st = it->Next()) {
      // делаем стартовым, если группа этого состояния содержит стартовое
      if (dfa_.start_state_ == gr_st) {
        new_dfa.SetStartState(st_ind);
      }

      // делаем допускающим, если группа этого состояния содержит допускающее
      Dfa::StateSet::iterator fnd = dfa_.accept_states_.find(gr_st);
      if (fnd != dfa_.accept_states_.end()) {
        new_dfa.AddToAcceptSet(st_ind);
      }
    }

    // добавляем переходы...

    // проходим по каждому символу
    for (unsigned ch_ind = 0; ch_ind < (1 << sizeof(char)* 8); ++ch_ind) {
      // st_to_ это состояние, куда будет произведен переход по данному символу
      unsigned st_to = dfa_.transitions_[st].row_[ch_ind];

      // находим группу, содержащую данное состояние
      if (st_to == 0) {
        continue;
      }

      GroupList::iterator int_it = old_groups.begin();
      for (unsigned st_ind_to = 1; int_it != end; ++int_it, ++st_ind_to) {
        if (int_it->IsState(st_to)) {
            // и добавляем переход по данному символу в результирующий ДКА
            new_dfa.AddTransition(st_ind, ch_ind, st_ind_to);
            break;
        }
      }
    }
  }

  // устанавливаем новый автомат на место старого
  dfa_ = new_dfa;
}

// разбивает данную группу состояний на несколько в соответствии с логикой алгоритма
void Minimization::DivideGroup(const StateGroup& gr, GroupList& new_sg, GroupList& old_sg) {
  // запоминаем индекс откуда начинаются новые, добавленные здесь, группы состояний
  unsigned int new_dev_index = new_sg.size();

  // первое состояние образует первую новую подгруппу
  unsigned st = gr.Reset();
  StateGroup first;
  first.AddState(st);
  new_sg.push_back(first);

  // проходим по всем состояниям в данной группе...
  for (; not gr.End(); st = gr.Next()) {
    bool state_added = false;

    // и исследуем его ортогональность с подгруппами нашей группы
    for (unsigned ind = new_dev_index; ind < new_sg.size(); ++ind) {
      // если текщее состояние и группа не ортогональны, добавляем состояние в эту группу
      StateGroup& tmp_gr = new_sg[ind];
      if (IsEqual(st, tmp_gr.Reset(), old_sg)) {
          tmp_gr.AddState(st);
          state_added = true;
          break;
      }
    }

    // если состояние ортогонально со всеми группами, создаем новую группу для данного состояния
    if (not state_added) {
      StateGroup tmp;
      tmp.AddState(st);
      new_sg.push_back(tmp);
    }
  }
}

bool Minimization::IsEqual(unsigned left, unsigned right, GroupList& old_sg) {
  if (left == right) {
    return true;
  }

  // проходим по каждому символу
  for (unsigned char_ind = 0; char_ind < (1 << sizeof(char)*8); ++char_ind) {
    unsigned st1_to = dfa_.transitions_[left].row_[char_ind];
    unsigned st2_to = dfa_.transitions_[right].row_[char_ind];

    // если состояния равны, нет смысла искать смежные группы
    if (st1_to == st2_to) {
      continue;
    }

    // если одно из состояний 0, а другое нет, то состояния ортогональны
    if (st1_to == 0 or st2_to == 0) {
      return false;
    }

    // находим группу, содержащую оба данных состояния
    bool contain = false;
    GroupList::iterator it = old_sg.begin(), end = old_sg.end();
    for (unsigned st_ind = 1; it != end; ++it, ++st_ind) {
      if (it->IsState(st1_to) and it->IsState(st2_to)) {
        contain = true;
        break;
      }
    }

    if (not contain) {
      return false;
    }
  }

  return true;
}

void Minimization::PrimaryDivide(GroupList& sg) {
  StateGroup first, second;
  for (unsigned ind = 1; ind < dfa_.transitions_.size(); ++ind) {
    Dfa::StateSet::iterator it = dfa_.accept_states_.find(ind);
    if (it != dfa_.accept_states_.end()) {
      second.AddState(ind);
    } else {
      first.AddState(ind);
    }
  }

  if (first.Size()) {
    sg.push_back(first);
  }
  sg.push_back(second);
}
