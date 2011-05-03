
#pragma once

#include <rex/dfa.h>

namespace rexp {

class Minimization {
  /*!
   * Группа состояний ДКА. Данная структура используется для хранения групп состояний ДКА.
   */
  class StateGroup {
    //! Состояния.
    mutable std::set<unsigned> state_set_;

    //! Использется для итерации по множеству состояний.
    mutable std::set<unsigned>::iterator it_;

  public:
    //! Установить итератор в начальное состояние.
    unsigned Reset() const {
      it_ = state_set_.begin();
      return *it_;
    }

    //! Перейти к следующему состоянию в группе.
    unsigned Next() const {
      ++ it_;
      return End() ? 0 : *it_;
    }

    //! Достигнут ли конец списка состояний в группе.
    bool End() const {
      return it_ == state_set_.end();
    }

    //! Добавить состояние в группу.
    void AddState(unsigned st) {
      state_set_.insert(st);
    }

    //! Удалить состояние из группы.
    void RemoveState(unsigned st) {
        std::set<unsigned>::iterator it = state_set_.find(st);
        if (it != state_set_.end()) {
          state_set_.erase(it);
        }
    }

    //! Есть ли данное состояние в группе.
    bool IsState(unsigned st) {
      std::set<unsigned>::iterator it = state_set_.find(st);
      if (it != state_set_.end()) {
        return true;
      }
      return false;
    }

    bool operator==(const StateGroup& gr) const {
      return state_set_ == gr.state_set_;
    }

    unsigned Size() const {
      return state_set_.size();
    }

    void Print() {
        std::cout << "********************************\n";
        std::set<unsigned>::iterator it = state_set_.begin(), end = state_set_.end();
        for (std::set<unsigned>::iterator it = state_set_.begin(), end = state_set_.end(); it != end; ++it) {
          std::cout << (*it) << " ";
        }
        std::cout << "\n********************************\n";
    }
  };

  typedef std::vector<StateGroup> GroupList;

public:
  //! Конструктор берет на вход ДКА.
  explicit Minimization(Dfa& dfa)
    : dfa_(dfa) {
  }

  /*!
   * Минимизирует ДКА, передаваемый как параметр конструктора. Минимизация проводится относительно
   * количества состояний. Результирующий ДКА допускает тот же язык, что и данный.
   */
  void Minimize();

private:
  //! ДКА для минимизации.
  Dfa& dfa_;

  //! Вектор групп состояинй для данного ДКА.
  GroupList groups_;

  //! Разбивает данную группу состояний на несколько в соответствии с логикой алгоритма.
  void DivideGroup(const StateGroup& gr, GroupList& new_sg, GroupList& old_sg);

  //! Определяет ортогональность двух состояний.
  bool IsEqual(unsigned left, unsigned right, GroupList& old_sg);

  /*!
   * проводит начальное разбиение ДКА на две группы. В первой - заключительные состояния.
   * Во второй - все оставшиеся
   */
  void PrimaryDivide(GroupList& sg);
};

} // namespace rexp

