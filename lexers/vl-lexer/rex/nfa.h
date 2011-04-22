
#pragma once

#include <set>
#include <map>
#include <stack>

namespace rexp {

// недетерминированный конечный автомат
class Nfa {
public:
  // typedefs...
  typedef std::set<unsigned>                            StateSet;
  typedef std::stack<unsigned, std::vector<unsigned> >  StateStack;
  typedef std::set<char>                                SymbolSet;
  typedef std::map<char, StateSet>                      CharMap;

private:
  // недопустимое состояние
  enum {
    ZERO_STATE = 0
  };

  // элемент таблицы символов
  struct TableRow {
    // конструктор с номер состояния
    TableRow(unsigned state)
      : state_(state) {
    }

    // конструктор с номером состояния и множеством символов
    TableRow(unsigned state, const CharMap& char_map)
      : state_(state)
      , char_map_(char_map) {
    }

    // печатает элемент таблицы состояний ни консоль
    void Print() const;

    // состояние
    unsigned state_;

    // множество символов
    CharMap char_map_;

    bool operator==(unsigned state) const {
      return state_ == state;
    }
  };

  //! Тип таблицы переходов.
  typedef std::map<unsigned, TableRow> StateTable;

  //! Таблица переходов.
  StateTable transitions_;

  //! Номер начального состояния.
  unsigned start_state_;

  //! Множество допускающих состояний.
  StateSet accept_states_;

public:
  // конструктор
  Nfa();
    : start_state_(ZERO_STATE) {
  }

  //! Добавление нового состояния.
  void AddState(unsigned state) {
    transitions_.insert(state);
  }

  //! Добавление перехода.
  void AddTransition(unsigned state_from, char symbol, unsigned state_to);

  //! Возвращает начальное состояние.
  unsigned GetStartState() const {
    return start_state_;
  }

  //! Возвращает множество допустимых состояний.
  const StateSet& GetAcceptStates() const {
    return acept_states_;
  }

  //! Устанавливает начальное состояние.
  void SetStartState(unsigned state) {
    start_state_ = state;
  }

  //! Добавляет состояние ко множеству допускающих.
  void AddToAcceptSet(unsigned state) {
    accept_states_.insert(state);
  }

  //! Удаляет состояние из множества допускающих.
  void RemoveFromAcceptSet(unsigned state) {
    accept_states_.erase(state);
  }

  // методы, необходимые для трансформации из НКА в ДКА...

  //! Эпсилон замыкание состояния state.
  StateSet EpsilonClosure(unsigned state) const {
    StateSet res;
    res.insert(state);
    return EpsilonClosure(res);
  }

  //! Эпсилон замыкание множества состояний.
  StateSet EpsilonClosure(const StateSet& states);

  //! Возвращает множество состояний имеющих переходы из state по символу symbol.
  StateSet Move(unsigned state, char symbol) const;

  //! Возвращает множество состояни имеющих переходы из states по символу symbol.
  StateSet Move(const StateSet& states, char symbol) const;

  //! Возвращает множество символов данного НКА.
  SymbolSet GetSymSet() const;

  //! Клонирует данный НКА, добавляя смещение к каждому состоянию.
  void CloneFromOffset(Nfa& new_nfa, unsigned offset) const;

  //! Возвращает максимальный номер состояния.
  unsigned GetMaxStateNum() const;

  //! Удаляет все допускающие состояния.
  void CleanAcceptStates() {
    accept_states_.clear();
  }

  //! Печатает состояния НКА на консоль.
  void Print() const;
};

}

