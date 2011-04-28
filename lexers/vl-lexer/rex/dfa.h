
#pragma once

#include <stdint.h>

#include <vector>
#include <set>
#include <iostream>

namespace rexp {

/*!
 * \brief Реализация класса детерминированного конечного автомата.
 */
class Dfa {
public:
  // Используется для обозначения недоступного состояния.
  static const unsigned ZERO_STATE = 0;

  //! Размер таблицы переходов.
  static const unsigned TABLE_SIZE = 256;

  //! Тип множества состояний.
  typedef std::set<unsigned> StateSet;

private:
  // элемент таблицы состояний
  struct TableRow {
    // конструктор
    TableRow()
      : row_(TABLE_SIZE, ZERO_STATE) {
    }

    //! Строка таблицы переходов.
    std::vector<unsigned> row_;

    //! Добавление перехода.
    void AddMove(uint8_t symbol, unsigned state_to) {
      row_[symbol] = state_to;
    }

    //! Реализация перехода.
    unsigned Move(uint8_t symbol) const {
      return row_[symbol];
    }

    // Печать таблицы переходов.
    void Print() const;
  };

  //! Тип таблицы переходов.
  typedef std::vector<TableRow> TransTable;

  unsigned    start_state_;    //!< Индекс начального состояния.
  StateSet    accept_states_;  //!< Множество допускающих состояний.
  TransTable  transitions_;    //!< Таблица переходов.

  // Минимизация автомата.
  friend class Minimization;

  // Сравнение автоматов на равенство.
  friend class DfaEqualCheck;

public:
  // Конструктор по умолчанию.
  Dfa()
    : start_state_(ZERO_STATE)
    , transitions_(1) {
  }

  //! Добавляет новое состояние в ДКА.
  void AddState(unsigned state) {
    if (state > transitions_.size() - 1) {
      transitions_.resize(state + 1);
    }
  }

  /*!
   * \brief Добавляет новый переход в ДКА.
   *
   * \param state_from  Состояние, откуда начинается переход.
   * \param symbol      Символ, по которому производится переход.
   * \param state_to    Состояние, в которое производится переход.
   */
  void AddTransition(unsigned state_from, uint8_t symbol, unsigned state_to);

  //! Возвращает номер начального состояния.
  unsigned GetStartState() const {
    return start_state_;
  }

  //! Возвращает множество допускающих состояний.
  const StateSet& GetAcceptStates() const {
    return accept_states_;
  }

  //! Устанавливает индекс начального состояния.
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

  //! Производит переход.
  unsigned Move(unsigned state, uint8_t symbol) const {
    return transitions_[state].Move(symbol);
  }

  //! Печатает автомат на консоль.
  void Print() const;
};

}  // namespace rexp

