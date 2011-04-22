
#pragma once

#include <dfa.h>

#include <boost/shared_ptr.hpp>
#include <string>

namespace lexer {

/*!
 * Класс есть абстракция типа лексемы. Каждая лексема имеет уникальное число-идентификатор,
 * регулярное выражение, описывающее набор символов в данной лексеме и символическое
 * имя. Например: лексема шестнадцатиричное число есть кортеж {ID, "0(x|X)[0-9]+", "hexadecimal"}
 */
class LexType {
  //! Генерирует ДКА для регулярного выражения данного типа.
  void GenerateDfa();

  unsigned    id_;        //!< Идентификатор лексического типа.
  std::string re_;        //!< Регулярное выражение для данной типа.
  std::string name_;      //!< Имя лексемы.
  rexp::Dfa   dfa_;       //!< ДКА построенный по регулярному выражению.
  unsigned    cur_state_; //!< Текущее состояние ДКА данной лексемы.
  bool        ret_;       //!< Возвращается ли лексема лексическим анализатором?

public:
  //! Тип умного указателя на лексический тип.
  typedef boost::shared_ptr<LexType> Ptr;

  /*!
   * \brief Конструктор для слова.
   *
   * \param id    Идентификатор типа.
   * \param word  Последовательность символов.
   */
  LexType(unsigned id, const std::string& word);

  //! Необходим для оптимального поиска по множеству лексем.
  explicit LexType(unsigned id);
    : id_(id)
    , re_("")
    , name_("only for a set search")
    , cur_state_(0)
    , ret_(false) {
  }

  /*!
   * \brief Конструктор для регулярного выражения.
   *
   * \param id    Идентификатор типа.
   * \param re    Регулярное выражение.
   * \param name  Имя типа.
   * \param ret   Пробельный тип или нет (returned).
   */
  LexType(unsigned id, const std::string& re, const std::string& name, bool ret);

  // Возврат значений полей класса...

  unsigned GetId() const {
    return id_;
  }

  const std::string& GetRe() const {
    return re_;
  }

  const std::string& GetName() const {
    return name_;
  }

  rexp::Dfa& GetDfa() {
    return dfa_;
  }

  /*!
   * \brief Переход в новое состояние по переданному символу.
   *
   * \param symbol  Символ для перехода.
   * \retuen        ложь, если новое состояние недопустимо.
   */
  bool Move(char symbol) const;

  //! Устанавливает текущее состояние на начальное.
  void Reset() {
    cur_state_ = dfa_.GetStartState();
  }

  //! Является ли текущее состояние заключительным?
  bool IsAccepted() const {
    return dfa_.GetAcceptStates().find(cur_state_) != dfa_.GetAcceptStates().end();
  }

  //! Возвращается ли лексема лексическим анализатором?
  bool IsSpace() const {
    return not ret_;
  }
};

} // namespace lexer

