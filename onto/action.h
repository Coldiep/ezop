/*!
* \file Определение действия операции.
 * \author Лапшин В.А.
 * \date 12.05.2011.
 */

#pragma once

#include <boost/shared_ptr.hpp>

#include <terms/term.h>

namespace ezop { namespace onto {

/*!
 * \brief Действие шаблона на некоторой операции.
 */
struct Action {
  //! Результат вычисления терма.
  struct Result {
    typedef boost::shared_ptr<Result> Ptr;
    virtual ~Result() {
    }
  };

  typedef boost::shared_ptr<Action> Ptr;

  /**
   * \brief Вычисление терма.
   *
   * \param term Терм для вычисления.
   * \result     Результат вычисления.
   */
  virtual Result::Ptr Calc(ezop::terms::Term::Ptr term) = 0;

  virtual ~Action() {
  }
};

}} // namespace ezop, onto

