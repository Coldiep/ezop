/*!
 * \file Реализация системы управления онтологиями.
 * \author Лапшин В.А.
 * \date 12.05.2011.
 */

#pragma once

#include <boost/shared_ptr.hpp>

#include <map>

#include <terms/type.h>

namespace ezop { namespace onto {

/*!
 * \brief Реализация системы управления онтологиями.
 *
 *
 */
class OntoSystem {
  /*!
   * \brief Класс, реализующий онтологию.
   */
  struct Ontology {
      ezop::terms::TypeSet type_set_;
  };

  typedef boost::shared_ptr<Ontology> OntoPtr;
  typedef std::map<std::string, OntoPtr> OntoList;

};

}} // namespace ezop, onto


