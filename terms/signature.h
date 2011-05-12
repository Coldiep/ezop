
#pragma once

#include <boost/shared_ptr.hpp>
#include <vector>

#include <terms/type.h>

namespace ezop { namespace terms {

/**
 * \brief Реализация класса операции алгебры.
 *
 * Операция -- это пара (r, s*), где:
 *   r  -- тип результата.
 *   s* -- список (может быть пустой) типов параметров.
 */
struct Operation {
  /// Тип умного указателя на операцию.
  typedef boost::shared_ptr<Operation> Ptr;

  /// Тип списка имен типов.
  typedef std::vector<std::string>  TypeNameList;

  std::string   name_;      ///< Имя Сигнатуры.
  std::string   res_type_;  ///< Тип результата.
  TypeNameList  params_;    ///< Типы параметров.

  Operation(const std::string& name, const std::string& res_type)
    : name_(name)
    , res_type(res_type) {
  }

  Operation(const std::string& name, const std::string& res_type, const TypeNameList& params)
    : name_(name)
    , res_type_(res_type)
    , params_(params) {
  }

  void Print(std::ostream& out) {
    out << res_type_ << " " << name_ << "(";
    bool f = true;
    for (unsigned i = 0; i < params_.size(); ++i) {
      if (f) {
        f = false;
      } else {
        out << ", ";
      }
      out << params_[i];
    }
    out << ")\n";
  }
};

}} // namespace ezop, terms.

