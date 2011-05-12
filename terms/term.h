
#pragma once

#include <boost/shared_ptr.hpp>
#include <stdexcept>

#include <terms/signature.h>

namespace ezop { namespace terms {

/// Класс, реализующий терм.
struct Term {
  //! Тип умного указателя на терм.
  typedef boost::shared_ptr<Term> Ptr;

  //! Тип списка параметров терма.
  typedef std::vector<Term::Ptr> TermList;

  Operation::Ptr signature_;  ///< Сигнатура терма.
  TermList       children_;   ///< Список дочерних элементов.

  explicit Term(Operation::Ptr signature)
    : signature_(signature)
    , children(signature_->params_.size()) {
  }

  void SetParam(Term::ptr term, unsigned place) {
    if (place >= children_.size()) {
      throw std::invalid_argument("Bad index of term passed.");
    }

    if (term->signature_->res_type != signature_->params_[place]) {
      throw std::invalid_argument("The term's type does not the same as type into term signature.");
    }

    children_[place].reset(term);
  }

  void PrintTerm(std::ostream& out, const Term* term) const {
    out << term->signature_->name_ << "(";
    bool f = true;
    for (unsigned i = 0; i < children_.size(); ++i) {
      if (f) {
        f = false;
      } else {
        out << ", ";
      }
      if (children_[i].get()) {
        PrintTerm(out, children_[i].get());
      } else {
        out << "Null";
      }
    }
    out << ")";
  }

  void Print(std::ostream& out) const {
    PrintTerm(out, this);
  }
};

}} // namespace ezop, terms.

