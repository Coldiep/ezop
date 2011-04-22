
#pragma once

#include <rex/dfa.h>

namespace rexp {

// класс для проверки на равенство двух конечных минимизированных автоматов
struct DfaEqualCheck {
  static bool Check(const Dfa& left, const Dfa& right);
};

} // namespace rexp

