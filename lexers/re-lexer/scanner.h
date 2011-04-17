#include <stdio.h>
#include <stack>
#include "exp_tree.h"

namespace relexer {

//! Определение класса сканера.
class Scanner
{
public:
  //! Обработка части регулярного выражения,заключенной в скобки.
  ExpTree* do_brackets(Position& p);
  //! Обработка регулярного выражения.
  ExpTree* process(std::string str);
};

}