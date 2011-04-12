#include <stdio.h>
#include <stack>
#include "exp_tree.h"

//! Определение класса сканера.
class scanner
{
public:
  //! Обработка части регулярного выражения, заключенной в скобки.
  exp_tree* do_brackets(Position& p);
  //! Обработка регулярного выражения.
  exp_tree* process(std::string str);
};