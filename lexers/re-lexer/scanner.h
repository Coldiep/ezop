#include <stdio.h>
#include <stack>
#include "exp_tree.h"

//! ќпределение класса сканера.
class scanner
{
public:
  //! ќбработка части регул€рного выражени€, заключенной в скобки.
  exp_tree* do_brackets(Position& p);
  //! ќбработка регул€рного выражени€.
  exp_tree* process(std::string str);
};