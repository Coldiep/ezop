#include <stdio.h>
#include <stack>
#include "exp_tree.h"


class scanner
{
public:
  exp_tree* do_brackets(Position& p);
  exp_tree* process(std::string str);
};