#define PRINT_ADDING


#include "public_grammar.h"
#include "grammar.h"
#include "earley_parser.h"
#include "allocator.h"
using namespace parser;

#include <exception>
#include <iostream>

#include <time.h>

int main() {
  try {
    
    PublicGrammar pg("test");
    pg.AddTerminal(1, "N");
    pg.AddTerminal(2, "+", "\\+");
    pg.AddTerminal(3, "x");

    pg.AddNonterminal(4, "A");
    pg.AddNonterminal(5, "M");

    pg.AddRule(1, "A --> M + A");
    pg.AddLhsSymbol(1, 4);
    pg.AddRhsSymbol(1, 5);
    pg.AddRhsSymbol(1, 2);
    pg.AddRhsSymbol(1, 4);

    pg.AddRule(2, "A --> M");
    pg.AddLhsSymbol(2, 4);
    pg.AddRhsSymbol(2, 5);

    pg.AddRule(3, "M --> N x M");
    pg.AddLhsSymbol(3, 5);
    pg.AddRhsSymbol(3, 1);
    pg.AddRhsSymbol(3, 3);
    pg.AddRhsSymbol(3, 5);

    pg.AddRule(4, "M --> N");
    pg.AddLhsSymbol(4, 5);
    pg.AddRhsSymbol(4, 1);

    pg.SetStartSymbolId(4);
  

    pg.Print(std::cout);
    std::cout << "\n\n";

    Grammar gr(&pg);

    lexer l(&pg);  
    l.set_stream("N+N");
  

    EarleyParser parser(&gr, &l);

    int time1 = time(0);
    if (parser.Parse()) {
        std::cout << "parse successful\n";
    int time2 = time(0);
    std::cout << "Time is " << (time2-time1) << "\n";
    } else {
      std::cout << "there is an error during parsing.\n";
    }
  } catch (std::exception& err) {
    std::cout << err.what() << std::endl;
  }

  return 0;
}