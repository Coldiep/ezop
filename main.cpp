
#include "public_grammar.h"
#include "grammar.h"
#include "lexer.h"
#include "earley_parser.h"
#include "allocator.h"
#include "excel_semantics.h"
using namespace parser;

#include <exception>
#include <iostream>

#include "c_grammar.h"
using namespace c_grammar;

#include <time.h>

int main() {
  try {
    PublicGrammar gr( "c" );
    c_grammar::init_grammar(&gr);

    grammar grr;
    grr.initialize(&gr);

    c_grammar::lexer lex("./tt.c");
    excel_semantics es;
    earley_parser parser(&grr, &lex, &es);

    int time1 = time(0);
    bool res = parser.parse();
    int time2 = time(0);
    if (res) {
      std::cout << "parse well done! Time is " << (time2-time1) << std::endl;
      //_parser.print_trees( std::cout );
    } else {
            std::cout << "there is an error during parsing. Line num: " << lex.get_line_num()
                      << ", position: " << lex.get_pos() << std::endl;
    }
  } catch (std::exception& err) {
    std::cout << err.what() << std::endl;
  }

  return 0;
}
