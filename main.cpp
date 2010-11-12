
#define PRINT_ADDING

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
    /*PublicGrammar gr( "c" );
    c_grammar::init_grammar(&gr);
    Grammar grr(&gr);

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
*/
    PublicGrammar pg("test");
    pg.AddTerminal(1, "N");
    pg.AddTerminal(2, "+");
    pg.AddTerminal(3, "*");

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

    pg.AddRule(3, "M --> N * M");
    pg.AddLhsSymbol(3, 5);
    pg.AddRhsSymbol(3, 1);
    pg.AddRhsSymbol(3, 3);
    pg.AddRhsSymbol(3, 5);

    pg.AddRule(4, "M --> N");
    pg.AddLhsSymbol(4, 5);
    pg.AddRhsSymbol(4, 1);

    pg.SetStartSymbolId(4);

    pg.Print(std::cout);

    std::cerr << "1\n";
    Grammar gr(&pg);
    std::cerr << "2\n";

    struct TestLexer : public parser::lexer {
      std::vector<unsigned> tokens_;
      unsigned cur_tok_index_;

      TestLexer()
        : cur_tok_index_(0)
      {}

      // return the current token
      token get_token() {
        if (cur_tok_index_ < tokens_.size()) {
            return token(tokens_[cur_tok_index_++]);
        }

        return token();
      }

      // is th end of input?
      bool is_end() {
          return cur_tok_index_ >=  tokens_.size();
      }
    };

    TestLexer lexer;
    lexer.tokens_.push_back(1);
    lexer.tokens_.push_back(2);
    lexer.tokens_.push_back(3);

    excel_semantics es;
    earley_parser parser(&gr, &lexer, &es);

    if (parser.parse()) {
      parser.print_trees( std::cout );
    } else {
      std::cout << "there is an error during parsing.";
    }
  } catch (std::exception& err) {
    std::cout << err.what() << std::endl;
  }

  return 0;
}
