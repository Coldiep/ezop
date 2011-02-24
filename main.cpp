
#define PRINT_ADDING

#include "public_grammar.h"
#include "grammar.h"
#include "lexer.h"
#include "earley_parser.h"
#include "allocator.h"
using namespace parser;

#include <exception>
#include <iostream>

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

    struct TestLexer : public parser::Lexer {
      std::vector<Grammar::SymbolId> tokens_;
      Grammar::SymbolId cur_tok_index_;

      TestLexer()
        : cur_tok_index_(0)
      {}

      TokenList GetTokens(Token::Ptr token) {
        if (cur_tok_index_ < tokens_.size()) {
          TokenList tokens;
          tokens.push_back(Token::Ptr(new Token(tokens_[cur_tok_index_++])));
          return tokens;
        }

        return TokenList();
      }

      // is th end of input?
      bool IsEnd() {
        return cur_tok_index_ >= tokens_.size();
      }
    };

    TestLexer lexer;
    lexer.tokens_.push_back(1);
    lexer.tokens_.push_back(2);
    lexer.tokens_.push_back(1);

    EarleyParser parser(&gr, &lexer);

    if (parser.Parse()) {
        std::cout << "parse successful\n";
    } else {
      std::cout << "there is an error during parsing.\n";
    }
  } catch (std::exception& err) {
    std::cout << err.what() << std::endl;
  }

  return 0;
}
