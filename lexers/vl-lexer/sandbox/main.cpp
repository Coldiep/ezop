
#include <lex.h>
#include <rex/scanner.h>
#include <rex/parser.h>
#include <parser/lexer.h>
#include <parser/token.h>

int main() {
  try {
#if 0
    std::string re = "[1-9][0-9]*";
    rexp::Parser parser(&re[0], &re[0]+re.length());
    parser.Print();
#endif

    lexer::Lexer lexer;
    lexer.AddLexType(1, "[1-9][0-9]*", "Integer", true);
    lexer.AddLexType(2, "\\+", "Add", true);
    lexer.AddLexType(3, "[:space:]+", "space", false);

    std::string str = "12+45 + 9";
    parser::Lexer::TokenList tok_list;
    tok_list.push_back(parser::Token::Ptr(new parser::Token()));
    for (lexer.SetInputStream(&str[0], &str[0] + str.length()); not lexer.IsEnd();) {
      parser::Lexer::TokenList new_tok_list;
      for (parser::Lexer::TokenList::iterator it = tok_list.begin(); it != tok_list.end(); ++it) {
        parser::Lexer::TokenList tl = lexer.GetTokens(*it);
        new_tok_list.insert(new_tok_list.end(), tl.begin(), tl.end());
      }

      if (new_tok_list.empty()) {
        break;
      }

      for (parser::Lexer::TokenList::iterator it = new_tok_list.begin(); it != new_tok_list.end(); ++it) {
        std::cout << "(" << (*it)->abs_pos_ << ";" << (*it)->text_ << ")\n";
      }
      tok_list.swap(new_tok_list);
    }
#if 0
    std::string re = "[1-9][0-9]*";
    rexp::Scanner scanner(&re[0], &re[0]+re.length());
    for (;;) {
      rexp::Scanner::Token::Ptr token = scanner.GetToken();
      if (token->Type() == rexp::Scanner::END) {
        break;
      }

      std::cout << "(" << token->Type() << "; " << token->Data() << ")\n";
    }
#endif
  } catch(const std::exception& err) {
    std::cerr << err.what() << "\n";
  }

  return 0;
}

