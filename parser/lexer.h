#include <list>
#include <vector>
#include "scanner.h"
#include "dfa.h"
#include "token.h"
#include "public_grammar.h"

namespace parser {

  struct lex_type
  {
    int id;
    std::string name;
    std::string regexp;
    dfa* d;
    bool valid;
    bool is_returned;
    int priority;
  };

  class lexer
  {
  public:
    lexer();
    lexer(const PublicGrammar* public_grammar);
    ~lexer();
    std::string stream;
    std::set <lex_type*> lex_types;
    int types_no;
    token_tree* tree;
    int cur_pos;

    void add_type(std::string name, std::string regexp,int priority, bool ret = true, int id = -1);
    void set_stream(std::string s);
    void set_stream_file(std::string f, std::string tail);
    void analyze();
    std::set <token*> get_tokens(token* tok);
    std::set <token*> get_tokens(int pos);
    void reset();
    std::string strip(std::string str);
    bool IsEnd();
  };


}
