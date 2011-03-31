#include <set>
#include <map>
#include <vector>
#include <string>

class token
{
public:
  token() {start = -1; finish = -1;};
  token(int t, int s, int f, std::string st, bool ret, int term_sym_id = 0);
  token(token* t);
  int type;
  int start;
  int finish;
  bool is_returned;
  int terminal_symbol_id;
  std::string id;
  std::string str;
  std::set<token*> children;
  std::string print(int level, std::map <unsigned int, std::string> types);
  std::string print_gv(std::map <unsigned int, std::string> types, std::string par = "");
  std::string print_xml(std::map <unsigned int, std::string> types);

};

class token_tree
{
public:
  token* root;
  token_tree();
  void add_node(token* node);
};