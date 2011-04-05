#include "error_thrower.h"
#include "open_token.h"

token::token(int t, int s, int f, std::string st, bool ret, int term_sym_id)
{
  type = t;
  start = s;
  finish = f;
  str = st;
  is_returned = ret;
  terminal_symbol_id = term_sym_id;
}
token::token(token* t)
{
  type = t->type;
  start = t->start;
  finish = t->finish;
  str = t->str;
  is_returned = t->is_returned;
  this->children = t->children;
  this->id = t->id;
}

token_tree::token_tree()
{
  this->root = new token(0,-1,-1,"",true);
}

void token_tree::add_node(token* node)
{
  std::vector <token*> nodes2see;
  nodes2see.push_back(root);
  int len = 1;
  int uses = 0;
  for (int i = 0; i<len; i++)
  {
    if(nodes2see[i]->finish == node->start - 1)
    {
      for (int j = 0; j < node->id.length(); j++)
        if (node->id[j] == '.')
          node->id.erase(j,std::string::npos);
      char integer[10] = "";
      itoa(++uses,integer,10);
      node->id.append(".").append(integer);

      nodes2see[i]->children.insert(new token(node));
    }
  for (std::set<token*>::iterator it = nodes2see[i]->children.begin(); it!=nodes2see[i]->children.end(); ++it)
  {
      nodes2see.push_back(*it);
      len++;
    }
  }
}
