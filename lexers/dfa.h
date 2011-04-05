#include <vector>
#include <map>
#include <set>

enum state_type {start, mid, finish, start_finish};
class state
{
public:
  int id;
  bool marked;
  std::map<char, unsigned int> transitions;
  std::set <int> ids;
  state_type type;

  state();
  unsigned int get_transition(char c);
  void add_transition(char c,unsigned int s);
};


class dfa
{
public:
  std::set <state*> states;
  state* curr_state;

  dfa();
  ~dfa();
  void add_state(state* s);
  void add_state(int id,state_type type=mid);
  state* make_state(std::set<tree_point*> leaves_set,state_type type=mid);
  void add_transition(int beg_state,char c,int end_state);
  state* get_state(int ii);
  state* get_start_state();
  int process(std::string str);
  void build(exp_tree* t);
  void build1(exp_tree* t);
  int move(char c);
};