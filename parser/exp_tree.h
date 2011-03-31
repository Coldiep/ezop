#include <stack>
#include <sstream>
#include <set>

enum point_type {symbol=0,opUnion, opIter,opConcat,OpLeftBracket,OpRightBracket,empty};

class tree_point
{
public:
  int id;
  tree_point* left;
  tree_point* right;
  tree_point* parent;
  point_type type;
  char contents;
  bool nullable;
  std::set <tree_point*> firstpos;
  std::set <tree_point*> lastpos;
  std::set <tree_point*> followpos;
  tree_point();
  tree_point(char c);
  tree_point(unsigned int c1,unsigned int c2);
  tree_point(tree_point*tp);
  void calc();
  void print(int n);
  int end;  
  ~tree_point();
};


class exp_tree
{
public:
  tree_point* root;
  std::string alphabet;
  std::set <tree_point*> leaves;
  exp_tree();
  exp_tree(exp_tree* t);
  exp_tree(tree_point pnt);
  exp_tree(unsigned int c1,unsigned int c2);
  exp_tree(char c);
  exp_tree(point_type t);
  bool is_empty();
  exp_tree* make_new_root(point_type t);
  static exp_tree* merge_trees(exp_tree* l,exp_tree* r,point_type t);
  void calc_followpos();
  ~exp_tree();


};
struct Position{
    unsigned int PosNumber;
    const char* PosPointer;
    
    Position (const char* posPointer)
    : PosPointer (posPointer)
    , PosNumber (1)
    {}
    
    void Next () {if (! IsEnd ()){++ PosPointer; ++ PosNumber;}}
    bool IsEnd () {return *PosPointer == 0;}
    unsigned int GetPos () {return PosNumber;}
    const char* LookForward (unsigned int dist) {return PosPointer+dist;}
    char operator * () {return *PosPointer;}
    Position& operator ++ () {Next (); return *this;}
    const char* operator + (unsigned int dist) {return LookForward (dist);}
};