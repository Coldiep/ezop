#pragma warning(disable: 4715)
#include "exp_tree.h"
#include "dfa.h"
#include "error_thrower.h"

int s_id=0;
state::state()
{
  marked=false;
}
unsigned int state::get_transition(char c)
{
  std::map<char, unsigned int>::iterator it = transitions.find(c);
  if(it != transitions.end()) 
    return (*it).second;
  return 0;

}
void state::add_transition(char c,unsigned int state)
{
  transitions[c] = state;
}

//******************************************************************************************
dfa::dfa(){}
void dfa::add_state(state* s)
{
  if (!(states.empty()))
  for (std::set <state*>::iterator it = states.begin(); it != states.end(); ++it)
      if((*it)->id==s->id)
        return;
  states.insert(s);
}
void dfa::add_state(int id,state_type type)
{
  if (!(states.empty()))
    for (std::set <state*>::iterator it = states.begin(); it != states.end(); ++it)
      if((*it)->id==id)
        throw_error("State with this id already exists: ",id);
  state* s = new state();
  s->id=id;
  s->type=type;
  states.insert(s);
}
state* dfa::make_state(std::set<tree_point*> leaves_set,state_type type)
{
  state* s = new state();
  s->type=type;
  for (std::set <tree_point*>::iterator it = leaves_set.begin(); it != leaves_set.end (); ++ it)
  {
    s->ids.insert((*it)->id);
    if ((*it)->contents=='#')
      (*it)->contents = (*it)->contents;
    if ((*it)->contents=='#' && (*it)->end == 1)
    {
      if(type==start)
        s->type=start_finish;
      else 
        s->type=finish;
    }
  }
  s->id=++s_id;

  for (std::set <state*>::iterator it = states.begin(); it != states.end (); ++ it)
  {
    if(s->ids.size()!=(*it)->ids.size())
      continue;
    bool next = false;
    for (std::set <int>::iterator itt = (*it)->ids.begin(); itt != (*it)->ids.end (); ++ itt)
    {
      if(s->ids.find(*itt)==s->ids.end())
      {
        next = true;
        break;
      }
    }
    if(next)
      continue;
    else
    {
      s->id=(*it)->id;
      return s;
    }
  }
  return s;
  
}
void dfa::add_transition(int beg_state,char c,int end_state)
{
  state* bs = get_state(beg_state);
  state* es = get_state(end_state);
  bs->add_transition(c,es->id);
}
state* dfa::get_state(int ii)
{
  for (std::set <state*>::iterator it = states.begin(); it != states.end(); ++it)
  {
  state* s = (*it);
    if(s->id==ii)
      return s;
  }
  throw_error("No state with id ",ii);
}
state* dfa::get_start_state()
{
  for (std::set <state*>::iterator it = states.begin(); it != states.end(); ++it)
  {
  state* s = (*it);
    if(s->type==start || s->type==start_finish)
      return s;
  }
  throw_error("No start state found!");
}
int dfa::process(std::string str)
{
  //printf("Processing string '%s' of length %i: ",str.c_str(),str.length());
  state* s=get_start_state();
  for(unsigned int i=0;i<str.length();i++)
  {
    unsigned int ind=s->get_transition(str[i]);
    if (ind==0)
    {
      s->type=mid;
      break;
    }
    s=get_state(ind);
  }
  if (s->type == finish || s->type == start_finish) 
    return 1;
  else 
    return 0;
}
struct tri
{
  tri(int idid, char aa, std::set <tree_point*> ss)
  {
    s=ss;
    a=aa;
    id=idid;
  }
  std::set <tree_point*> s;
  char a;
  int id;
};

void dfa::build(exp_tree* tr)
{
  s_id=0;
  bool flag=true;
  std::string input = tr->alphabet;
  std::set <tree_point*> leaves = tr->leaves;

  state* firststate = make_state(tr->root->firstpos,start);
  add_state(firststate);
  

  while(flag)
  {
    flag  = false;
    std::set<tri*> toadd;
    for (std::set <state*>::iterator it = states.begin(); it != states.end(); ++ it)
    {
      if((*it)->marked)
        continue;
      (*it)->marked=true;
      for(unsigned int i=0;i<input.length();i++)
      {
        char a = input[i];
        std::set <tree_point*> S;
    for (std::set <tree_point*>::iterator leaves_it = leaves.begin(); leaves_it != leaves.end(); ++leaves_it)
    {
      tree_point* leaf = (*leaves_it);
      for (std::set <int>::iterator iter = (*it)->ids.begin(); iter != (*it)->ids.end(); ++iter)
            if((*iter)==leaf->id && leaf->contents==a)
            {
              S.insert(leaf->followpos.begin(),leaf->followpos.end());
            }
    }
         if(!(S.empty()) )
        {
          flag = true;
          tri* tmp = new tri((*it)->id,a,S);
          toadd.insert(tmp);
        }
      }
    }
    for (std::set <tri*>::iterator it = toadd.begin(); it != toadd.end (); ++ it)
    {
      state* newstate  = make_state((*it)->s);
      add_state(newstate);
      add_transition((*it)->id,(*it)->a,newstate->id);
    }
    toadd.clear();

  }
  curr_state = get_start_state();
}

int dfa::move(char c)
{
  unsigned int ind = curr_state->get_transition(c);
  if (ind != 0)
  {
    curr_state = get_state(ind);
    return 1;
  }
  else
    return 0;

}
dfa::~dfa()
{
  for (std::set <state*>::iterator it = states.begin(); it != states.end(); ++it)
  delete (*it);
}
void dfa::build1(exp_tree* tr)
{
  s_id=0;
  bool flag=true;
  std::string input = tr->alphabet;
  std::set <tree_point*> leaves = tr->leaves;

  state* firststate = make_state(tr->root->firstpos,start);
  add_state(firststate);
  while(flag)
  {
    for (std::set <state*>::iterator it = states.begin(); it != states.end(); ++it)
    {
      state* t = (*it);
      if(t->marked)
        continue;
      t->marked=true;
      for(unsigned int i=0;i<input.length();i++)
      {
        char a = input[i];
        std::set <tree_point*> S;
    for (std::set <tree_point*>::iterator leaves_it = leaves.begin(); leaves_it != leaves.end(); ++leaves_it)
    {
      tree_point* leaf = (*leaves_it);
      for (std::set <int>::iterator ids_it = t->ids.begin(); ids_it != t->ids.end(); ++ids_it)  
            if((*ids_it)==leaf->id && leaf->contents==a)
            {
              S.insert(leaf->followpos.begin(),leaf->followpos.end());
            }
    }
        if(!(S.empty()) )
        {
          state* newstate  = make_state(S);
          add_state(newstate);
          add_transition(t->id,a,newstate->id);
        }
      }
    }
    flag=false;
    for (std::set <state*>::iterator it = states.begin(); it != states.end(); ++it)
      if((*it)->marked==false)
      {
        flag=true;
        break;
      }
  }
}