#pragma warning(disable: 4715)
#include "exp_tree.h"
#include "dfa.h"
#include "error_thrower.h"

int s_id=0;

/*!
* \brief Конструктор по умолчанию.
*
*/
state::state()
{
  marked=false;
}

/*!
* \brief Получение списка возможных переходов из данного состояния по определенному символу.
*
* \param[in] c  Символ, по которому должен осуществляться переход.
*
*/
unsigned int state::get_transition(char c)
{
  std::map<char, unsigned int>::iterator it = transitions.find(c);
  if(it != transitions.end()) 
    return (*it).second;
  return 0;

}

/*!
* \brief Добавление перехода.
*
* \param[in] c    Символ, по которому осуществляется переход.
* \param[in] state  Идентификатор состояния, в которое осуществляется переход.
*
*/
void state::add_transition(char c,unsigned int state)
{
  transitions[c] = state;
}

//******************************************************************************************
/*!
* \brief Конструктор по умолчанию.
*
*/
dfa::dfa(){}


/*!
* \brief Добавление состояния.
*
* \param[in] state  Состояние, которое следует добавить.
*
*/
void dfa::add_state(state* s)
{
  if (!(states.empty()))
  for (std::set <state*>::iterator it = states.begin(); it != states.end(); ++it)
      if((*it)->id==s->id)
        return;
  states.insert(s);
}

/*!
* \brief Добавление состояния.
*
* \param[in] id    Идентификатор состояния, которое следует добавить.
* \param[in] type  Тип состояния, которое следует добавить.
*
*/
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


/*!
* \brief Создние нового состояния.
*
* \param[in] leaves_set    Множество вершин дерева регулярного выражения, которым будет соответствовать состояние.
* \param[in] type      Тип состояния.
* \return          Новое состояние.
*/
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

/*!
* \brief Добавление перехода.
*
* \param[in] beg_state  Состояние, из которого будет осуществляться переход.
* \param[in] c      Символ, по которому будет осуществляться переход.
* \param[in] beg_state  Состояние, в которое будет осуществляться переход
* 
*/
void dfa::add_transition(int beg_state,char c,int end_state)
{
  state* bs = get_state(beg_state);
  state* es = get_state(end_state);
  bs->add_transition(c,es->id);
}

/*!
* \brief Получение состояния по идентификатору.
*
* \param[in] ii    Идентификатор состояния.
* \return      Состояние ДКА с указанным идентификатором.
*/
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

/*!
* \brief Получение начального состояния ДКА.
*
* \return  Начальное состояние ДКА.
*/
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

/*!
* \brief Запуск алгоритма проверки соответствия входной строки регулярному выражению, на основе которого построен ДКА.
*
* \param[in] str  Регулярное выражение.
* \return      1, если входная строка соответствует регулярному выражению, на основе которого построен ДКА, 0 - если не соответствует.
*/
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

//! Структура для хранения информации о состоянии ДКА.
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

/*!
* \brief Построение ДКА по дереву регулярного выражения.
*
* \param[in]  tr  Дерево регулярного выражения.
*
*/
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

/*!
* \brief Переход ДКА из текущего состояния по символу.
*
* \param[in] c  Символ, по которому должен осуществляться переход.
* \return    1, если переход возможен, 0 - если невозможен.
*/
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

/*!
* \brief Деструктор.
*
*/
dfa::~dfa()
{
  for (std::set <state*>::iterator it = states.begin(); it != states.end(); ++it)
  delete (*it);
}
