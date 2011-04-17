#include <vector>
#include <map>
#include <set>

namespace relexer {

//! Типы состояний ДКА.
enum state_type {start,mid,finish,start_finish};

//! Определение класса состояния ДКА.
class State
{
public:
  //! Идентификатор состояния.
  int id_;
  //! Флаг для определения того, является ли состояние помеченным.
  bool marked_;
  //! Возможные переходы из данного состояния.
  std::map<char,unsigned int> transitions_;
  //! Множество идентификаторов вершин дерева разбора,входящих в данное состояние.
  std::set <int> ids_;
  //! Тип состояния.
  state_type type_;

  //! Конструктор по умолчанию.
  State();
  //! Получение списка возможных переходов из данного состояния по определенному символу.
  unsigned int get_transition(char c);
  //! Добавление перехода.
  void add_transition(char c,unsigned int s);
};

//! Определение класса ДКА.
class DFA
{
public:
  //! Множество состояний ДКА.
  std::set <State*> states_;
  //! Текущее состояние ДКА.
  State* curr_state_;

  //! Конструктор по умолчанию.
  DFA();
  //! Деструктор.
  ~DFA();
  //! Добавление состояния.
  void add_state(State* s);
  //! Добавление состояния.
  void add_state(int id,state_type type=mid);
  //! Создние нового состояния.
  State* make_state(std::set<TreePoint*> leaves_set,state_type type=mid);
  //! Добавление перехода.
  void add_transition(int beg_state,char c,int end_state);
  //! Получение состояния по идентификатору.
  State* get_state(int ii);
  //! Получение начального состояния ДКА.
  State* get_start_state();
  //! Запуск алгоритма проверки соответствия входной строки регулярному выражению,на основе которого построен ДКА.
  int process(std::string str);
  //! Построение ДКА по дереву регулярного выражения.
  void build(ExpTree* t);
  //! Переход ДКА из текущего состояния по символу.
  int move(char c);
};

}