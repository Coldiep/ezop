#include <vector>
#include <map>
#include <set>

//! Типы состояний ДКА.
enum state_type {start, mid, finish, start_finish};

//! Определение класса состояния ДКА.
class state
{
public:
  //! Идентификатор состояния.
  int id;
  //! Флаг для определения того, является ли состояние помеченным.
  bool marked;
  //! Возможные переходы из данного состояния.
  std::map<char, unsigned int> transitions;
  //! Множество идентификаторов вершин дерева разбора, входящих в данное состояние.
  std::set <int> ids;
  //! Тип состояния.
  state_type type;

  //! Конструктор по умолчанию.
  state();
  //! Получение списка возможных переходов из данного состояния по определенному символу.
  unsigned int get_transition(char c);
  //! Добавление перехода.
  void add_transition(char c,unsigned int s);
};

//! Определение класса ДКА.
class dfa
{
public:
  //! Множество состояний ДКА.
  std::set <state*> states;
  //! Текущее состояние ДКА.
  state* curr_state;

  //! Конструктор по умолчанию.
  dfa();
  //! Деструктор.
  ~dfa();
  //! Добавление состояния.
  void add_state(state* s);
  //! Добавление состояния.
  void add_state(int id,state_type type=mid);
  //! Создние нового состояния.
  state* make_state(std::set<tree_point*> leaves_set,state_type type=mid);
  //! Добавление перехода.
  void add_transition(int beg_state,char c,int end_state);
  //! Получение состояния по идентификатору.
  state* get_state(int ii);
  //! Получение начального состояния ДКА.
  state* get_start_state();
  //! Запуск алгоритма проверки соответствия входной строки регулярному выражению, на основе которого построен ДКА.
  int process(std::string str);
  //! Построение ДКА по дереву регулярного выражения.
  void build(exp_tree* t);
  //! Переход ДКА из текущего состояния по символу.
  int move(char c);
};