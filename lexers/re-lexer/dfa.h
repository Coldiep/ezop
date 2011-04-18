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
  unsigned int GetTransition(char c);
  //! Добавление перехода.
  void AddTransition(char c,unsigned int s);
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
  void AddState(State* s);
  //! Добавление состояния.
  void AddState(int id,state_type type=mid);
  //! Создние нового состояния.
  State* MakeState(std::set<TreePoint*> leaves_set, int& s_id, state_type type=mid);
  //! Добавление перехода.
  void AddTransition(int beg_state,char c,int end_state);
  //! Получение состояния по идентификатору.
  State* GetState(int ii);
  //! Получение начального состояния ДКА.
  State* GetStartState();
  //! Запуск алгоритма проверки соответствия входной строки регулярному выражению,на основе которого построен ДКА.
  int Process(std::string str);
  //! Построение ДКА по дереву регулярного выражения.
  void Build(ExpTree* t);
  //! Переход ДКА из текущего состояния по символу.
  int Move(char c);
};

}