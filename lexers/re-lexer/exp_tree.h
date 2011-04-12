#include <stack>
#include <sstream>
#include <set>

//! Типы вершин дерева разбора.
enum point_type {symbol=0,opUnion, opIter,opConcat,OpLeftBracket,OpRightBracket,empty};

//! Определение класса вершины дерева регулярного выражения.
class tree_point {
public:
  tree_point* left;     //!< Левая дочерняя вершина.
  tree_point* right;    //!< Правая дочерняя вершина.
  tree_point* parent;   //!< Родительская вершина.
  point_type  type;     //!< Тип вершины.
  char        contents; //!< Символ, соответствующий вершине.
  bool        nullable; //!< Показатель того, выводима ли из вершины пустая строка.
  int         id;       //!< Идентификатор вершины.

  int end;  // Это что такое?

  //! Тип множества вершин дерева.
  typedef std::set<tree_point*> TreePointSet;

  TreePointSet firstpos;    //!< Множество firstpos.
  TreePointSet lastpos;     //!< Множество lastpos.
  TreePointSet followpos;   //!< Множество followpos.

  //! Конструктор по умолчанию.
  tree_point();

  //! Конструктор, инициализирующийся одним символом.
  tree_point(char c);

  //! Конструктор, инициализирующийся диапазоном символов.
  tree_point(unsigned int c1,unsigned int c2);

  //! Конструктор копирования.
  tree_point(tree_point*tp);

  //! Вычисление множеств firstpos, lastpos и followpos для вершины.
  void calc();

  //! Вывод параметров вершины на печать.
  void print(int n);

  //! Деструктор.
  ~tree_point();
};

//! Определение класса дерева регулярного выражения.
class exp_tree {
public:
  tree_point*             root;     //! Корень дерева.
  std::string             alphabet; //! Алфавит регулярного выражения.
  std::set<tree_point*>   leaves;   //! Множество листьев дерева.

  //! Конструктор по умолчанию.
  exp_tree();

  //! Конструктор копирования.
  exp_tree(exp_tree* t);

  //! Конструктор, инициализирующийся вершиной.
  exp_tree(tree_point pnt);

  //! Конструктор, инициализирующийся диапазоном символов.
  exp_tree(unsigned int c1,unsigned int c2);

  //! Конструктор, инициализирующийся одним символом.
  exp_tree(char c);

  //! Конструктор, инициализирующийся типом вершины.
  exp_tree(point_type t);

  //! Проверка того, является ли дерево пустым.
  bool is_empty();

  //! Построение нового дерева с корнем заданного типа.
  exp_tree* make_new_root(point_type t);

  //! Слияние двух деревьев в одно.
  static exp_tree* merge_trees(exp_tree* l,exp_tree* r,point_type t)

  //! Вычисление множества followpos.
  void calc_followpos();

  //! Деструктор.
  ~exp_tree();
};

//! Определение структуры позиции в строке регулярного выражения.
struct Position{
  const char* PosPointer;
  unsigned int PosNumber;

  Position (const char* posPointer)
    : PosPointer(posPointer)
    , PosNumber(1) {
  }

  void Next() {
    if (not IsEnd ()) {
      ++ PosPointer;
      ++ PosNumber;
    }
  }

  bool IsEnd() {
    return *PosPointer == 0;
  }

  unsigned GetPos() {
    return PosNumber;
  }

  const char* LookForward(unsigned dist) {
    return PosPointer + dist;
  }

  char operator*() {
    return *PosPointer;
  }

  Position& operator++() {
    Next();
    return *this;
  }

  const char* operator+(unsigned dist) {
    return LookForward(dist);
  }
};
