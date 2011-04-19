#include <stack>
#include <sstream>
#include <set>

namespace relexer {

//! Типы вершин дерева разбора.
enum point_type {symbol=0,opUnion,opIter,opConcat,OpLeftBracket,OpRightBracket,empty};

//! Определение класса вершины дерева регулярного выражения.
class TreePoint {
public:
  TreePoint*  left_;     //!< Левая дочерняя вершина.
  TreePoint*  right_;    //!< Правая дочерняя вершина.
  TreePoint*  parent_;   //!< Родительская вершина.
  char        contents_; //!< Символ,соответствующий вершине.
  point_type  type_;     //!< Тип вершины.
  bool        nullable_; //!< Показатель того,выводима ли из вершины пустая строка.
  int         id_;       //!< Идентификатор вершины.
  int         end_;      //!< Показатель того,что вершина соответствует маркеру конца регулярного выражения.
  

public:
  //! Тип множества вершин дерева.
  typedef std::set<TreePoint*> TreePointSet;

  TreePointSet firstpos_;    //!< Множество firstpos.
  TreePointSet lastpos_;     //!< Множество lastpos.
  TreePointSet followpos_;   //!< Множество followpos.

  //! Конструктор по умолчанию.
  TreePoint();

  //! Конструктор,инициализирующийся одним символом.
  TreePoint(char c);

  //! Конструктор,инициализирующийся диапазоном символов.
  TreePoint(unsigned int c1,unsigned int c2);

  //! Конструктор копирования.
  TreePoint(TreePoint* tp);

  //! Вычисление множеств firstpos,lastpos и followpos для вершины.
  void Calc(int& n_id, std::set<TreePoint*>& leaves, std::string& symbols);

  //! Вывод параметров вершины на печать.
  void Print(int n);

  //! Деструктор.
  ~TreePoint();
};

//! Определение класса дерева регулярного выражения.
class ExpTree {
public:
  TreePoint*             root_;     //! Корень дерева.
  std::string            alphabet_; //! Алфавит регулярного выражения.
  std::set<TreePoint*>   leaves_;   //! Множество листьев дерева.

  //! Конструктор по умолчанию.
  ExpTree();

  //! Конструктор копирования.
  ExpTree(ExpTree* t);

  //! Конструктор,инициализирующийся вершиной.
  ExpTree(TreePoint pnt);

  //! Конструктор,инициализирующийся диапазоном символов.
  ExpTree(unsigned int c1,unsigned int c2);

  //! Конструктор,инициализирующийся одним символом.
  ExpTree(char c);

  //! Конструктор,инициализирующийся типом вершины.
  ExpTree(point_type t);

  //! Проверка того, является ли дерево пустым.
  bool is_empty();

  //! Построение нового дерева с корнем заданного типа.
  ExpTree* MakeNewRoot(point_type t);

  //! Слияние двух деревьев в одно.
  static ExpTree* MergeTrees(ExpTree* l,ExpTree* r,point_type t);

  //! Вычисление множества followpos.
  void CalcFollowpos();

  //! Деструктор.
  ~ExpTree();
};

//! Определение структуры позиции в строке регулярного выражения.
struct Position{
  const char* PosPointer;
  unsigned int PosNumber;

  Position (const char* posPointer)
    : PosPointer(posPointer)
    ,PosNumber(1) {
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

}