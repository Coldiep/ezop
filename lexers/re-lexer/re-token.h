#include <set>
#include <map>
#include <vector>
#include <string>

namespace relexer {

//! Определение класса токена.
class ReToken
{
public:
  //! Конструктор по умолчанию.
  ReToken() {start_ = -1; finish_ = -1;};
  //! Конструктор,заполняющий все поля экземпляра класса.
  ReToken(int t,int s,int f,std::string st,bool ret,int term_sym_id = 0);
  //! Конструктор копирования.
  ReToken(ReToken* t);
  //! Тип токена.
  int type_;
  //! Позиция начала токена.
  int start_;
  //! Позиция конца токена.
  int finish_;
  //! Флаг для указания того, нужно ли включать данный токен в выходное дерево токенов.
  bool is_returned_;
  //! Идентификатор терминального символа грамматики,которому соответствует данный токен.
  int terminal_symbol_id_;
  //! Идентификатор токена,использующийся для построения дерева.
  std::string id_;
  //! Фрагмент входной цепочки,которому соотвествует данный токен.
  std::string str_;
  //! Множество дочерних узлов данного токена в выходном дереве токенов.
  std::set<ReToken*> children_;

};

//! Определение класса дерева токенов.
class TokenTree
{
public:
  //! Корень дерева.
  ReToken* root_;
  //! Конструктор по умолчанию.
  TokenTree();
  //! Добавление вершины к дереву.
  void AddNode(ReToken* node);
};

}