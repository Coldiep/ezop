
#pragma once

#include <Wt/WDateTime>
#include <Wt/Dbo/Types>
#include <Wt/Dbo/WtSqlTraits>

namespace ezop { namespace web {

class User;
class Project;

/// Класс онтологии.
class Ontology {
public:
  template<class Action>
  void persist(Action& a) {
    Wt::Dbo::field(a, name_,   "name");
    Wt::Dbo::field(a, desc_,   "desc");
    Wt::Dbo::field(a, text_,   "text");
    Wt::Dbo::field(a, date_,   "date");

    Wt::Dbo::belongsTo(a, author_, "author");
    Wt::Dbo::belongsTo(a, project_, "project");
  }

private:
  Wt::WString             name_;       ///< Имя онтологии.
  std::string             desc_;       ///< Описание.
  std::string             text_;       ///< Текст онтологии.
  Wt::Dbo::ptr<User>      author_;     ///< Автор онтологии.
  Wt::Dbo::ptr<Project>   project_;     ///< Автор онтологии.
  Wt::WDateTime           date_;       ///< Дата создания онтологии.
};

}}  // namespace ezop, web.

DBO_EXTERN_TEMPLATES(ezop::web::Ontology);

