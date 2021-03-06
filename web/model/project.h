
#pragma once

#include <Wt/WDateTime>
#include <Wt/WString>
#include <Wt/Dbo/Types>
#include <Wt/Dbo/WtSqlTraits>

namespace ezop { namespace web {

class User;
class Ontology;

/// Онтологии, входящие в проект.
typedef Wt::Dbo::collection<Wt::Dbo::ptr<Ontology> > Ontologies;

/// Участники проекта.
typedef Wt::Dbo::collection<Wt::Dbo::ptr<User> > Users;

/// Класс проекта.
class Project {
public:
  /// Получить онтологии, входящие в проект.
  Ontologies GetOntologies();

  template<class Action>
  void persist(Action& a) {
    Wt::Dbo::field(a, name_,  "name");
    Wt::Dbo::field(a, desc_,  "desc");
    Wt::Dbo::field(a, date_,  "date");

    Wt::Dbo::belongsTo(a, owner_, "owner");
    Wt::Dbo::hasMany(a, ontologies_,    Wt::Dbo::ManyToOne,  "ontology");
    Wt::Dbo::hasMany(a, participants_,  Wt::Dbo::ManyToMany, "participant");
  }

private:
  Wt::WString         name_;          ///< Имя проекта.
  std::string         desc_;          ///< Описание проекта.
  Wt::WDateTime       date_;          ///< Дата создания проекта.
  Wt::Dbo::ptr<User>  owner_;         ///< Собственник проекта.
  Users               participants_;  ///< Участники проекта.
  Ontologies          ontologies_;    ///< Онтологии, входящие в проект.
};

}}  // namespace ezop, web.

DBO_EXTERN_TEMPLATES(ezop::web::Project);

