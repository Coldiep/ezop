
#pragma once

#include <Wt/WString>
#include <Wt/Dbo/Types>

namespace ezop { namespace web {

class Project;
class Ontology;

/// Проекты, создаваемые пользователями.
typedef Wt::Dbo::collection<Wt::Dbo::ptr<Project> > Projects;

/// Онтологии, создаваемые пользователями.
typedef Wt::Dbo::collection<Wt::Dbo::ptr<Ontology> > Ontologies;

/// Класс пользователя сессии.
class User {
public:
  /// Роль пользователя в системе.
  enum Role {
    kUnknown = 0 ///< Неизвестный пользователь.
    , kVisitor   ///< Обычный посетитель -- пользователь сервисами системы.
    , kAdmin     ///< Администратор.
  };

  /// Установка пароля через MD5.
  void SetPassword(const std::string& password);

  /// Аутентификация в базе.
  bool Authenticate(const std::string& password) const;

  /// Генерация уникального токена.
  std::string GenerateToken();

  /// Получить проекты, собственниками которых является пользователь.
  Projects GetProjects();

  /// Получить онтологии, автором которых является пользователь.
  Ontologies GetOntologies();

  template<class Action>
  void persist(Action& a) {
    Wt::Dbo::field(a, name_,     "name");
    Wt::Dbo::field(a, password_, "password");
    Wt::Dbo::field(a, role_,     "role");
    Wt::Dbo::field(a, token_,    "token");

    Wt::Dbo::hasMany(a, projects_,   Wt::Dbo::ManyToOne,  "owner");
    Wt::Dbo::hasMany(a, ontologies_, Wt::Dbo::ManyToOne,  "author");
  }

  std::string password_;    ///< Пароль, зашифрованный MD5.
  std::string token_;       ///< Уникальная строка, используемая в куках.
  Wt::WString name_;        ///< Имя пользователя.
  Role        role_;        ///< Роль пользователя в системе.
  Projects    projects_;    ///< Проекты, собственником которых пользователь является.
  Ontologies  ontologies_;  ///< Онтологии, автором которых является пользователь.
};

}}  // namespace ezop, web.

DBO_EXTERN_TEMPLATES(ezop::web::User);
