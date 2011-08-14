
#include <web/model/user.h>
#include <web/model/ontology.h>
#include <web/model/project.h>

#include <web/session.h>
using ezop::web::Session;

Session::Session(const std::string& sqlite_db)
  : connection_(sqlite_db) {
  connection_.setProperty("show-queries", "true");

  // Устанавливаем соединение с базой.
  setConnection(connection_);

  // Связываем классы с таблицами в базе данных.
  mapClass<User>("user");
  mapClass<Ontology>("ontology");
  mapClass<Project>("project");

  try {
    Wt::Dbo::Transaction t(*this);
    createTables();

    // Создаем пользователя "администратор".
    Wt::Dbo::ptr<User> admin = add(new User());
    User* a = admin.modify();
    a->name_ = "admin";
    a->role_ = User::kAdmin;
    a->SetPassword("admin");

    // Создаем пользователя "неизвестный".
    Wt::Dbo::ptr<User> unknown = add(new User());
    User* b = unknown.modify();
    b->name_ = "unknown";
    b->role_ = User::kUnknown;
    b->SetPassword("");

    t.commit();

    std::cerr << "Created database, and admin/admin user";
  } catch (std::exception& e) {
    std::cerr << e.what() << std::endl;
    std::cerr << "Using existing database";
  }
}

void Session::SetUser(Wt::Dbo::ptr<User> user) {
  user_ = user;
}

