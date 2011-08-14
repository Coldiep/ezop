
#pragma once

#include <string>

#include <Wt/Dbo/Session>
#include <Wt/Dbo/ptr>
#include <Wt/WSignal>
#include <Wt/Dbo/backend/Sqlite3>

namespace ezop { namespace web {

class User;

/// Класс представляет сессию работы с сайтом.
class Session : public Wt::Dbo::Session {
public:
  /// Инициализация именем базы.
  explicit Session(const std::string& sqlite_db);

  /// Установка имени пользователя.
  void SetUser(Wt::Dbo::ptr<User> user);

  /// Получение текущего пользователя.
  Wt::Dbo::ptr<User> GetUser() const {
    return user_;
  }

private:
  Wt::Dbo::backend::Sqlite3 connection_; ///< Объект соединения с базой.
  Wt::Dbo::ptr<User>        user_;       ///< Текущий пользователь.
};

}}  // namespace ezop, web.
