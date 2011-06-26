
#pragma once

#include <Wt/WContainerWidget>
#include <Wt/WLineEdit>

namespace ezop { namespace web {

/// Виджет ввода пользовательских данных.
class Login : public Wt::WContainerWidget {
  void Init();

  /// Проверка пароля.
  static int CheckPassword(void* param, int argc, char** argv, char** col_name);

  /// Реализация получения данных пользователя.
  void Check();

public:
  /// Конструктор инициализирует представление окна.
  Login() {
    Init();
  }

private:
  Wt::WLineEdit* username_; ///< Поле для вводе имени пользователя.
  Wt::WLineEdit* password_; ///< Поле для вовда пароля.
};

}}  // namespace ezop, web.

