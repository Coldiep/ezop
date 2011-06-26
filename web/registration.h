
#pragma once

#include <string>

#include <Wt/WContainerWidget>
#include <Wt/WLineEdit>

namespace ezop { namespace web {

/// Виджет ввода пользовательских данных.
class Registration : public Wt::WContainerWidget {
  void Init();
  void Register();

public:
  /// Конструктор инициализирует представление окна.
  Registration() {
    Init();
  }

private:
  Wt::WLineEdit* first_name_;   ///< Поле для вводе имени пользователя.
  Wt::WLineEdit* second_name_;  ///< Поле для вовда пароля.
  Wt::WLineEdit* email_;        ///< Поле для вовда пароля.
  Wt::WLineEdit* user_name_;    ///< Поле для вовда пароля.
  Wt::WLineEdit* password_;     ///< Поле для вовда пароля.
  std::wstring   user_;         ///< Имя пользователя.
};

}}  // namespace ezop, web.

