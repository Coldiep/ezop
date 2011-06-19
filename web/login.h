
#pragma once

#include <stdexcept>

#include <Wt/WContainerWidget>
#include <Wt/WText>
#include <Wt/WString>
#include <Wt/WBreak>
#include <Wt/WTable>
#include <Wt/WLabel>
#include <Wt/WPushButton>
#include <Wt/WLineEdit>
#include <Wt/WMessageBox>
#include <Wt/WStringUtil>

#include <sqlite3.h>

namespace ezop { namespace web {

/// Виджет ввода пользовательских данных.
class Login : public Wt::WContainerWidget {
  void Init() {
    // Добавляем заголовок окна.
    addWidget(new Wt::WText(Wt::WString::tr("login.title"), this));
    addWidget(new Wt::WBreak(this));

    // Формируем элементы интерфейса.
    Wt::WTable* layout = new Wt::WTable();
    addWidget(layout);
    layout->setStyleClass("login");

    // Имя пользователя.
    Wt::WLabel* username_label = new Wt::WLabel(Wt::WString::tr("user.name"), layout->elementAt(0, 0));
    layout->elementAt(0, 0)->resize(Wt::WLength(22, Wt::WLength::FontEx), Wt::WLength::Auto);
    username_ = new Wt::WLineEdit(layout->elementAt(0, 1));
    username_label->setBuddy(username_);
    username_->setStyleClass("label");
    username_->setTextSize(22);

    // Пароль.
    Wt::WLabel* password_label = new Wt::WLabel(Wt::WString::tr("user.password"), layout->elementAt(1, 0));
    layout->elementAt(1, 0)->resize(Wt::WLength(22, Wt::WLength::FontEx), Wt::WLength::Auto);
    password_ = new Wt::WLineEdit(layout->elementAt(1, 1));
    password_->setEchoMode(Wt::WLineEdit::Password);
    password_label->setBuddy(password_);
    password_->setStyleClass("label");
    password_->setTextSize(22);

    addWidget(new Wt::WBreak(this));

    Wt::WPushButton* login_button = new Wt::WPushButton(Wt::WString::tr("user.login"), layout->elementAt(2, 1));
    login_button->clicked().connect(this, &Login::Check);
    login_button->setStyleClass("label");
  }

  struct UserPassword {
    std::string password_;
    bool        result_;

    explicit UserPassword(const std::string& password)
      : password_(password)
      , result_(false) {
    }
  };

  /// Проверка пароля.
  static int CheckPassword(void* param, int argc, char** argv, char** col_name) {
    UserPassword* pwd = (UserPassword*)param;
    for(int i = 0; i < argc; ++i) {
      if (std::string(col_name[i]) == "password") {
        if (argv[i] == pwd->password_) {
          pwd->result_ = true;
        } else {
          pwd->result_ = false;
        }
        break;
      }
    }
    return 0;
  }

  void Check() {
    try {
      std::wstring username = username_->text();
      std::wstring password = password_->text();
      if (username.empty() or password.empty()) {
        throw std::invalid_argument("Имя пользователя и пароль не могут быть пустыми");
      }
      std::string id = Wt::toUTF8(username);
      std::string pwd = Wt::toUTF8(password);

      sqlite3* db = NULL;
      Wt::WApplication* app = Wt::WApplication::instance();
      if (sqlite3_open((app->appRoot() + "ezop.db").c_str(), &db) == 0) {
        char* err_msg = NULL;
        UserPassword check_pwd(pwd);
        if (sqlite3_exec(db, ("select id, password from users where id='" + id + "'").c_str(), CheckPassword, &check_pwd, &err_msg) == SQLITE_OK) {
          if (check_pwd.result_) {
            Wt::WMessageBox::show(Wt::WString::tr("message.confirmation"), Wt::WString::tr("login.ok"), Wt::Ok);
          } else {
            throw std::invalid_argument(("Пользователь с именем '" + id + "' не зарегистирован в системе").c_str());
          }
        } else {
          sqlite3_free(err_msg);
          throw std::invalid_argument("Не получилось прочитать данные с базы");
        }
        sqlite3_close(db);
      } else {
        throw std::invalid_argument("Невозможно соединиться с базой данных пользователей");
      }
    } catch (const std::exception& err) {
      Wt::WMessageBox::show(Wt::WString::fromUTF8("Ошибка"), Wt::WString::fromUTF8(err.what()), Wt::Abort);
    }
    username_->setText("");
    password_->setText("");
  }

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

