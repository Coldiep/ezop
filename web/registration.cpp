
#include <Wt/WApplication>
#include <Wt/WText>
#include <Wt/WString>
#include <Wt/WBreak>
#include <Wt/WTable>
#include <Wt/WLabel>
#include <Wt/WPushButton>
#include <Wt/WLineEdit>
#include <Wt/WMessageBox>
#include <Wt/WStringUtil>

#include <stdexcept>

#include <sqlite3.h>

#include <web/registration.h>
using ezop::web::Registration;

void Registration::Init() {
  // Добавляем заголовок окна.
  addWidget(new Wt::WText(Wt::WString::tr("registration.title"), this));
  addWidget(new Wt::WBreak(this));

  // Формируем элементы интерфейса.
  Wt::WTable* layout = new Wt::WTable();
  addWidget(layout);
  layout->setStyleClass("registration");

  // Имя пользователя.
  Wt::WLabel* first_name_label = new Wt::WLabel(Wt::WString::tr("registration.first"), layout->elementAt(0, 0));
  layout->elementAt(0, 0)->resize(Wt::WLength(22, Wt::WLength::FontEx), Wt::WLength::Auto);
  first_name_ = new Wt::WLineEdit(layout->elementAt(0, 1));
  first_name_label->setBuddy(first_name_);
  first_name_->setStyleClass("label");
  first_name_->setTextSize(22);

  // Фамилия пользователя.
  Wt::WLabel* second_name_label = new Wt::WLabel(Wt::WString::tr("registration.second"), layout->elementAt(1, 0));
  layout->elementAt(1, 0)->resize(Wt::WLength(22, Wt::WLength::FontEx), Wt::WLength::Auto);
  second_name_ = new Wt::WLineEdit(layout->elementAt(1, 1));
  second_name_label->setBuddy(second_name_);
  second_name_->setStyleClass("label");
  second_name_->setTextSize(22);

  // email.
  Wt::WLabel* email_label = new Wt::WLabel(Wt::WString::tr("registration.email"), layout->elementAt(2, 0));
  layout->elementAt(2, 0)->resize(Wt::WLength(22, Wt::WLength::FontEx), Wt::WLength::Auto);
  email_ = new Wt::WLineEdit(layout->elementAt(2, 1));
  email_label->setBuddy(email_);
  email_->setStyleClass("label");
  email_->setTextSize(22);

  // Ник.
  Wt::WLabel* user_name_label = new Wt::WLabel(Wt::WString::tr("registration.nick"), layout->elementAt(3, 0));
  layout->elementAt(3, 0)->resize(Wt::WLength(22, Wt::WLength::FontEx), Wt::WLength::Auto);
  user_name_ = new Wt::WLineEdit(layout->elementAt(3, 1));
  user_name_label->setBuddy(user_name_);
  user_name_->setStyleClass("label");
  user_name_->setTextSize(22);

  // Пароль.
  Wt::WLabel* password_label = new Wt::WLabel(Wt::WString::tr("registration.password"), layout->elementAt(4, 0));
  layout->elementAt(4, 0)->resize(Wt::WLength(22, Wt::WLength::FontEx), Wt::WLength::Auto);
  password_ = new Wt::WLineEdit(layout->elementAt(4, 1));
  password_->setEchoMode(Wt::WLineEdit::Password);
  password_label->setBuddy(password_);
  password_->setStyleClass("label");
  password_->setTextSize(22);

  addWidget(new Wt::WBreak(this));

  Wt::WPushButton* login_button = new Wt::WPushButton(Wt::WString::tr("registration.push"), layout->elementAt(5, 1));
  login_button->clicked().connect(this, &Registration::Register);
  login_button->setStyleClass("label");
}

namespace {

struct UserPassword {
  std::string password_;
  bool        result_;

  explicit UserPassword(const std::string& password)
    : password_(password)
    , result_(false) {
  }

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
};

} //namespace 

void Registration::Register() {
  try {
    // Получаем даные, введенные пользователем.
    std::wstring password = password_->text();
    std::wstring username = user_name_->text();
    if (username.empty() or password.empty()) {
      throw std::invalid_argument("Имя пользователя и пароль не могут быть пустыми");
    }

    std::string id = Wt::toUTF8(username);
    std::string pwd = Wt::toUTF8(password);
    std::string fname = Wt::toUTF8(first_name_->text());
    std::string sname = Wt::toUTF8(second_name_->text());
    std::string email = Wt::toUTF8(email_->text());

    // Сначала проверяем, зарегистрирован такой пользователь или нет.
    sqlite3* db = NULL;
    Wt::WApplication* app = Wt::WApplication::instance();
    if (sqlite3_open((app->appRoot() + "ezop.db").c_str(), &db) == 0) {
      char* err_msg = NULL;
      UserPassword check_pwd(pwd);
      if (sqlite3_exec(db, ("select id, password from users where id='" + id + "'").c_str(), UserPassword::CheckPassword, &check_pwd, &err_msg) == SQLITE_OK) {
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

    user_name_->setText("");
    password_->setText("");
    first_name_->setText("");
    email_->setText("");
    second_name_->setText("");

    Wt::WMessageBox::show(Wt::WString::tr("message.confirmation"), Wt::WString::tr("login.ok"), Wt::Ok);
  } catch (const std::exception& err) {
    Wt::WMessageBox::show(Wt::WString::fromUTF8("Ошибка"), Wt::WString::fromUTF8(err.what()), Wt::Abort);
  }
  user_name_->setText("");
  password_->setText("");
}
