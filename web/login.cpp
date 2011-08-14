
#include <stdexcept>

#include <Wt/WApplication>
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

#include <Wt/Dbo/Session>
#include <Wt/Dbo/backend/Sqlite3>

#include <web/model/user.h>
#include <web/login.h>
using ezop::web::Login;

void Login::Init() {
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

void Login::Check() {
  try {
    std::wstring username = username_->text();
    std::wstring password = password_->text();
    if (username.empty() or password.empty()) {
      throw std::invalid_argument("Имя пользователя и пароль не могут быть пустыми");
    }
    std::string id = Wt::toUTF8(username);
    std::string pwd = Wt::toUTF8(password);

    Wt::WApplication* app = wApp;
    Wt::Dbo::backend::Sqlite3 connection(app->appRoot() + "/ezop.db");
    Wt::Dbo::Session session;
    session.setConnection(connection);
    session.mapClass<ezop::web::User>("user");
    session.mapClass<ezop::web::Project>("project");
    session.mapClass<ezop::web::Ontology>("ontology");

    Wt::Dbo::Transaction t(session);
    Wt::Dbo::ptr<ezop::web::User> user = session.find<ezop::web::User>().where("name = ?").bind(id);

    if (user) {
      if (user->Authenticate(pwd)) {

      } else {
        throw std::invalid_argument("Неверный пароль");
      }
    } else {
      throw std::invalid_argument("Пользователь с таким именем не зарегистрирован в системе");
    }

    t.commit();
  } catch (const std::exception& err) {
    Wt::WMessageBox::show(Wt::WString::fromUTF8("Ошибка"), Wt::WString::fromUTF8(err.what()), Wt::Abort);
  }
  username_->setText("");
  password_->setText("");
}

