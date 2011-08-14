
#include <stdexcept>

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

#include <Wt/Dbo/Session>
#include <Wt/Dbo/backend/Sqlite3>

#include <web/model/user.h>

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
    Wt::WApplication* app = wApp;
    Wt::Dbo::backend::Sqlite3 connection(app->appRoot() + "/ezop.db");
    Wt::Dbo::Session session;
    connection.setProperty("show-queries", "true");
    session.setConnection(connection);
    session.mapClass<ezop::web::User>("user");
    session.mapClass<ezop::web::Project>("project");
    session.mapClass<ezop::web::Ontology>("ontology");

    Wt::Dbo::Transaction t(session);
    Wt::Dbo::ptr<ezop::web::User> user = session.find<ezop::web::User>().where("name = ?").bind(id);
    t.commit();

    if (user) {
      throw std::invalid_argument("Пользователь с таким именем уже зарегистрирован в системе");
    } else {
      Wt::Dbo::Transaction t(session);

      Wt::Dbo::ptr<User> new_user = session.add(new User());
      User* a = new_user.modify();
      a->name_ = id;
      a->role_ = User::kVisitor;
      a->SetPassword(pwd);

      t.commit();
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
