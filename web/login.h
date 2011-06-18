
#pragma once

#include <Wt/WContainerWidget>
#include <Wt/WText>
#include <Wt/WString>
#include <Wt/WBreak>
#include <Wt/WTable>
#include <Wt/WLabel>
#include <Wt/WPushButton>
#include <Wt/WLineEdit>
#include <Wt/WMessageBox>

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
    layout->elementAt(0, 0)->resize(Wt::WLength(18, Wt::WLength::FontEx), Wt::WLength::Auto);
    username_ = new Wt::WLineEdit(layout->elementAt(0, 1));
    username_label->setBuddy(username_);
    username_->setStyleClass("label");
    username_->setTextSize(20);

    // Пароль.
    Wt::WLabel* password_label = new Wt::WLabel(Wt::WString::tr("user.password"), layout->elementAt(1, 0));
    password_ = new Wt::WLineEdit(layout->elementAt(1, 1));
    password_->setEchoMode(Wt::WLineEdit::Password);
    password_label->setBuddy(password_);
    password_->setStyleClass("label");
    password_->setTextSize(20);

    addWidget(new Wt::WBreak(this));

    Wt::WPushButton* login_button = new Wt::WPushButton(Wt::WString::tr("user.login"), layout->elementAt(2, 1));
    login_button->clicked().connect(this, &Login::Check);
    login_button->setStyleClass("label");
  }

  void Check() {
    user_ = username_->text();
    std::wstring password = password_->text();
    username_->setText("");
    password_->setText("");
    Wt::WMessageBox::show(Wt::WString::tr("message.confirmation"), Wt::WString::tr("login.ok"), Wt::Ok);
  }

public:
  /// Конструктор инициализирует представление окна.
  Login() {
    Init();
  }

private:
  Wt::WLineEdit* username_; ///< Поле для вводе имени пользователя.
  Wt::WLineEdit* password_; ///< Поле для вовда пароля.
  std::wstring   user_;     ///< Имя пользователя.
};

}}  // namespace ezop, web.

