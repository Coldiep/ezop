
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
class Registration : public Wt::WContainerWidget {
  void Init() {
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

  void Register() {
    //user_ = username_->text();
    std::wstring password = password_->text();
    user_name_->setText("");
    password_->setText("");
    Wt::WMessageBox::show(Wt::WString::tr("message.confirmation"), Wt::WString::tr("login.ok"), Wt::Ok);
  }

public:
  /// Конструктор инициализирует представление окна.
  Registration() {
    Init();
  }

private:
  Wt::WLineEdit* first_name_; ///< Поле для вводе имени пользователя.
  Wt::WLineEdit* second_name_; ///< Поле для вовда пароля.
  Wt::WLineEdit* email_; ///< Поле для вовда пароля.
  Wt::WLineEdit* user_name_; ///< Поле для вовда пароля.
  Wt::WLineEdit* password_; ///< Поле для вовда пароля.
  std::wstring   user_;     ///< Имя пользователя.
};

}}  // namespace ezop, web.

