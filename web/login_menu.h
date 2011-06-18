
#pragma once

#include <Wt/WText>
#include <Wt/WString>

#include <web/menu_element.h>
#include <web/login.h>

namespace ezop { namespace web {

/// Класс раздела, отвечающего за вход в систему.
class LoginMenu : public MenuElement {
public:
  /// Возвращает true, если данный элемент меню имеет пункты подменю.
  virtual bool HasSubMenu() {
    return true;
  }

  /**
   * \brief Создает пункты подменю.
   *
   * \param parent_menu Родительское меню, пункты подменю которого будут созданы.
   */
  virtual void CreateSubMenu(Wt::WMenu* parent_menu) {
    parent_menu->addItem(Wt::WString::tr("login-menu-login"), new Login());
    parent_menu->addItem(Wt::WString::tr("login-menu-registration"), new Wt::WText(Wt::WString::fromUTF8("login-menu-registration")));
  }
};

}}  // namespace ezop, web.

