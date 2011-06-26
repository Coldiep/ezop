
#include <Wt/WText>
#include <Wt/WString>

#include <web/login.h>
#include <web/registration.h>
#include <web/login_menu.h>
using ezop::web::LoginMenu;

/**
 * \brief Создает пункты подменю.
 *
 * \param parent_menu Родительское меню, пункты подменю которого будут созданы.
 */
void LoginMenu::CreateSubMenu(Wt::WMenu* parent_menu) {
  parent_menu->addItem(Wt::WString::tr("login-menu-login"), new Login());
  parent_menu->addItem(Wt::WString::tr("login-menu-registration"), new Registration());
}

