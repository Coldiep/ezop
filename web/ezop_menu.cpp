
#include <Wt/WText>
#include <Wt/WString>

#include <web/ezop_menu.h>
using ezop::web::EzopMenu;

EzopMenu::EzopMenu() {
  new Wt::WText(Wt::WString::tr("morda"));
}

/**
 * \brief Создает пункты подменю.
 *
 * \param parent_menu Родительское меню, пункты подменю которого будут созданы.
 */
void EzopMenu::CreateSubMenu(Wt::WMenu* parent_menu) {
  parent_menu->addItem(Wt::WString::tr("ezop-menu-morda"), new Wt::WText(Wt::WString::tr("morda")));
  parent_menu->addItem(Wt::WString::tr("ezop-menu-docs"), new Wt::WText(Wt::WString::tr("ezop-menu-docs")));
  parent_menu->addItem(Wt::WString::tr("ezop-menu-news"), new Wt::WText(Wt::WString::tr("ezop-menu-news")));
}

