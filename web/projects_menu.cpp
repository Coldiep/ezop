
#include <Wt/WText>
#include <Wt/WString>

#include <web/projects_menu.h>
using ezop::web::ProjectsMenu;

/**
 * \brief Создает пункты подменю.
 *
 * \param parent_menu Родительское меню, пункты подменю которого будут созданы.
 */
void ProjectsMenu::CreateSubMenu(Wt::WMenu* parent_menu) {
  parent_menu->addItem(Wt::WString::tr("projects-menu-list"), new Wt::WText(Wt::WString::fromUTF8("projects-menu-list")));
  parent_menu->addItem(Wt::WString::tr("projects-menu-mine"), new Wt::WText(Wt::WString::fromUTF8("projects-menu-mine")));
  parent_menu->addItem(Wt::WString::tr("projects-menu-create"), new Wt::WText(Wt::WString::fromUTF8("projects-menu-create")));


}

