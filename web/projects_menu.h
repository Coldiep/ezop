
#pragma once

#include <Wt/WText>
#include <Wt/WString>

#include <web/menu_element.h>

namespace ezop { namespace web {

class ProjectsMenu : public MenuElement {
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
    parent_menu->addItem(Wt::WString::tr("projects-menu-list"), new Wt::WText(Wt::WString::fromUTF8("projects-menu-list")));
    parent_menu->addItem(Wt::WString::tr("projects-menu-mine"), new Wt::WText(Wt::WString::fromUTF8("projects-menu-mine")));
    parent_menu->addItem(Wt::WString::tr("projects-menu-create"), new Wt::WText(Wt::WString::fromUTF8("projects-menu-create")));
  }
};

}}  // namespace ezop, web.

