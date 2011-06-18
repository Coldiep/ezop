
#pragma once

#include <Wt/WText>
#include <Wt/WString>

#include <web/menu_element.h>

namespace ezop { namespace web {

class EzopMenu : public MenuElement {
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
    parent_menu->addItem(Wt::WString::tr("ezop-menu-morda"), new Wt::WText(Wt::WString::tr("ezop-menu-morda")));
    parent_menu->addItem(Wt::WString::tr("ezop-menu-docs"), new Wt::WText(Wt::WString::tr("ezop-menu-docs")));
    parent_menu->addItem(Wt::WString::tr("ezop-menu-news"), new Wt::WText(Wt::WString::tr("ezop-menu-news")));
  }
};

}}  // namespace ezop, web.

