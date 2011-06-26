
#pragma once

#include <web/menu_element.h>

namespace ezop { namespace web {

class ProjectsMenu : public MenuElement {
public:
  /// Возвращает true, если данный элемент меню имеет пункты подменю.
  bool HasSubMenu() {
    return true;
  }

  /**
   * \brief Создает пункты подменю.
   *
   * \param parent_menu Родительское меню, пункты подменю которого будут созданы.
   */
  void CreateSubMenu(Wt::WMenu* parent_menu);
};

}}  // namespace ezop, web.

