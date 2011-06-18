
#pragma once

#include <Wt/WContainerWidget>
#include <Wt/WMenu>

namespace ezop { namespace web {

/// Абстрактный класс для всех элементов главного меню.
class MenuElement : public Wt::WContainerWidget {
public:
  /// Возвращает true, если данный элемент меню имеет пункты подменю.
  virtual bool HasSubMenu() = 0;

  /**
   * \brief Создает пункты подменю.
   *
   * \param parent_menu Родительское меню, пункты подменю которого будут созданы.
   */
  virtual void CreateSubMenu(Wt::WMenu* parent_menu) {
  }

  /// Виртуальный деструктор.
  virtual ~MenuElement() {
  }
};

}}  // namespace ezop, web.

