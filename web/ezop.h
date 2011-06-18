
#pragma once

#include <string>

#include <Wt/WContainerWidget>
#include <Wt/WMenu>
#include <Wt/WStackedWidget>

#include <web/menu_element.h>

namespace ezop { namespace web {

/// Абстрактный класс для всех элементов главного меню.

/// Главный класс Web приложения.
class EzopWidget : public Wt::WContainerWidget {
public:
  /// В конструкторе создается главное меню проекта.
  EzopWidget();

private:
  /// Добавления элемента меню.
  void AddToMenu(Wt::WMenu* menu, const std::string& name, MenuElement* element);

  /// Содержимое.
  Wt::WStackedWidget* content_;
};

}}  // namespace ezop, web.

