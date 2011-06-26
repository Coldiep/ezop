
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

  /// Установка текущего имени пользователя.
  void SetUserName(const std::string& name);

private:
  /// Добавления элемента меню.
  void AddToMenu(Wt::WMenu* menu, const std::string& name, MenuElement* element);

  /// Имя текущего пользователя.
  std::string user_name_;

  /// Содержимое.
  Wt::WStackedWidget* content_;
};

}}  // namespace ezop, web.

