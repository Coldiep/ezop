
#pragma once

#include <string>

#include <Wt/WContainerWidget>
#include <Wt/WMenu>
#include <Wt/WString>
#include <Wt/WStackedWidget>

#include <web/menu_element.h>
#include <web/session.h>

namespace ezop { namespace web {

/// Абстрактный класс для всех элементов главного меню.

/// Главный класс Web приложения.
class EzopWidget : public Wt::WContainerWidget {
public:
  /// В конструкторе создается главное меню проекта.
  EzopWidget();

  /// Установка текущего имени пользователя.
  void SetUserName(const Wt::WString& name);

private:
  /// Добавления элемента меню.
  void AddToMenu(Wt::WMenu* menu, const std::string& name, MenuElement* element);

  /// Имя текущего пользователя.
  Wt::WString user_name_;

  /// Содержимое.
  Wt::WStackedWidget* content_;

  /// Сессия базы данных.
  Session session_;
};

}}  // namespace ezop, web.

