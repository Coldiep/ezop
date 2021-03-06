
#pragma once

#include <Wt/WApplication>

#include <web/ezop_widget.h>

namespace ezop { namespace web {

/// Абстрактный класс для всех элементов главного меню.

/// Главный класс Web приложения.
class EzopApplication : public Wt::WApplication {
public:
  /// В конструкторе создается главное меню проекта.
  explicit EzopApplication(const Wt::WEnvironment& env);

  /// Установка текущего имени пользователя.
  void SetUserName(const std::string& name);

private:
  /// Указатель на класс главного виджета приложения.
  EzopWidget* ezop_widget_;
};

}}  // namespace ezop, web.

