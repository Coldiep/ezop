
#pragma once

#include <string>

#include <Wt/WContainerWidget>
#include <Wt/WText>
#include <Wt/WString>
#include <Wt/WAnchor>
#include <Wt/WTemplate>

namespace ezop { namespace web {

/// Класс, представляющий панель банера.
class Banner : public Wt::WContainerWidget {
public:
  /// В конструкторе создаются компоненты банера.
  explicit Banner(const Wt::WString& user_name, Wt::WContainerWidget* parent);

  /// Установка текущего имени пользователя.
  void UpdateUser(const Wt::WString& name);

private:
  Wt::WTemplate* panel_; ///< Панель с названием и именем пользователя.
};

}}  // namespace ezop, web.

