
#include <Wt/WApplication>
#include <Wt/WHBoxLayout>
#include <Wt/WVBoxLayout>
#include <Wt/WSubMenuItem>

#include <web/ezop_menu.h>
#include <web/projects_menu.h>
#include <web/login_menu.h>

#include <web/ezop_widget.h>
using ezop::web::EzopWidget;

EzopWidget::EzopWidget()
  : user_name_("user.anonimus")
  , content_(new Wt::WStackedWidget())
  , session_(Wt::WApplication::appRoot() + "/ezop.db") {

  // Инициализируем содержимое.
  content_->setOverflow(Wt::WContainerWidget::OverflowAuto);
  content_->setPositionScheme(Wt::Relative);
  content_->setStyleClass("content");

  // Создаем главное меню.
  Wt::WMenu* menu = new Wt::WMenu(content_, Wt::Vertical);
  menu->setRenderAsList(true);
  menu->setStyleClass("menu");
  menu->setInternalPathEnabled();
  menu->setInternalBasePath("/");

  // Добавляем элементы меню.
  AddToMenu(menu, "ezop-menu", new EzopMenu());
  AddToMenu(menu, "projects-menu", new ProjectsMenu());
  AddToMenu(menu, "login-menu", new LoginMenu());

  // Создаем план главного окна.
  Wt::WHBoxLayout* horiz_layout = new Wt::WHBoxLayout();
  Wt::WVBoxLayout* vert_layout = new Wt::WVBoxLayout();
  Wt::WVBoxLayout* main_layout = new Wt::WVBoxLayout(this);

  // Добавляем виджеты на главное окно.
  main_layout->addWidget(new Wt::WText(Wt::WString::tr("sidebar-title")));
  main_layout->addLayout(horiz_layout, 1);
  horiz_layout->addWidget(menu, 0);
  horiz_layout->addLayout(vert_layout, 1);
  vert_layout->addWidget(content_, 1);
  horiz_layout->setResizable(0, true);
}

void EzopWidget::AddToMenu(Wt::WMenu* menu, const std::string& name, MenuElement* element) {
  // Если элемент меню имеет подменю, позволяем создать их.
  if (element->HasSubMenu()) {
    // Создаем подменю, кторое может содержать дочерние элементы.
    Wt::WSubMenuItem* smi = new Wt::WSubMenuItem(Wt::WString::tr(name), element);
    Wt::WMenu* sub_menu = new Wt::WMenu(content_, Wt::Vertical, 0);
    smi->setSubMenu(sub_menu);
    menu->addItem(smi);

    // Устанавливаем стили и пути.
    sub_menu->setRenderAsList(true);
    sub_menu->setInternalPathEnabled();
    sub_menu->setInternalBasePath("/" + smi->pathComponent());
    sub_menu->setStyleClass("menu submenu");

    // Позволяем классу элемента меню создать свое подменю.
    element->CreateSubMenu(sub_menu);
  } else {
    menu->addItem(Wt::WString::tr(name), element);
  }
}

/// Установка текущего имени пользователя.
void EzopWidget::SetUserName(const Wt::WString& name) {
  user_name_ = name;
}

