
#include <Wt/WApplication>
#include <Wt/WHBoxLayout>
#include <Wt/WVBoxLayout>
#include <Wt/WSubMenuItem>

#include <web/ezop_menu.h>
#include <web/projects_menu.h>
#include <web/login_menu.h>

#include <web/ezop.h>
using ezop::web::EzopWidget;

EzopWidget::EzopWidget()
  : content_(new Wt::WStackedWidget()) {

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

/// Процедура создания приложения и главного окна.
Wt::WApplication* gCreateApplication(const Wt::WEnvironment& env) {
  // Создаем класс приложения.
  Wt::WApplication* app = new Wt::WApplication(env);

  // Загружаем русский интерфейс.
  app->messageResourceBundle().use(app->appRoot() + "rus");

  // Создаем подложку, на которой будут располагаться все элементы окна.
  Wt::WHBoxLayout* layout = new Wt::WHBoxLayout(app->root());
  layout->setContentsMargins(0, 0, 0, 0);

  // Создаем объект класса EzopWidget и располагаем его на всем окне.
  layout->addWidget(new EzopWidget());

  // Устнавливаем название окна и стиль отрисовки.
  app->setTitle(Wt::WString::tr("project-title"));
  app->addMetaHeader("viewport", "width=700, height=1200");
  app->useStyleSheet("main.css");

  return app;
}

/// Точка входа в программу.
int main(int argc, char** argv) {
  try {
    return Wt::WRun(argc, argv, &gCreateApplication);
  } catch (const std::exception& err) {
    std::cerr << err.what() << "\n";
  }
}

