
#include <Wt/WHBoxLayout>

#include <web/ezop.h>
using ezop::web::EzopApplication;

EzopApplication::EzopApplication(const Wt::WEnvironment& env)
  : Wt::WApplication(env) {
  // Загружаем русский интерфейс.
  messageResourceBundle().use(appRoot() + "rus");

  // Создаем подложку, на которой будут располагаться все элементы окна.
  Wt::WHBoxLayout* layout = new Wt::WHBoxLayout(root());
  layout->setContentsMargins(0, 0, 0, 0);

  // Создаем объект класса EzopWidget и располагаем его на всем окне.
  ezop_widget_ = new EzopWidget();
  layout->addWidget(ezop_widget_);

  // Устнавливаем название окна и стиль отрисовки.
  setTitle(Wt::WString::tr("project-title"));
  addMetaHeader("viewport", "width=700, height=1200");
  useStyleSheet("main.css");
}

void EzopApplication::SetUserName(const std::string& name) {
  ezop_widget_->SetUserName(name);
}

/// Процедура создания приложения и главного окна.
Wt::WApplication* gCreateApplication(const Wt::WEnvironment& env) {
  return new EzopApplication(env);
}

/// Точка входа в программу.
int main(int argc, char** argv) {
  try {
    return Wt::WRun(argc, argv, &gCreateApplication);
  } catch (const std::exception& err) {
    std::cerr << err.what() << "\n";
  }
}

