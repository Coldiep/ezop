
#include <Wt/WApplication>
#include <Wt/WBreak>
#include <Wt/WContainerWidget>
#include <Wt/WLineEdit>
#include <Wt/WPushButton>
#include <Wt/WText>
#include <Wt/WMenu>
#include <Wt/WStackedWidget>
#include <Wt/WHBoxLayout>
#include <Wt/WVBoxLayout>
#include <Wt/WSubMenuItem>
using namespace Wt;

class MenuElement : public Wt::WContainerWidget {
public:
  virtual bool HasSubMenu() = 0;
  virtual void PopulateMenu(WMenu *menu) = 0;
};

class OntoOperations : public MenuElement {
public:
  OntoOperations() {
  }

  bool HasSubMenu() {
    return true;
  }

  void PopulateMenu(Wt::WMenu *menu) {
    menu->addItem(Wt::WString::tr("new-ontology-in-kernel-environment"), new Wt::WText(Wt::WString::tr("new-ontology-in-kernel-environment")));
    menu->addItem(Wt::WString::tr("ontology-base"), new Wt::WText(Wt::WString::tr("ontology-base")));
    menu->addItem(Wt::WString::tr("ontology-in-environment"), new Wt::WText(Wt::WString::tr("ontology-in-environment")));
    menu->addItem(Wt::WString::tr("onto-dictionary"), new Wt::WText(Wt::WString::tr("onto-dictionary")));
    menu->addItem(Wt::WString::tr("onto-list"), new Wt::WText(Wt::WString::tr("onto-list")));
    menu->addItem(Wt::WString::tr("template-dictionary"), new Wt::WText(Wt::WString::tr("template-dictionary")));
    menu->addItem(Wt::WString::tr("template-list"), new Wt::WText(Wt::WString::tr("template-list")));

    new Wt::WText(Wt::WString::tr("ontologies-text"), this);
  }

private:
};

class UserLogin : public MenuElement {
  bool HasSubMenu() {
    return false;
  }

  void PopulateMenu(WMenu *menu) {
  }
};

class Documentation : public MenuElement {
public:
  Documentation() {
    new Wt::WText(Wt::WString::tr("documentation-text"), this);
  }

  bool HasSubMenu() {
    return false;
  }

  void PopulateMenu(WMenu *menu) {
  }
};

class MainPage : public MenuElement {
public:
  MainPage() {
    new Wt::WText(Wt::WString::tr("morda"), this);
  }

  bool HasSubMenu() {
    return false;
  }

  void PopulateMenu(WMenu *menu) {
  }
};

class EzopWidget : public Wt::WContainerWidget {
public:
  EzopWidget()
    : contents_stack_(NULL) {

    contents_stack_ = new Wt::WStackedWidget();
    contents_stack_->setOverflow(Wt::WContainerWidget::OverflowAuto);
    contents_stack_->setPositionScheme(Wt::Relative);
    contents_stack_->setStyleClass("contents");

    Wt::WMenu* menu = new Wt::WMenu(contents_stack_, Wt::Vertical);
    menu->setRenderAsList(true);
    menu->setStyleClass("menu");
    menu->setInternalPathEnabled();
    menu->setInternalBasePath("/");

    AddToMenu(menu, "main-page", new MainPage());
    AddToMenu(menu, "ontologies", new OntoOperations());
    AddToMenu(menu, "documentation", new Documentation());
    AddToMenu(menu, "user-login", new UserLogin());

    Wt::WHBoxLayout* horiz_layout = new WHBoxLayout();
    Wt::WVBoxLayout* vert_layout = new WVBoxLayout;
    Wt::WVBoxLayout* main_layout = new Wt::WVBoxLayout(this);

    main_layout->addWidget(new Wt::WText(Wt::WString::tr("sidebar-title")));
    main_layout->addLayout(horiz_layout, 1);
    horiz_layout->addWidget(menu, 0);
    horiz_layout->addLayout(vert_layout, 1);
    vert_layout->addWidget(contents_stack_, 1);
    horiz_layout->setResizable(0, true);
  }

private:
  void AddToMenu(Wt::WMenu* menu, const char* name, MenuElement* element) {
    if (element->HasSubMenu()) {
      Wt::WSubMenuItem* smi = new WSubMenuItem(Wt::WString::tr(name), element);
      Wt::WMenu* sub_menu = new WMenu(contents_stack_, Wt::Vertical, 0);
      sub_menu->setRenderAsList(true);

      smi->setSubMenu(sub_menu);
      menu->addItem(smi);

      sub_menu->setInternalPathEnabled();
      sub_menu->setInternalBasePath("/" + smi->pathComponent());
      sub_menu->setStyleClass("menu submenu");

      element->PopulateMenu(sub_menu);
    } else {
      menu->addItem(Wt::WString::tr(name), element);
    }
  }

  Wt::WStackedWidget* contents_stack_;
};

WApplication* createApplication(const WEnvironment& env) {
  Wt::WApplication* app = new WApplication(env);
  app->setCssTheme("polished");

  app->messageResourceBundle().use(app->appRoot() + "rus");

  Wt::WHBoxLayout* layout = new Wt::WHBoxLayout(app->root());
  layout->setContentsMargins(0, 0, 0, 0);
  layout->addWidget(new EzopWidget());

  app->setTitle(Wt::WString::tr("project-title"));
  app->addMetaHeader("viewport", "width=700, height=1200");
  app->useStyleSheet("main.css");

  return app;
}

int main(int argc, char** argv) {
  return WRun(argc, argv, &createApplication);
}

