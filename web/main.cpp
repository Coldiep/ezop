
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
#include <Wt/WTable>
#include <Wt/WLabel>
#include <Wt/WMessageBox>
using namespace Wt;

class MenuElement : public Wt::WContainerWidget {
public:
  virtual bool HasSubMenu() = 0;
  virtual void PopulateMenu(WMenu *menu) {
  }
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
    menu->addItem(Wt::WString::tr("onto.examples"), new Wt::WText(Wt::WString::tr("onto.examples")));

    new Wt::WText(Wt::WString::tr("ontologies-text"), this);
  }

private:
};

class UserLogin : public MenuElement {
public:
  UserLogin() {
    Wt::WVBoxLayout* vert_layout = new WVBoxLayout(this);

    vert_layout->addWidget(new Wt::WText(Wt::WString::tr("login.title")), 0);
    Wt::WTable* layout = new Wt::WTable(this);
    vert_layout->addWidget(layout);

    Wt::WLabel* username_label = new Wt::WLabel(Wt::WString::tr("user.name"), layout->elementAt(0, 0));
    layout->elementAt(0, 0)->resize(Wt::WLength(14, Wt::WLength::FontEx), Wt::WLength::Auto);
    username_ = new Wt::WLineEdit(layout->elementAt(0, 1));
    username_label->setBuddy(username_);

    Wt::WLabel* password_label = new Wt::WLabel(Wt::WString::tr("user.password"), layout->elementAt(1, 0));
    password_ = new Wt::WLineEdit(layout->elementAt(1, 1));
    password_->setEchoMode(Wt::WLineEdit::Password);
    password_label->setBuddy(password_);

    new Wt::WBreak(this);

    Wt::WPushButton* login_button = new Wt::WPushButton(Wt::WString::tr("user.login"), this);
    login_button->clicked().connect(this, &UserLogin::Check);
    vert_layout->addWidget(login_button);
  }

  bool HasSubMenu() {
    return false;
  }

private:
  void Check() {
    user_ = username_->text();
    std::wstring password = password_->text();
    username_->setText("");
    password_->setText("");
    Wt::WMessageBox::show(Wt::WString::tr("message.confirmation"), Wt::WString::tr("login.ok"), Wt::Ok);
  }

  Wt::WLineEdit* username_;
  Wt::WLineEdit* password_;
  std::wstring    user_;
};

class Documentation : public MenuElement {
public:
  Documentation() {
    new Wt::WText(Wt::WString::tr("documentation-text"), this);
  }

  bool HasSubMenu() {
    return false;
  }
};

class MainPage : public MenuElement {
public:
  MainPage() {
    new Wt::WText(Wt::WString::tr("morda"), this);
  }

  bool HasSubMenu() {
    return true;
  }

  void PopulateMenu(WMenu *menu) {
    menu->addItem(Wt::WString::tr("main.news"), new Wt::WText(Wt::WString::tr("main.news")));
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
    Wt::WVBoxLayout* vert_layout = new WVBoxLayout();
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

