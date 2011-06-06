
#include <Wt/WApplication>
#include <Wt/WBreak>
#include <Wt/WContainerWidget>
#include <Wt/WLineEdit>
#include <Wt/WPushButton>
#include <Wt/WText>
#include <Wt/WMenu>
#include <Wt/WStackedWidget>
using namespace Wt;

class HelloApplication : public WApplication {
public:
  HelloApplication(const WEnvironment& env);

private:
  WLineEdit*  nameEdit_;
  WText*      greeting_;

  void Greet();
};

HelloApplication::HelloApplication(const WEnvironment& env)
  : WApplication(env) {


  setTitle("Test");

  Wt::WContainerWidget* container = new WContainerWidget(root());
  Wt::WStackedWidget* contents = new Wt::WStackedWidget();
  container->addWidget(contents);

  Wt::WMenu* menu = new Wt::WMenu(contents, Wt::Vertical);
  menu->setRenderAsList(true);

  menu->addItem("Item1", new Wt::WText("item1"));
  menu->addItem("Item2", new Wt::WText("item2"));
  menu->addItem("Item3", new Wt::WText("item3"));
  useStyleSheet("main.css");
  //root()->addWidget(container);
  root()->addWidget(menu);

#if 0
 //menu->addItem("Demo", new DemoWidget());
 //menu->addItem(new Wt::WMenuItem("Demo2", new DemoWidget()));
  root()->addWidget(new WText("Your name, please ? "));
  nameEdit_ = new WLineEdit(root());
  nameEdit_->setFocus();

  WPushButton* b = new WPushButton("Greet me.", root());
  b->setMargin(5, Left);

  root()->addWidget(new WBreak());
  greeting_ = new WText(root());

  b->clicked().connect(this, &HelloApplication::Greet);

  nameEdit_->enterPressed().connect(boost::bind(&HelloApplication::Greet, this));
#endif
}

void HelloApplication::Greet() {
  greeting_->setText("Hello there, " + nameEdit_->text());
}

WApplication* createApplication(const WEnvironment& env) {
  return new HelloApplication(env);
}

int main(int argc, char** argv) {
  return WRun(argc, argv, &createApplication);
}

