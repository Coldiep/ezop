
#include <Wt/WApplication>
#include <Wt/WBreak>
#include <Wt/WContainerWidget>
#include <Wt/WLineEdit>
#include <Wt/WPushButton>
#include <Wt/WText>

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
  setTitle("Hello world");                               // application title

  root()->addWidget(new WText("Your name, please ? "));  // show some text
  nameEdit_ = new WLineEdit(root());                     // allow text input
  nameEdit_->setFocus();                                 // give focus

  WPushButton* b = new WPushButton("Greet me.", root()); // create a button
  b->setMargin(5, Left);                                 // add 5 pixels margin

  root()->addWidget(new WBreak());                       // insert a line break
  greeting_ = new WText(root());                         // empty text

  b->clicked().connect(this, &HelloApplication::Greet);

  nameEdit_->enterPressed().connect(boost::bind(&HelloApplication::greet, this));
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

