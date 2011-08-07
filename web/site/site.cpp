
#include <Wt/WApplication>
#include <Wt/WContainerWidget>
#include <Wt/WServer>

#include <web/site/view/morda.h>

using namespace Wt;

static const char* kEzopUrl = "/ezop";

class EzopApplication : public WApplication {
public:
  EzopApplication(const WEnvironment& env) 
    : WApplication(env) {
    root()->addWidget(new ezop::web::Morda("/", WApplication::appRoot() + "ezop.db"));
    useStyleSheet("css/ezop.css");
  }
};

WApplication* createApplication(const WEnvironment& env) {
  return new EzopApplication(env);
}

int main(int argc, char** argv) {
  try {
    return WRun(argc, argv, &createApplication);
  } catch (const std::exception& err) {
    std::cerr << err.what() << "\n";
  }
}

