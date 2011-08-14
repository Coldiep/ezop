
#include <Wt/WHBoxLayout>

#include <web/site/banner.h>
using ezop::web::Banner;

Banner::Banner(const Wt::WString& user_name, Wt::WContainerWidget* parent)
  : Wt::WContainerWidget(parent)
  , panel_(new Wt::WTemplate()) {
  panel_->setTemplateText(tr("ezop-header"));
  panel_->bindString("user", user_name);
}

void Banner::UpdateUser(const Wt::WString& user_name) {
  panel_->bindString("user", user_name);
  panel_->refresh();
}
