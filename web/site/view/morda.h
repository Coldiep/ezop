
#pragma once

#include <Wt/WCompositeWidget>

namespace ezop { namespace web {

class Site;

class Morda : public Wt::WCompositeWidget {
public:
  Morda(const std::string& base_path, const std::string& sqlite_db, Wt::WContainerWidget* parent = NULL);

  Wt::WString User();
  void Login(const std::string& user);
  void Logout();

  Wt::Signal<Wt::WString>& UserChanged() {
    return user_changed_;
  }

private:
  Site* impl_;
  Wt::Signal<Wt::WString> user_changed_;
};

}} // namespace ezop, web.
