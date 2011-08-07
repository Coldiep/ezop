
//#include <Wt/WRandom>
#include <Wt/Dbo/Impl>

#include <web/site/model/project.h>
#include <web/site/model/ontology.h>

#include <web/site/model/user.h>

DBO_INSTANTIATE_TEMPLATES(ezop::web::User);

namespace ezop { namespace web {

void User::SetPassword(const std::string& password) {
  password_ = password;
}

bool User::Authenticate(const std::string& password) const {
  return password_ == password;
}

std::string User::GenerateToken() {
  //token_ = Wt::WRandom::generateId(16);
  return token_;
}

Projects User::GetProjects() {
  return projects_.find().where("owner = ?").bind(name_).orderBy("date desc");
}

Ontologies User::GetOntologies() {
  return ontologies_.find().where("author = ?").bind(name_).orderBy("date desc");
}

}} // namespace ezop, web.
