
#include <Wt/Dbo/Impl>

#include <web/model/ontology.h>
#include <web/model/user.h>
#include <web/model/project.h>

DBO_INSTANTIATE_TEMPLATES(ezop::web::Project);

namespace ezop { namespace web {

Ontologies Project::GetOntologies() {
  return ontologies_.find().where("project = ?").bind(name_).orderBy("date desc");
}

}} // namespace ezop, web.

