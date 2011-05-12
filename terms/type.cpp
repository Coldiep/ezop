
#include <stdexcept>
#include <sstream>
#include <fstream>
#include <vector>

#include <terms/type.h>
using ezop::terms::Type;
using ezop::terms::TypeSet;

void TypeSet::AddType(const std::string& name, const std::string& desc) {
  // Проверяем, не имеется ли уже тип с переданным именем во множестве.
  if (type_list_.find(name) != type_list_.end()) {
    std::stringstream err;
    err << "The type having passed name \"" << name << "\" has already been added";
    throw std::invalid_argument(err.str().c_str());
  }

  // Добавляем тип.
  type_list_[name] = Type::Ptr(new Type(name, desc));
}

void TypeSet::RemoveType(const std::string& name) {
  TypeList::iterator it = type_list_.find(name);
  if (it == type_list_.end()) {
    std::stringstream err;
    err << "The type having passed name \"" << name << "\" is not in the set";
    throw std::runtime_error(err.str().c_str());
  }

  type_list_.erase(it);
}

const Type* TypeSet::Find(const std::string& name) const {
  TypeList::const_iterator it = type_list_.find(name);
  if (it != type_list_.end()) {
    return it->second.get();
  }
  return NULL;
}

void TypeSet::Print(std::ostream& out) const {
  for (TypeList::const_iterator it = type_list_.begin(); it != type_list_.end(); ++it) {
    out << "(\"" << it->second->name_ << "\", \"" << it->second->description_ << "\")\n";
  }
}

