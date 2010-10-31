
#include <stdexcept>
#include <sstream>
#include <fstream>
#include <vector>

#include "type.h"
using namespace NTermAlg;

void TTypeSet::AddType( const TTypeName& typeName, const std::string& comment, const TTypeNameList& parents  )
{
    // Check if the type having passed name is in the types set.
    if (TypePtrList.find(typeName) != TypePtrList.end()) {
        std::stringstream err;
        err << "The type having passed name \"" << typeName << "\" has already been added";
        throw std::runtime_error(err.str().c_str());
    }

    // Check every parent type in the types set.
    for (TTypeNameList::const_iterator pit = parents.begin(); pit != parents.end(); ++pit) {
        TTypePtrList::iterator it = TypePtrList.find(*pit);
        if (it == TypePtrList.end()) {
            std::stringstream err;
            err << "The passed parent type with name \"" << *pit << "\" is not in the set";
            throw std::runtime_error(err.str().c_str());
        }
    }

    TTypePtr tp = new TType(typeName, comment);
    TypePtrList[typeName] = tp;
    for (TTypeNameList::const_iterator pit = parents.begin(); pit != parents.end(); ++pit) {
        tp->Parents.insert(*pit);
        TypePtrList[*pit]->Children.insert(typeName);
    }
}

void TTypeSet::AddType( const TTypeName& typeName, const std::string& comment )
{
    AddType(typeName, comment, TTypeNameList());
}

void TTypeSet::AddType( const TTypeName& typeName )
{
    AddType(typeName, TTypeName(), TTypeNameList());
}

void TTypeSet::RemoveType (const TTypeName& typeName )
{
    TTypePtrList::iterator it = TypePtrList.find(typeName);
    if (it == TypePtrList.end()) {
        std::stringstream err;
        err << "The type having passed name \"" << typeName << "\" is not in the set";
        throw std::runtime_error(err.str().c_str());
    }

    TypePtrList.erase(it);
}

bool TTypeSet::Find (const TTypeName& typeName, TTypePtr& typePtr ) const
{
    TTypePtrList::const_iterator it = TypePtrList.find(typeName);
    if (it != TypePtrList.end()) {
        typePtr = it->second;
        return true;
    }

    return false;
}

namespace NUtils {

unsigned int Split( const std::string& str, std::vector<std::string>& res, char delim)
{
    if (str.empty()) {
        return 0;
    }

    std::string peace;
    for (unsigned int i = 0; i < str.size(); ++i) {
        if (delim == str[i]) {
            res.push_back(peace);
            peace = "";
        }
        else {
            peace += str[i];
        }
    }
    res.push_back(peace);

    return res.size();
}

} // namespace NUtils

void TTypeSet::Load( const std::string& fileName )
{
    std::ifstream file(fileName.c_str());
    if (not file.is_open()) {
        std::stringstream err;
        err << "The file having passed name \"" << fileName << "\" cannot be open to read";
        throw std::runtime_error(err.str().c_str());
    }

    std::string lineStr;
    while (std::getline (file, lineStr)) {
        std::vector <std::string> fields;
        if (NUtils::Split(lineStr, fields, '\t') != 3) {
            std::stringstream err;
            err << "Invalid file format: \"" << lineStr << "\"";
            throw std::runtime_error(err.str().c_str());
        } else if (fields[0].empty()) {
            std::stringstream err;
            err << "Invalid file format: empty type name \"" << lineStr[0] << "\"";
            throw std::runtime_error(err.str().c_str());
        }

        std::vector <std::string> pn;
        NUtils::Split(fields[2], pn, ' ');
        TTypeNameList pnl;
        for (unsigned int i = 0; i < pn.size(); ++i) pnl.insert(pn[i]);

        AddType(fields[0], fields[1], pnl);
    }
}

void TTypeSet::Save( const std::string& fileName )
{
    std::ofstream file(fileName.c_str());
    if (not file.is_open()) {
        std::stringstream err;
        err << "The file having passed name \"" << fileName << "\" cannot be open to write";
        throw std::runtime_error(err.str().c_str());
    }

    for (TTypePtrList::iterator it = TypePtrList.begin(); it != TypePtrList.end(); ++it) {
        const TTypePtr& typePtr = it->second;
        file << typePtr->Name;

        // Out comment
        file << "\t" << typePtr->Description;

        // Out parent types.
        file << "\t";
        bool first = true;
        for (TType::TTypeNameList::const_iterator pit = typePtr->Parents.begin(); pit != typePtr->Parents.end(); ++pit) {
            if (first) first = false;
            else file << " ";

            file << *pit;
        }

        // Endup the string of the type.
        file << "\n";
    }
}

void TTypeSet::Print( std::ostream& out )
{
    for (TTypePtrList::iterator it = TypePtrList.begin(); it != TypePtrList.end(); ++it) {
        const TTypePtr& typePtr = it->second;
        out << "\n--------------------------------------\n";

        out << "\tName: \"" << typePtr->Name << "\"\n";
        // Out comment
        out << "\tDescription: \"" << typePtr->Description << "\"\n";

        // Out parent types.
        out << "\tParent types:";
        bool f = true;
        for (TType::TTypeNameList::const_iterator pit = typePtr->Parents.begin(); pit != typePtr->Parents.end(); ++pit) {
            if (f) f = false;
            else out << ",";
            out << " \"" << *pit << "\"";
        }
        out << "\n";

        // Out child types.
        out << "\tChild types:";
        f = true;
        for (TType::TTypeNameList::const_iterator cit = typePtr->Children.begin(); cit != typePtr->Children.end(); ++cit) {
            if (f) f = false;
            else out << ",";
            out << " \"" << *cit << "\"";
        }

        // Endup the string of the type.
        out << "\n";
    }
}

