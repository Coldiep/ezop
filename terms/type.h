
#pragma once

#include <string>
#include <set>
#include <map>
#include <iostream>
#include <vector>

#include "rc_smart_ptr.h"

namespace NTermAlg {

// Type is the most fundamental notion in term algebra.
// Every type has a name, which is unique in the set of
// all types in the storage. Every type has parent and
// child types enumerated in the special list.
struct TType {
    typedef std::string             TTypeName;
    typedef std::set <TTypeName>    TTypeNameList;

    // Parent and child types list.
    TTypeNameList   Parents, Children;

    // The type's name.
    TTypeName     Name;

    // The type description.
    std::string Description;

    // Default construction.
    TType()
    {}

    // Construction.
    explicit TType( const TTypeName& name, const std::string& desc )
        : Name(name)
        , Description(desc)
    {}
};

// The set of types. A user can add or remove type to the set.
// The class provides service to search type by using passed name
// and save the set to file.
class TTypeSet {
    friend struct TTermSignature;

public:
    typedef NUtils::TRcSmartPtr<TType>                      TTypePtr;
    typedef std::map<TType::TTypeName, TTypePtr>            TTypePtrList;
    typedef TType::TTypeName                                TTypeName;
    typedef TType::TTypeNameList                            TTypeNameList;

private:
    TTypePtrList TypePtrList;

public:
    void AddType( const TTypeName& typeName );
    void AddType( const TTypeName& typeName, const std::string& comment );
    void AddType( const TTypeName& typeName, const std::string& comment, const TTypeNameList& parents );

    void RemoveType (const TTypeName& typeName );
    bool Find (const TTypeName& typeName, TTypePtr& typePtr ) const;

    void Load( const std::string& fileName );
    void Save( const std::string& fileName );

    void Print( std::ostream& out );
};

} // namespace NTermAlg


