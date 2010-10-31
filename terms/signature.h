
#pragma once

#include <vector>

#include "type.h"

namespace NTermAlg {

// A term signature is a pair (r, s*), where r -- result type,
// s* string (may be empty) of parameter types.
struct TTermSignature {
    // The sort of the term.
    enum ETermSort {
        UNKNOWN_TERM = 0
        , CLASSIC_TERM
        , VARIABLE_TERM
    };

    typedef TType::TTypeName        TTypeName;
    typedef std::vector<TTypeName>  TTypeNameList;

    ETermSort       Sort;
    std::string     Name;
    TTypeName       ResType;
    TTypeNameList   ParamTypes;

    TTermSignature()
        : Sort(UNKNOWN_TERM)
    {}

    explicit TTermSignature( const std::string& name, ETermSort sort = CLASSIC_TERM )
        : Sort(sort)
        , Name(name)
    {}

    TTermSignature( const std::string& name, TTypeName resType, ETermSort sort = CLASSIC_TERM )
        : Sort(sort)
        , Name(name)
        , ResType(resType)
    {}

    TTermSignature( const std::string& name, TTypeName resType, const TTypeNameList& paramTypes )
        : Sort(CLASSIC_TERM)
        , Name(name)
        , ResType(resType)
        , ParamTypes(paramTypes)
    {}

    void Print( std::ostream& out )
    {
        switch (Sort) {
            UNKNOWN_TERM:
                out << "Unknown: ";
                break;
            CLASSIC_TERM:
                out << "Classic: ";
                break;
            VARIABLE_TERM:
                out << "Variable: ";
                break;
            default:
                break;
        }

        out << ResType << " " << Name << "(";
        bool f = true;
        for (unsigned int i = 0; i < ParamTypes.size(); ++i) {
            if (f) f = false;
            else out << ", ";
            out << ParamTypes[i];
        }
        out << ")\n";
    }
};

} // namespace NTermAlg


