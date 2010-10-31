
#pragma once

#include <stdexcept>

#include "signature.h"

namespace NTermAlg {

// The term representation.
struct TTerm {
    // The list of child terms.
    typedef std::vector<TTerm*>     TTermList;

    TTermSignature  Signature;
    TTermList       Children;

    TTerm()
    {}

    explicit TTerm( const TTermSignature signature )
        : Signature(signature)
        , Children(signature.ParamTypes.size(), 0)
    {}

    void SetTerm( TTerm* term, unsigned int place )
    {
        if (place >= Children.size())
            throw std::runtime_error("Bad index of term passed.");

        if (term->Signature.ResType != Signature.ParamTypes[place])
            throw std::runtime_error("The term's type does not the same as type into term signature.");

        if (Children[place])
            throw std::runtime_error("The passed term has already been set to this place.");

        Children[place] = term;
    }

    void PrintTerm( std::ostream& out, const TTerm* term ) const
    {
        out << term->Signature.Name << "(";
        bool f = true;
        for (unsigned int i = 0; i < Children.size(); ++i) {
            if (f) f = false;
            else out << ", ";
            PrintTerm(out, Children[i]);
        }
    }

    void Print( std::ostream& out ) const
    {
        PrintTerm(out, this);
    }
};

} // namespace NTermAlg


