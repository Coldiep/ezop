
#pragma once

namespace NUtils{

template <typename T>
class TRcSmartPtr {
    typedef unsigned int TCounterType;

public:
     TRcSmartPtr()
        : Ptr (0)
        , Counter (0)
    {}

    TRcSmartPtr( T* ptr )
        : Ptr (ptr)
        , Counter (new TCounterType (1))
    {}
        
    template <typename Other>
    TRcSmartPtr(const TRcSmartPtr <Other>& src)
        : Ptr (src.Ptr)
        , Counter (src.Counter)
    {
        if (Counter) ++ *Counter;
    }
        
    ~TRcSmartPtr()
    {
        if (Counter) -- *Counter;
        if (Counter == 0 || *Counter == 0){
            delete Ptr;
            delete Counter;
        }
    }

    TRcSmartPtr( const TRcSmartPtr <T>& src )
        : Ptr (src.Ptr)
        , Counter (src.Counter)
    {
        if (Counter) ++ *Counter;
    }

    TRcSmartPtr <T>& operator = ( const TRcSmartPtr <T>& src )
    {
        if (this == &src) {
            return *this;
        }
        if (Counter) -- *Counter;
        if (Counter == 0 || *Counter == 0){
            delete Ptr;
            delete Counter;
        }
        Ptr = src.Ptr;
        Counter = src.Counter;

        if (Counter) ++ *Counter;

        return *this;
    }

    T* Get() const
    {
        return Ptr;
    }

    const T* GetConst() const
    {
        return Ptr;
    }

    const T& operator * () const { return *Ptr; }
    const T* operator-> () const { return Ptr;  }
    T& operator * ()        { return *Ptr; }
    T* operator -> ()       { return Ptr;  }
    operator void* () const { return Ptr; }

private:
    T* Ptr;
    TCounterType* Counter;
 
    TRcSmartPtr( T* ptr, TCounterType* coun )
        : Ptr (ptr)
        , Counter (coun)
    {}
 
    template <typename Other, typename Type> friend TRcSmartPtr <Other> static_pointer_cast(TRcSmartPtr <Type>& sp);
    template <typename Other, typename Type> friend TRcSmartPtr <Other> dynamic_pointer_cast(TRcSmartPtr <Type>& sp);
    template <typename Other> friend class TRcSmartPtr;
};

template <typename Other, typename T>
TRcSmartPtr <Other> static_pointer_cast(TRcSmartPtr <T>& sp)
{
    if (sp.Counter) {
        ++ *sp.Counter;
    }
    return TRcSmartPtr <Other> (static_cast <Other*> (sp.Ptr), sp.Counter);
}
        
template <typename Other, typename T>
TRcSmartPtr <Other> dynamic_pointer_cast(TRcSmartPtr <T>& sp)
{
    Other* dp = dynamic_cast <Other*> (sp.Ptr);
    if (dp == 0) {
        return TRcSmartPtr <Other> ();
    }
    if (sp.Counter) {
        ++ *sp.Counter;
    }
    return TRcSmartPtr <Other> (dp, sp.Counter);
}

} // namespace NUtils

