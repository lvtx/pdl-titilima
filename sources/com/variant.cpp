#include "..\..\include\pdl_com.h"

LVariant::LVariant(void)
{
    ::VariantInit(this);
}

LVariant::~LVariant(void)
{
    Clear();
}

LVariant::LVariant(__in short i2)
{
    vt = VT_I2;
    iVal = i2;
}

LVariant::LVariant(__in int i4)
{
    vt = VT_I4;
    intVal = i4;
}

LVariant::LVariant(__in float r4)
{
    vt = VT_R4;
    fltVal = r4;
}

LVariant::LVariant(__in double r8, VARTYPE type /* = VT_R8 */)
{
    vt = type;
    dblVal = r8;
}

LVariant::LVariant(__in CY cy)
{
    vt = VT_CY;
    cyVal = cy;
}

LVariant::LVariant(__in BSTR bstr)
{
    vt = VT_BSTR;
    bstrVal = ::SysAllocString(bstr);
}

LVariant::LVariant(__in IDispatch* dispatch)
{
    vt = VT_DISPATCH;
    pdispVal = dispatch;
    if (NULL != pdispVal)
        pdispVal->AddRef();
}

LVariant& LVariant::operator=(__in short i2)
{
    if (VT_I2 != vt)
    {
        Clear();
        vt = VT_I2;
    }

    iVal = i2;
    return *this;
}

LVariant& LVariant::operator=(__in int i4)
{
    if (VT_I4 != vt)
    {
        Clear();
        vt = VT_I4;
    }

    intVal = i4;
    return *this;
}

LVariant& LVariant::operator=(__in float r4)
{
    if (VT_R4 != vt)
    {
        Clear();
        vt = VT_R4;
    }

    fltVal = r4;
    return *this;
}

LVariant& LVariant::operator=(__in CY cy)
{
    if (VT_CY != vt)
    {
        Clear();
        vt = VT_CY;
    }

    cyVal = cy;
    return *this;
}

LVariant& LVariant::operator=(__in BSTR bstr)
{
    Clear();

    vt = VT_BSTR;
    bstrVal = ::SysAllocString(bstr);
    return *this;
}

LVariant& LVariant::operator=(__in IDispatch* dispatch)
{
    Clear();

    vt = VT_DISPATCH;
    pdispVal = dispatch;
    if (NULL != pdispVal)
        pdispVal->AddRef();
    return *this;
}

HRESULT LVariant::Clear(void)
{
    return ::VariantClear(this);
}

void LVariant::put_Date(__in DATE dt)
{
    if (VT_DATE != vt)
    {
        Clear();
        vt = VT_DATE;
    }

    dblVal = dt;
}

void LVariant::put_Double(__in double r8)
{
    if (VT_R8 != vt)
    {
        Clear();
        vt = VT_R8;
    }

    dblVal = r8;
}
