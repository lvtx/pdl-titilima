///////////////////////////////////////////////////////////////////////////////
// FileName:    bstr.cpp
// Created:     2009/06/05
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: COM BSTR ×Ö·û´®ÀàÊµÏÖ
///////////////////////////////////////////////////////////////////////////////

#include "..\..\include\pdl_com.h"

LBStr::LBStr(void)
{
    m_str = NULL;
}

LBStr::LBStr(__in PCSTR lpString)
{
    LStringW str = lpString;
    m_str = ::SysAllocString(str);
}

LBStr::LBStr(__in PCWSTR lpString)
{
    m_str = ::SysAllocString(lpString);
}

LBStr::~LBStr(void)
{
    if (NULL != m_str)
        ::SysFreeString(m_str);
}

LBStr& LBStr::operator=(__in PCSTR lpString)
{
    Copy(lpString);
    return *this;
}

LBStr& LBStr::operator=(__in PCWSTR lpString)
{
    Copy(lpString);
    return *this;
}

LBStr::operator BSTR(void)
{
    return m_str;
}

LBStr::operator PCWSTR(void)
{
    return m_str;
}

BSTR* LBStr::operator&(void)
{
    return &m_str;
}

void LBStr::Attach(__in BSTR bstr)
{
    if (NULL != m_str)
        ::SysFreeString(m_str);
    m_str = bstr;
}

void LBStr::Copy(__in PCSTR lpString)
{
    LStringW str = lpString;
    Copy(str);
}

void LBStr::Copy(__in PCWSTR lpString)
{
    if (NULL != m_str)
        ::SysFreeString(m_str);
    m_str = ::SysAllocString(lpString);
}

BSTR LBStr::Detach(void)
{
    BSTR ret = m_str;
    m_str = NULL;
    return ret;
}

UINT LBStr::GetLength(void)
{
    if (NULL == m_str)
        return 0;
    else
        return ::SysStringLen(m_str);
}

BOOL LBStr::IsEmpty(void)
{
    return 0 == GetLength();
}
