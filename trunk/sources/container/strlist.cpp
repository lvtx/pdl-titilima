#include <pdl_container.h>
#include "..\common\lock.h"
#include <pdl_file.h>
#include <pdl_string.h>

void LStrListA_Copy(void* dst, const void* src)
{
    PSTR* ppdst = (PSTR*)dst;
    PCSTR psrc = (PCSTR)src;
    *ppdst = new char[lstrlenA(psrc) + 1];
    lstrcpyA(*ppdst, psrc);
}

void LStrListA_Destroy(void* ptr)
{
    PSTR* pp = (PSTR*)ptr;
    if (NULL != *pp)
    {
        delete [] *pp;
        *pp = NULL;
    }
}

LStrListA::LStrListA(__in ILock* lock /* = NULL */) : LPtrList()
{
    LPtrList::Create(sizeof(PCSTR), LStrListA_Copy, LStrListA_Destroy, lock);
}

PCSTR LStrListA::AddHead(__in PCSTR lpString)
{
    PCSTR* ret = (PCSTR*)LPtrList::AddHead(lpString);
    return *ret;
}

PCSTR LStrListA::AddTail(__in PCSTR lpString)
{
    PCSTR* ret = (PCSTR*)LPtrList::AddTail(lpString);
    return *ret;
}

PCSTR LStrListA::GetAt(__in LIterator it)
{
    PCSTR ret = NULL;
    if (LPtrList::GetAt(it, &ret))
        return ret;
    else
        return FALSE;
}

LIterator LStrListA::GetHeadIterator(void)
{
    return LPtrList::GetHeadIterator();
}

void LStrListA::GetNextIterator(__inout LIterator* it)
{
    return LPtrList::GetNextIterator(it);
}

void LStrListA::GetPrevIterator(__inout LIterator* it)
{
    return LPtrList::GetPrevIterator(it);
}

LIterator LStrListA::GetTailIterator(void)
{
    return LPtrList::GetTailIterator();
}

PCSTR LStrListA::InsertAfter(__in LIterator it, __in PCSTR lpString)
{
    PCSTR* ret = (PCSTR*)LPtrList::InsertAfter(it, lpString);
    return *ret;
}

PCSTR LStrListA::InsertBefore(__in LIterator it, __in PCSTR lpString)
{
    PCSTR* ret = (PCSTR*)LPtrList::InsertBefore(it, lpString);
    return *ret;
}

DWORD LStrListA::LoadFromFile(__in PCSTR lpFile, __in DWORD dwFlags)
{
    LTxtFile file;
    if (!file.Open(lpFile))
        return 0;

    if (SLFILE_CLEAR & dwFlags)
        Clear();

    DWORD ret = 0;
    LStringA str;
    while (!file.Eof())
    {
        file.ReadLn(&str);
        if (('\0' != str[0]) || (SLFILE_INCLUDENULL & dwFlags))
        {
            AddTail(str);
            ++ret;
        }
    }
    return ret;
}

DWORD LStrListA::LoadFromFile(__in PCWSTR lpFile, __in DWORD dwFlags)
{
    LTxtFile file;
    if (!file.Open(lpFile))
        return 0;

    if (SLFILE_CLEAR & dwFlags)
        Clear();

    DWORD ret = 0;
    LStringA str;
    while (!file.Eof())
    {
        file.ReadLn(&str);
        if (('\0' != str[0]) || (SLFILE_INCLUDENULL & dwFlags))
        {
            AddTail(str);
            ++ret;
        }
    }
    return ret;
}

DWORD LStrListA::SaveToFile(__in PCSTR lpFile, __in DWORD dwFlags)
{
    if (NULL == m_itHead)
        return 0;

    LTxtFile file;
    BOOL b;
    if (SLFILE_CLEAR & dwFlags)
        b = file.Reset(lpFile);
    else
        b = file.Append(lpFile);
    if (!b)
        return 0;

    DWORD ret = 0;
    LIterator it = GetHeadIterator();
    while (NULL != it)
    {
        PCSTR str = GetAt(it);
        if (('\0' != *str) || (SLFILE_INCLUDENULL & dwFlags))
        {
            file.WriteLn(str);
            ++ret;
        }
        GetNextIterator(&it);
    }
    return ret;
}

DWORD LStrListA::SaveToFile(__in PCWSTR lpFile, __in DWORD dwFlags)
{
    if (NULL == m_itHead)
        return 0;

    LTxtFile file;
    BOOL b;
    if (SLFILE_CLEAR & dwFlags)
        b = file.Reset(lpFile);
    else
        b = file.Append(lpFile);
    if (!b)
        return 0;

    DWORD ret = 0;
    LIterator it = GetHeadIterator();
    while (NULL != it)
    {
        PCSTR str = GetAt(it);
        if (('\0' != *str) || (SLFILE_INCLUDENULL & dwFlags))
        {
            file.WriteLn(str);
            ++ret;
        }
        GetNextIterator(&it);
    }
    return ret;
}

void LStrListA::SetAt(__in LIterator it, __in PCSTR lpString)
{
    LPtrList::SetAt(it, lpString);
}
