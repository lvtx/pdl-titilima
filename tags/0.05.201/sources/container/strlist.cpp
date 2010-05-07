#include "..\..\include\pdl_container.h"
#include "..\common\lock.h"
#include "..\..\include\pdl_file.h"
#include "..\..\include\pdl_string.h"

void LStrList_Copy(void* dst, const void* src)
{
    PSTR* ppdst = (PSTR*)dst;
    PCSTR psrc = (PCSTR)src;
    *ppdst = new char[lstrlenA(psrc) + 1];
    lstrcpyA(*ppdst, psrc);
}

void LStrList_Destroy(void* ptr)
{
    PSTR* pp = (PSTR*)ptr;
    if (NULL != *pp)
    {
        delete [] *pp;
        *pp = NULL;
    }
}

LStrList::LStrList(__in ILock* lock /* = NULL */) : LPtrList()
{
    m_lock = LDummyLock::Get();
    LPtrList::Create(sizeof(PCSTR), LStrList_Copy, LStrList_Destroy, lock);
}

LIterator LStrList::AddHead(__in PCSTR lpString)
{
    LPtrList::AddHead(lpString);
    return m_itHead;
}

LIterator LStrList::AddHead(__in PCWSTR lpString)
{
    LStringA strA = lpString;
    return AddHead(strA);
}

LIterator LStrList::AddTail(__in PCSTR lpString)
{
    LPtrList::AddTail(lpString);
    return m_itTail;
}

LIterator LStrList::AddTail(__in PCWSTR lpString)
{
    LStringA strA = lpString;
    return AddTail(strA);
}

PCSTR LStrList::GetAt(__in LIterator it)
{
    PCSTR ret = NULL;
    if (LPtrList::GetAt(it, &ret))
        return ret;
    else
        return FALSE;
}

BOOL LStrList::GetAt(__in LIterator it, __out LStringA* str)
{
    PCSTR ret = GetAt(it);
    if (NULL == ret)
        return FALSE;

    str->Copy(ret);
    return TRUE;
}

BOOL LStrList::GetAt(__in LIterator it, __out LStringW* str)
{
    PCSTR ret = GetAt(it);
    if (NULL == ret)
        return FALSE;

    str->Copy(ret);
    return TRUE;
}

LIterator LStrList::InsertAfter(__in LIterator it, __in PCSTR lpString)
{
    return LPtrList::InsertAfter(it, lpString);
}

LIterator LStrList::InsertAfter(__in LIterator it, __in PCWSTR lpString)
{
    LStringA strA = lpString;
    return InsertAfter(it, strA);
}

LIterator LStrList::InsertBefore(__in LIterator it, __in PCSTR lpString)
{
    return LPtrList::InsertBefore(it, lpString);
}

LIterator LStrList::InsertBefore(__in LIterator it, __in PCWSTR lpString)
{
    LStringA strA = lpString;
    return InsertBefore(it, strA);
}

BOOL LStrList::IsEmpty(__in LIterator it)
{
    PCSTR p = GetAt(it);
    if (NULL == p || '\0' == *p)
        return TRUE;
    else
        return FALSE;
}

DWORD LStrList::LoadFromFile(__in PCSTR lpFile, __in DWORD dwFlags)
{
    LTxtFile file;
    if (!file.Open(lpFile, LTxtFile::modeReadWrite))
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

DWORD LStrList::LoadFromFile(__in PCWSTR lpFile, __in DWORD dwFlags)
{
    LTxtFile file;
    if (!file.Open(lpFile, LTxtFile::modeReadWrite))
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

DWORD LStrList::SaveToFile(__in PCSTR lpFile, __in DWORD dwFlags)
{
    if (NULL == m_itHead)
        return 0;

    LTxtFile::MODE mode;
    if (SLFILE_CLEAR & dwFlags)
        mode = LTxtFile::modeReset;
    else
        mode = LTxtFile::modeAppend;
    LTxtFile file;
    if (!file.Open(lpFile, mode))
        return 0;

    DWORD ret = 0;
    LIterator it = GetHeadIterator();
    while (NULL != it)
    {
        PCSTR str = GetAt(it);
        LIterator itNext = GetNextIterator(it);
        if (('\0' != *str) || (SLFILE_INCLUDENULL & dwFlags))
        {
            if (NULL != itNext)
                file.WriteLn(str);
            else
                file.Write(str);
            ++ret;
        }

        it = itNext;
    }

    return ret;
}

DWORD LStrList::SaveToFile(__in PCWSTR lpFile, __in DWORD dwFlags)
{
    if (NULL == m_itHead)
        return 0;

    LTxtFile::MODE mode;
    if (SLFILE_CLEAR & dwFlags)
        mode = LTxtFile::modeReset;
    else
        mode = LTxtFile::modeAppend;
    LTxtFile file;
    if (!file.Open(lpFile, mode))
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
        it = GetNextIterator(it);
    }

    return ret;
}

void LStrList::SetAt(__in LIterator it, __in PCSTR lpString)
{
    LPtrList::SetAt(it, lpString);
}

void LStrList::SetAt(__in LIterator it, __in PCWSTR lpString)
{
    LStringA strA = lpString;
    SetAt(it, strA);
}
