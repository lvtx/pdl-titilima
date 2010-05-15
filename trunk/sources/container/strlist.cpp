#include "..\..\include\pdl_container.h"
#include "..\..\include\pdl_util.h"
#include "..\..\include\pdl_file.h"
#include "..\..\include\pdl_string.h"

#define SLFLAG_UNICODE  0x00000001

void LStrList_CopyA(void* dst, const void* src)
{
    PSTR* ppdst = (PSTR*)dst;
    PCSTR psrc = *((PCSTR*)src);
    *ppdst = LStringA::AllocString(psrc);
}

void LStrList_CopyW(void* dst, const void* src)
{
    PWSTR* ppdst = (PWSTR*)dst;
    PCWSTR psrc = *((PCWSTR*)src);
    *ppdst = LStringW::AllocString(psrc);
}

void LStrList_DestroyA(void* ptr)
{
    PSTR p = *((PSTR*)ptr);
    if (NULL != p)
        LStringA::FreeString(p);
}

void LStrList_DestroyW(void* ptr)
{
    PWSTR p = *((PWSTR*)ptr);
    if (NULL != p)
        LStringW::FreeString(p);
}

LStrList::LStrList(__in ILock* lock, __in BOOL bUnicode) : LPtrList()
{
    m_dwFlags = 0;
    CopyPtr pfnCopy = LStrList_CopyA;
    DestructPtr pfnDestruct = LStrList_DestroyA;
    if (bUnicode)
    {
        m_dwFlags = SLFLAG_UNICODE;
        pfnCopy = LStrList_CopyW;
        pfnDestruct = LStrList_DestroyW;
    }
    LPtrList::Create(sizeof(PVOID), pfnCopy, pfnDestruct, lock);
}

LIterator LStrList::AddHead(__in PCSTR lpString)
{
    LAutoLock al(m_lock);
    if (IsUnicode())
    {
        LStringW str = lpString;
        AddHead(str);
    }
    else
    {
        LPtrList::AddHead(&lpString);
    }
    return m_itHead;
}

LIterator LStrList::AddHead(__in PCWSTR lpString)
{
    LAutoLock al(m_lock);
    if (IsUnicode())
    {
        LPtrList::AddHead(&lpString);
    }
    else
    {
        LStringA str = lpString;
        AddHead(str);
    }
    return m_itHead;
}

LIterator LStrList::AddTail(__in PCSTR lpString)
{
    LAutoLock al(m_lock);
    if (IsUnicode())
    {
        LStringW str = lpString;
        AddTail(str);
    }
    else
    {
        LPtrList::AddTail(&lpString);
    }
    return m_itTail;
}

LIterator LStrList::AddTail(__in PCWSTR lpString)
{
    LAutoLock al(m_lock);
    if (IsUnicode())
    {
        LPtrList::AddTail(&lpString);
    }
    else
    {
        LStringA str = lpString;
        AddTail(str);
    }
    return m_itTail;
}

BOOL LStrList::GetAt(__in LIterator it, __out LStringA* str)
{
    PVOID pv = GetRawString(it);
    if (NULL == pv)
        return FALSE;

    if (IsUnicode())
        str->Copy((PCWSTR)pv);
    else
        str->Copy((PCSTR)pv);
    return TRUE;
}

BOOL LStrList::GetAt(__in LIterator it, __out LStringW* str)
{
    PVOID pv = GetRawString(it);
    if (NULL == pv)
        return FALSE;

    if (IsUnicode())
        str->Copy((PCWSTR)pv);
    else
        str->Copy((PCSTR)pv);
    return TRUE;
}

PVOID LStrList::GetRawString(__in LIterator it)
{
    PVOID ret = NULL;
    if (!LPtrList::GetAt(it, &ret))
        return NULL;

    PDLASSERT(NULL != ret);
    return ret;
}

LIterator LStrList::InsertAfter(__in LIterator it, __in PCSTR lpString)
{
    LIterator ret = NULL;
    LAutoLock al(m_lock);
    if (IsUnicode())
    {
        LStringW str = lpString;
        ret = InsertAfter(it, str);
    }
    else
    {
        ret = LPtrList::InsertAfter(it, &lpString);
    }
    return ret;
}

LIterator LStrList::InsertAfter(__in LIterator it, __in PCWSTR lpString)
{
    LIterator ret = NULL;
    LAutoLock al(m_lock);
    if (IsUnicode())
    {
        ret = LPtrList::InsertAfter(it, &lpString);
    }
    else
    {
        LStringA str = lpString;
        ret = InsertAfter(it, str);
    }
    return ret;
}

LIterator LStrList::InsertBefore(__in LIterator it, __in PCSTR lpString)
{
    LIterator ret = NULL;
    LAutoLock al(m_lock);
    if (IsUnicode())
    {
        LStringW str = lpString;
        ret = InsertBefore(it, str);
    }
    else
    {
        ret = LPtrList::InsertBefore(it, &lpString);
    }
    return ret;
}

LIterator LStrList::InsertBefore(__in LIterator it, __in PCWSTR lpString)
{
    LIterator ret = NULL;
    LAutoLock al(m_lock);
    if (IsUnicode())
    {
        ret = LPtrList::InsertBefore(it, &lpString);
    }
    else
    {
        LStringA str = lpString;
        ret = InsertBefore(it, str);
    }
    return ret;
}

BOOL LStrList::IsEmptyLine(__in LIterator it)
{
    PVOID pv = GetRawString(it);
    if (NULL == pv)
        return TRUE;

    if (IsUnicode())
        return L'\0' == *((PCWSTR)pv);
    else
        return '\0' == *((PCSTR)pv);
}

PDLINLINE BOOL LStrList::IsUnicode(void)
{
    return 0 != (SLFLAG_UNICODE & m_dwFlags);
}

DWORD LStrList::LoadFromFile(__in PCSTR lpFile, __in DWORD dwFlags)
{
    LTxtFile file;
    if (!file.Open(lpFile, LTxtFile::modeReadWrite))
        return 0;

    if (SLFILE_CLEAR & dwFlags)
        Clear();

    BOOL bIncludeNull = (0 != (SLFILE_INCLUDENULL & dwFlags));
    return LoadFromFile(&file, bIncludeNull);
}

DWORD LStrList::LoadFromFile(__in PCWSTR lpFile, __in DWORD dwFlags)
{
    LTxtFile file;
    if (!file.Open(lpFile, LTxtFile::modeReadWrite))
        return 0;

    if (SLFILE_CLEAR & dwFlags)
        Clear();

    BOOL bIncludeNull = (0 != (SLFILE_INCLUDENULL & dwFlags));
    return LoadFromFile(&file, bIncludeNull);
}

DWORD LStrList::LoadFromFile(LTxtFile* tf, BOOL bIncludeNull)
{
    LAutoLock al(m_lock);

    // 如果列表为空，则按照来源文件编码
    if (LPtrList::IsEmpty())
    {
        m_dwFlags = 0;
        if (tf->IsUnicode())
        {
            m_dwFlags = SLFLAG_UNICODE;
            m_pfnCopy = LStrList_CopyW;
            m_pfnDestroy = LStrList_DestroyW;
        }
        else
        {
            m_pfnCopy = LStrList_CopyA;
            m_pfnDestroy = LStrList_DestroyA;
        }
    }

    DWORD ret = 0;
    if (IsUnicode())
    {
        LStringW str;
        while (!tf->Eof())
        {
            tf->ReadLn(&str);
            if ((L'\0' != str[0]) || bIncludeNull)
            {
                AddTail(str);
                ++ret;
            }
        }
    }
    else
    {
        LStringA str;
        while (!tf->Eof())
        {
            tf->ReadLn(&str);
            if (('\0' != str[0]) || bIncludeNull)
            {
                AddTail(str);
                ++ret;
            }
        }
    }
    return ret;
}

DWORD LStrList::SaveToFile(__in PCSTR lpFile, __in DWORD dwFlags)
{
    if (NULL == m_itHead)
        return 0;

    LTxtFile::MODE mode;
    BOOL bUnicode = IsUnicode();
    if (SLFILE_CLEAR & dwFlags)
        mode = LTxtFile::modeReset;
    else
        mode = LTxtFile::modeAppend;

    LTxtFile file(bUnicode);
    if (!file.Open(lpFile, mode))
        return 0;

    BOOL bIncludeNull = (0 != (SLFILE_INCLUDENULL & dwFlags));
    return SaveToFile(&file, bIncludeNull);
}

DWORD LStrList::SaveToFile(__in PCWSTR lpFile, __in DWORD dwFlags)
{
    if (NULL == m_itHead)
        return 0;

    LTxtFile::MODE mode;
    BOOL bUnicode = IsUnicode();
    if (SLFILE_CLEAR & dwFlags)
        mode = LTxtFile::modeReset;
    else
        mode = LTxtFile::modeAppend;

    LTxtFile file(bUnicode);
    if (!file.Open(lpFile, mode))
        return 0;

    BOOL bIncludeNull = (0 != (SLFILE_INCLUDENULL & dwFlags));
    return SaveToFile(&file, bIncludeNull);
}

DWORD LStrList::SaveToFile(LTxtFile* tf, BOOL bIncludeNull)
{
    DWORD ret = 0;
    LAutoLock al(m_lock);

    LIterator it = GetHeadIterator();
    if (IsUnicode())
    {
        while (NULL != it)
        {
            PCWSTR str = (PCWSTR)GetRawString(it);
            if (L'\0' != *str || bIncludeNull)
            {
                tf->WriteLn(str);
                ++ret;
            }
            it = GetNextIterator(it);
        }
    }
    else
    {
        while (NULL != it)
        {
            PCSTR str = (PCSTR)GetRawString(it);
            if ('\0' != *str || bIncludeNull)
            {
                tf->WriteLn(str);
                ++ret;
            }
            it = GetNextIterator(it);
        }
    }

    return ret;
}

void LStrList::SetAt(__in LIterator it, __in PCSTR lpString)
{
    if (IsUnicode())
    {
        LStringW str = lpString;
        SetAt(it, str);
    }
    else
    {
        LPtrList::SetAt(it, &lpString);
    }
}

void LStrList::SetAt(__in LIterator it, __in PCWSTR lpString)
{
    if (IsUnicode())
    {
        LPtrList::SetAt(it, &lpString);
    }
    else
    {
        LStringA str = lpString;
        SetAt(it, str);
    }
}
