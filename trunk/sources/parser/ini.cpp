///////////////////////////////////////////////////////////////////////////////
// FileName:    ini.cpp
// Created:     2009/04/11
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: ini parser
///////////////////////////////////////////////////////////////////////////////

#include "..\..\include\pdl_parser.h"
#include "..\..\include\pdl_file.h"
#include "..\..\include\pdl_module.h"
#ifdef _WIN32_WCE
#include "..\adaptor\wince_adaptor.h"
#endif // _WIN32_WCE

#define INISTATE_DIRTY      0x00000001

///////////////////////////////////////////////////////////////////////////////
// LIniParser

LIniParser::LIniParser(__in ILock* lock) : m_data(lock)
{
    m_dwState = 0;
    m_secList.Create(sizeof(LIterator));
}

LIniParser::~LIniParser(void)
{
    Close();
}

void LIniParser::Close(void)
{
    m_data.Clear();
    m_secList.Clear();
}

// LIniParser::FindKey
// 返回指定 Section、指定 Key 的行首位置。
// [in] lpszSection: 要查找的 Section 名称。
// [in] lpszKey: 要查找的 Key 名称。
// [ret] 如果找到，则返回该行的起始位置，否则返回 -1。

LIterator LIniParser::FindKey(__in PCSTR lpszSection, __in PCSTR lpszKey)
{
    LIterator itSec = FindSection(lpszSection);
    if (NULL == itSec)
        return NULL;

    LIterator itNext = FindNextSection(itSec);
    while (itSec != itNext)
    {
        LStringA str;
        m_data.GetAt(itSec, &str);
        int nEqual = str.Find('=');
        if (-1 != nEqual)
        {
            LStringA strKey = str.Left(nEqual);
            strKey.Trim(" \t");
            if (0 == lstrcmpiA(lpszKey, strKey))
                return itSec;
        }

        itSec = m_data.GetNextIterator(itSec);
    }
    return NULL;
}

LIterator LIniParser::FindNextSection(__in LIterator it)
{
    LIterator it2 = m_secList.GetHeadIterator();
    LIterator itSec = NULL;
    while (NULL != it2)
    {
        m_secList.GetAt(it2, &itSec);
        it2 = m_secList.GetNextIterator(it2);
        if (itSec == it && NULL != it2)
        {
            m_secList.GetAt(it2, &itSec);
            return itSec;
        }
    }
    return NULL;
}

// LIniParser::FindSection
// 查找指定的 Section，返回行首位置。
// [in] lpszSection: 要查找的 Section 名称。
// [ret] 如果成功则返回指定的 Section 行首位置，否则返回 -1。

LIterator LIniParser::FindSection(__in PCSTR lpszSection)
{
    LIterator it = m_secList.GetHeadIterator();
    LIterator itSec = NULL;
    while (NULL != it)
    {
        m_secList.GetAt(it, &itSec);

        LStringA str;
        m_data.GetAt(itSec, &str);
        str.Trim(" \t");
        str.Trim("[]");
        if (0 == lstrcmpiA(lpszSection, str))
            return itSec;

        it = m_secList.GetNextIterator(it);
    }
    return NULL;
}

int LIniParser::GetInt(
    __in PCSTR lpszSection,
    __in PCSTR lpszKey,
    __in int nDefault)
{
    PSTR pszRet = GetStringA(lpszSection, lpszKey);
    int nRet = nDefault;

    if (NULL != pszRet)
    {
        if (isdigit(pszRet[0]))
            nRet = atoi(pszRet);
        else
            nRet = nDefault;
        delete [] pszRet;
    }
    else
    {
        nRet = nDefault;
    }
    return nRet;
}

DWORD LIniParser::GetString(
    __in PCSTR lpszSection,
    __in PCSTR lpszKey,
    __in PCSTR lpszDefault,
    __out LStringA* strResult)
{
    PSTR pszRet = GetStringA(lpszSection, lpszKey);
    if (NULL != pszRet)
        strResult->Attach(pszRet);
    else
        *strResult = lpszDefault;
    return strResult->GetLength();
}

DWORD LIniParser::GetString(
    __in PCSTR lpszSection,
    __in PCSTR lpszKey,
    __in PCWSTR lpszDefault,
    __out LStringW* strResult)
{
    PSTR pszRet = GetStringA(lpszSection, lpszKey);
    if (NULL != pszRet)
    {
        strResult->Copy(pszRet);
        LStringA::FreeString(pszRet);
    }
    else
    {
        *strResult = lpszDefault;
    }
    return strResult->GetLength();
}

DWORD LIniParser::GetString(
    __in PCSTR lpszSection,
    __in PCSTR lpszKey,
    __in PCSTR lpszDefault,
    __out PSTR lpszBuffer,
    __in DWORD nSize)
{
    PSTR pszRet = GetStringA(lpszSection, lpszKey);
    if (NULL != pszRet)
    {
        lstrcpynA(lpszBuffer, pszRet, nSize);
        LStringA::FreeString(pszRet);
    }
    else
    {
        lstrcpynA(lpszBuffer, lpszDefault, nSize);
    }
    return lstrlenA(lpszBuffer);
}

DWORD LIniParser::GetString(
    __in PCSTR lpszSection,
    __in PCSTR lpszKey,
    __in PCWSTR lpszDefault,
    __out PWSTR lpszBuffer,
    __in DWORD nSize)
{
    PSTR pszRet = GetStringA(lpszSection, lpszKey);
    if (NULL != pszRet)
    {
        MultiByteToWideChar(CP_ACP, 0, pszRet, -1, lpszBuffer, nSize);
        LStringA::FreeString(pszRet);
    }
    else
    {
        lstrcpynW(lpszBuffer, lpszDefault, nSize);
    }
    return lstrlenW(lpszBuffer);
}

BOOL LIniParser::GetSection(
    __in PCSTR lpszSection,
    __out LIniSection* sec,
    __in BOOL bCreate)
{
    LIterator it = FindSection(lpszSection);
    if (NULL == it)
    {
        if (bCreate)
        {
            LStringA strSec;
            strSec.Format("[%s]", lpszSection);
            it = m_data.AddTail(strSec);
            m_secList.AddTail(&it);
            m_dwState |= INISTATE_DIRTY;
        }
        else
        {
            return FALSE;
        }
    }

    sec->m_pState = &m_dwState;
    sec->m_ini = &m_data;
    sec->m_head = it;
    sec->m_tail = FindNextSection(it);
    if (NULL == sec->m_tail)
        sec->m_tail = m_data.AddTail(""); // 插入一个空行作为分界线
    return TRUE;
}

DWORD LIniParser::GetSectionCount(void)
{
    return m_secList.GetCount();
}

// LIniParser::GetStringA
// 获取指定 Section、指定 Key 处的原始字符串。
// [in] lpszSection: 要查找的 Section 名称。
// [in] lpszKey: 要查找的 Key 名称。
// [ret] 如果获取成功则返回原始字符串，这个字符串需要调用 delete [] 来销毁。
//       如果获取失败，则返回 NULL。

PSTR LIniParser::GetStringA(__in PCSTR lpszSection, __in PCSTR lpszKey)
{
    LIterator it = FindKey(lpszSection, lpszKey);
    if (NULL == it)
        return NULL;

    LStringA strLine;
    m_data.GetAt(it, &strLine);
    LStringA str = strchr((PCSTR)strLine, '=') + 1;
    str.Trim(" \t");
    return str.Detach();
}

BOOL LIniParser::Open(__in PCSTR lpszFileName)
{
    m_data.Clear();
    m_secList.Clear();
    if (NULL == lpszFileName)
        return FALSE;

    LStringA path;
    if (LFile::IsFullPathName(lpszFileName))
    {
        path = lpszFileName;
    }
    else
    {
        LAppModule::GetModulePath(NULL, &path);
        path += lpszFileName;
    }
    if (!LFile::Exists(path))
        return FALSE;

    LTxtFile file;
    if (!file.Open(path, LTxtFile::modeReadWrite))
        return FALSE;

    Open(&file);
    return TRUE;
}

BOOL LIniParser::Open(__in PCWSTR lpszFileName)
{
    m_data.Clear();
    m_secList.Clear();
    if (NULL == lpszFileName)
        return FALSE;

    LStringW path;
    if (LFile::IsFullPathName(lpszFileName))
    {
        path = lpszFileName;
    }
    else
    {
        LAppModule::GetModulePath(NULL, &path);
        path += lpszFileName;
    }
    if (!LFile::Exists(path))
        return FALSE;

    LTxtFile file;
    if (!file.Open(path, LTxtFile::modeReadWrite))
        return FALSE;

    Open(&file);
    return TRUE;
}

void LIniParser::Open(__in LTxtFile* pFile)
{
    LStringA str;
    while (!pFile->Eof())
    {
        pFile->ReadLn(&str);
        int len = str.Trim(" \t");
        if (str.IsEmpty())
            continue;

        LIterator it = m_data.AddTail(str);
        if ('[' == str[0] && ']' == str[len - 1])
            m_secList.AddTail(&it);
    }
    m_dwState = 0;
}

void LIniParser::RemoveSection(__in PCSTR lpszSection)
{
    LIterator itSec = FindSection(lpszSection);
    LIterator itNext = FindNextSection(itSec);
    while (itNext != itSec)
    {
        LIterator itDel = itSec;
        itSec = m_data.GetNextIterator(itSec);
        m_data.Remove(itDel);
    }
}

BOOL LIniParser::Save(__in_opt PCSTR lpszFileName)
{
    if (NULL == lpszFileName || 0 == (INISTATE_DIRTY & m_dwState))
        return FALSE;

    LStringA path;
    if (LFile::IsFullPathName(lpszFileName))
    {
        path = lpszFileName;
    }
    else
    {
        LAppModule::GetModulePath(NULL, &path);
        path += lpszFileName;
    }
    return m_data.SaveToFile(path, SLFILE_CLEAR | SLFILE_INCLUDENULL);
}

BOOL LIniParser::Save(__in_opt PCWSTR lpszFileName)
{
    if (NULL == lpszFileName || 0 == (INISTATE_DIRTY & m_dwState))
        return FALSE;

    LStringW path;
    if (LFile::IsFullPathName(lpszFileName))
    {
        path = lpszFileName;
    }
    else
    {
        LAppModule::GetModulePath(NULL, &path);
        path += lpszFileName;
    }
    return m_data.SaveToFile(path, SLFILE_CLEAR | SLFILE_INCLUDENULL);
}

BOOL LIniParser::WriteInt(
    __in PCSTR lpszSection,
    __in PCSTR lpszKey,
    __in int nValue)
{
    CHAR str[20];
    wsprintfA(str, "%d", nValue);
    return WriteStringA(lpszSection, lpszKey, str);
}

BOOL LIniParser::WriteString(
    __in PCSTR lpszSection,
    __in PCSTR lpszKey,
    __in PCSTR lpszValue)
{
    return WriteStringA(lpszSection, lpszKey, lpszValue);
}

BOOL LIniParser::WriteString(
    __in PCSTR lpszSection,
    __in PCSTR lpszKey,
    __in PCWSTR lpszValue)
{
    LStringA str = lpszValue;
    return WriteStringA(lpszSection, lpszKey, str);
}

// LIniParser::WriteStringA
// 将指定的 Section 和 Key 处设置为指定的字符串。
// [in] lpszSection: 要写入的 Section 名称。
// [in] lpszKey: 要写入的 Key 名称。
// [in] lpszValue: 要写入的字符串。
// [ret] 如果成功则返回 TRUE，否则返回 FALSE。

BOOL LIniParser::WriteStringA(PCSTR lpszSection, PCSTR lpszKey, PCSTR lpszValue)
{
    m_dwState |= INISTATE_DIRTY;

    LStringA str;
    LIterator itSec = FindSection(lpszSection);
    if (NULL == itSec)
    {
        // 不存在指定的 section
        str.Format("[%s]", lpszSection);
        LIterator itTail = m_data.AddTail(str);
        m_secList.AddTail(&itTail);
        itSec = itTail;
    }

    str.Format("%s=%s", lpszKey, lpszValue);
    LIterator itKey = FindKey(lpszSection, lpszKey);
    if (NULL != itKey)
    {
        m_data.SetAt(itKey, str);
        return TRUE;
    }

    // 尾部插入新的值
    LIterator it = FindNextSection(itSec);
    if (NULL != it)
        m_data.InsertBefore(it, str);
    else
        m_data.AddTail(str);
    return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
// LIniSection

LIniSection::LIniSection(void)
{
    m_pState = NULL;
    m_ini = NULL;
    m_head = NULL;
    m_tail = NULL;
}

LIterator LIniSection::AddHead(__in PCSTR lpKeyName, __in PCSTR lpValue)
{
    LStringA str;
    str.Format("%s=%s", lpKeyName, lpValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertAfter(m_head, str);
}

LIterator LIniSection::AddHead(__in PCSTR lpKeyName, __in PCWSTR lpValue)
{
    LStringA str;
    str.Format("%s=%S", lpKeyName, lpValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertAfter(m_head, str);
}

LIterator LIniSection::AddHead(__in PCSTR lpKeyName, __in int nValue)
{
    LStringA str;
    str.Format("%s=%d", lpKeyName, nValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertAfter(m_head, str);
}

LIterator LIniSection::AddTail(__in PCSTR lpKeyName, __in PCSTR lpValue)
{
    LStringA str;
    str.Format("%s=%s", lpKeyName, lpValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertBefore(m_tail, str);
}

LIterator LIniSection::AddTail(__in PCSTR lpKeyName, __in PCWSTR lpValue)
{
    LStringA str;
    str.Format("%s=%S", lpKeyName, lpValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertBefore(m_tail, str);
}

LIterator LIniSection::AddTail(__in PCSTR lpKeyName, __in int nValue)
{
    LStringA str;
    str.Format("%s=%d", lpKeyName, nValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertBefore(m_tail, str);
}

void LIniSection::Clear(void)
{
    LIterator it = GetHead();
    while (NULL != it)
    {
        LIterator itDel = it;
        it = GetNext(it);
        m_ini->Remove(itDel);
    }
    *m_pState |= INISTATE_DIRTY;
}

LIterator LIniSection::GetHead(void)
{
    return GetNext(m_head);
}

BOOL LIniSection::GetKeyName(__in LIterator it, __out LStringA* str)
{
    LStringA tmp;
    if (!m_ini->GetAt(it, &tmp))
        return FALSE;

    int n = tmp.Find('=');
    if (-1 == n)
        return FALSE;

    tmp.SetAt(n, '\0');
    tmp.Trim(" \t");
    str->Attach(tmp.Detach());
    return TRUE;
}

BOOL LIniSection::GetKeyName(__in LIterator it, __out LStringW* str)
{
    LStringW tmp;
    if (!m_ini->GetAt(it, &tmp))
        return FALSE;

    int n = tmp.Find(L'=');
    if (-1 == n)
        return FALSE;

    tmp.SetAt(n, L'\0');
    tmp.Trim(L" \t");
    str->Attach(tmp.Detach());
    return TRUE;
}

LIterator LIniSection::GetNext(__in LIterator it)
{
    LIterator ret = m_ini->GetNextIterator(it);
    if (ret == m_tail)
        ret = NULL;
    return ret;
}

BOOL LIniSection::GetValue(__in LIterator it, __out LStringA* str)
{
    LStringA strLine;
    if (!m_ini->GetAt(it, &strLine))
        return FALSE;

    int n = strLine.Find('=');
    if (-1 == n)
        return FALSE;

    *str = strLine.Mid(n + 1);
    return TRUE;
}

BOOL LIniSection::GetValue(__in LIterator it, __out LStringW* str)
{
    LStringW strLine;
    if (!m_ini->GetAt(it, &strLine))
        return FALSE;

    int n = strLine.Find(L'=');
    if (-1 == n)
        return FALSE;

    *str = strLine.Mid(n + 1);
    return TRUE;
}

LIterator LIniSection::InsertBefore(
    __in LIterator it,
    __in PCSTR lpKeyName,
    __in PCSTR lpValue)
{
    LStringA str;
    str.Format("%s=%s", lpKeyName, lpValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertBefore(it, str);
}

LIterator LIniSection::InsertBefore(
    __in LIterator it,
    __in PCSTR lpKeyName,
    __in PCWSTR lpValue)
{
    LStringA str;
    str.Format("%s=%S", lpKeyName, lpValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertBefore(it, str);
}

LIterator LIniSection::InsertBefore(
    __in LIterator it,
    __in PCSTR lpKeyName,
    __in int nValue)
{
    LStringA str;
    str.Format("%s=%d", lpKeyName, nValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertBefore(it, str);
}

LIterator LIniSection::InsertAfter(
    __in LIterator it,
    __in PCSTR lpKeyName,
    __in PCSTR lpValue)
{
    LStringA str;
    str.Format("%s=%s", lpKeyName, lpValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertAfter(it, str);
}

LIterator LIniSection::InsertAfter(
    __in LIterator it,
    __in PCSTR lpKeyName,
    __in PCWSTR lpValue)
{
    LStringA str;
    str.Format("%s=%S", lpKeyName, lpValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertAfter(it, str);
}

LIterator LIniSection::InsertAfter(
    __in LIterator it,
    __in PCSTR lpKeyName,
    __in int nValue)
{
    LStringA str;
    str.Format("%s=%d", lpKeyName, nValue);
    *m_pState |= INISTATE_DIRTY;
    return m_ini->InsertAfter(it, str);
}

BOOL LIniSection::IsEmpty(void)
{
    return m_head == m_tail;
}

BOOL LIniSection::SetInt(__in LIterator it, __in int nValue)
{
    LStringA str;
    str.Format("%d", nValue);
    return SetValue(it, str);
}

BOOL LIniSection::SetValue(__in LIterator it, __in PCSTR lpValue)
{
    LStringA str;
    if (!m_ini->GetAt(it, &str))
        return FALSE;

    int n = str.Find('=');
    if (-1 == n)
        return FALSE;

    str.SetAt(n + 1, '\0');
    str += lpValue;
    m_ini->SetAt(it, str);
    *m_pState |= INISTATE_DIRTY;
    return TRUE;
}

BOOL LIniSection::SetValue(__in LIterator it, __in PCWSTR lpValue)
{
    LStringW str;
    if (!m_ini->GetAt(it, &str))
        return FALSE;

    int n = str.Find(L'=');
    if (-1 == n)
        return FALSE;

    str.SetAt(n + 1, L'\0');
    str += lpValue;
    m_ini->SetAt(it, str);
    *m_pState |= INISTATE_DIRTY;
    return TRUE;
}
