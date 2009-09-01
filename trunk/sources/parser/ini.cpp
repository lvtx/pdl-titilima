///////////////////////////////////////////////////////////////////////////////
// FileName:    ini.cpp
// Created:     2009/04/11
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: ini parser
///////////////////////////////////////////////////////////////////////////////

#include <pdl_parser.h>
#include <pdl_file.h>
#include <stdlib.h>
#ifdef _WIN32_WCE
#include "..\adaptor\wince_adaptor.h"
#endif // _WIN32_WCE

LIniParser::LIniParser(__in ILock* lock) : LStrList(lock)
{
    m_secList.Create(sizeof(LIterator));
}

LIniParser::~LIniParser(void)
{
    Close();
}

void LIniParser::Close(void)
{
    LStrList::Clear();
    m_secList.Clear();
}

// LIniParser::FindKey
// ����ָ�� Section��ָ�� Key ������λ�á�
// [in] lpszSection: Ҫ���ҵ� Section ���ơ�
// [in] lpszKey: Ҫ���ҵ� Key ���ơ�
// [ret] ����ҵ����򷵻ظ��е���ʼλ�ã����򷵻� -1��

LIterator LIniParser::FindKey(__in PCSTR lpszSection, __in PCSTR lpszKey)
{
    LIterator itSec = FindSection(lpszSection);
    if (NULL == itSec)
        return NULL;

    LIterator itNext = FindNextSection(itSec);
    while (itSec != itNext)
    {
        LStringA str;
        LStrList::GetAt(itSec, &str);
        int nEqual = str.Find('=');
        if (-1 != nEqual)
        {
            LStringA strKey = str.Left(nEqual);
            strKey.Trim(" \t");
            if (0 == lstrcmpiA(lpszKey, strKey))
                return itSec;
        }

        itSec = m_secList.GetNextIterator(itSec);
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
// ����ָ���� Section����������λ�á�
// [in] lpszSection: Ҫ���ҵ� Section ���ơ�
// [ret] ����ɹ��򷵻�ָ���� Section ����λ�ã����򷵻� -1��

LIterator LIniParser::FindSection(__in PCSTR lpszSection)
{
    LIterator it = m_secList.GetHeadIterator();
    LIterator itSec = NULL;
    while (NULL != it)
    {
        m_secList.GetAt(it, &itSec);

        LStringA str = LStrList::GetAt(itSec);
        str.Trim(" \t");
        str.Trim("[]");
        if (0 == lstrcmpiA(lpszSection, str))
            return itSec;

        it = m_secList.GetNextIterator(it);
    }
    return NULL;
}

// LIniParser::GetFilePath
// �Ӹ������ļ�����ȡ��ɵ��ļ�����
// [out] lpFilePath: �������ļ�����
// [in] dwSize: lpFilePath �Ļ�������С��
// [in] lpFileName: �������ļ���������ļ������ܲ�������·����

void LIniParser::GetFilePath(
    __out PSTR lpFilePath,
    __in DWORD dwSize,
    __in PCSTR lpFileName)
{
    char szPathName[MAX_PATH];
    if (NULL == lpFileName)
    {
        // Ĭ���ļ���Ϊ "<Ӧ�ó�����>.ini"
        ::GetModuleFileNameA(NULL, szPathName, MAX_PATH);
        lstrcpyA(strrchr(szPathName, '.') + 1, "ini");
    }
    else
    {
        if (isalpha(lpFileName[0])
            && (':' == lpFileName[1])
            && ('\\' == lpFileName[2]))
        {
            // ����·��
            lstrcpynA(szPathName, lpFileName, MAX_PATH);
        }
        else
        {
            ::GetModuleFileNameA(NULL, szPathName, MAX_PATH);
            lstrcpyA(strrchr(szPathName, '\\') + 1, lpFileName);
        }
    }
    lstrcpynA(lpFilePath, szPathName, MAX_PATH);
}

void LIniParser::GetFilePath(
    __out PWSTR lpFilePath,
    __in DWORD dwSize,
    __in PCWSTR lpFileName)
{
    WCHAR szPathName[MAX_PATH];
    if (NULL == lpFileName)
    {
        // Ĭ���ļ���Ϊ "<Ӧ�ó�����>.ini"
        ::GetModuleFileNameW(NULL, szPathName, MAX_PATH);
        lstrcpyW(wcsrchr(szPathName, L'.') + 1, L"ini");
    }
    else
    {
        if (iswalpha(lpFileName[0])
            && (L':' == lpFileName[1])
            && (L'\\' == lpFileName[2]))
        {
            // ����·��
            lstrcpynW(szPathName, lpFileName, MAX_PATH);
        }
        else
        {
            ::GetModuleFileNameW(NULL, szPathName, MAX_PATH);
            lstrcpyW(wcsrchr(szPathName, L'\\') + 1, lpFileName);
        }
    }
    lstrcpynW(lpFilePath, szPathName, MAX_PATH);
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
        delete [] pszRet;
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
        delete [] pszRet;
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
        delete [] pszRet;
    }
    else
    {
        lstrcpynW(lpszBuffer, lpszDefault, nSize);
    }
    return lstrlenW(lpszBuffer);
}

DWORD LIniParser::GetSectionCount(void)
{
    return m_secList.GetCount();
}

// LIniParser::GetStringA
// ��ȡָ�� Section��ָ�� Key ����ԭʼ�ַ�����
// [in] lpszSection: Ҫ���ҵ� Section ���ơ�
// [in] lpszKey: Ҫ���ҵ� Key ���ơ�
// [ret] �����ȡ�ɹ��򷵻�ԭʼ�ַ���������ַ�����Ҫ���� delete [] �����١�
//       �����ȡʧ�ܣ��򷵻� NULL��

PSTR LIniParser::GetStringA(__in PCSTR lpszSection, __in PCSTR lpszKey)
{
    LIterator it = FindKey(lpszSection, lpszKey);
    if (NULL == it)
        return NULL;

    PCSTR lpLine = LStrList::GetAt(it);
    LStringA str = strchr(lpLine, '=') + 1;
    str.Trim(" \t");
    return str.Detach();
}

BOOL LIniParser::Open(__in_opt PCSTR lpszFileName)
{
    LStrList::Clear();
    m_secList.Clear();

    CHAR szFileName[MAX_PATH];
    GetFilePath(szFileName, MAX_PATH, lpszFileName);
    if (!LFile::Exists(szFileName))
        return FALSE;

    LTxtFile file;
    if (!file.Open(szFileName, LTxtFile::modeReadWrite))
        return FALSE;

    Open(&file);
    return TRUE;
}

BOOL LIniParser::Open(__in_opt PCWSTR lpszFileName)
{
    LStrList::Clear();
    m_secList.Clear();

    WCHAR szFileName[MAX_PATH];
    GetFilePath(szFileName, MAX_PATH, lpszFileName);
    if (!LFile::Exists(szFileName))
        return FALSE;

    LTxtFile file;
    if (!file.Open(szFileName, LTxtFile::modeReadWrite))
        return FALSE;

    Open(&file);
    return TRUE;
}

// Open
// ��һ�� ini �ļ���
// [in] pFile: ini �ļ��Ķ���ָ�롣

void LIniParser::Open(__in LTxtFile* pFile)
{
    LStringA str;
    while (!pFile->Eof())
    {
        pFile->ReadLn(&str);
        if ('\0' != str[0])
            LStrList::AddTail(str);

        int len = str.Trim(" \t");
        if (len > 0 && '[' == str[0] && ']' == str[len - 1])
            m_secList.AddTail(&m_itTail);
    }
}

void LIniParser::RemoveSection(__in PCSTR lpszSection)
{
    LIterator itSec = FindSection(lpszSection);
    LIterator itNext = FindNextSection(itSec);
    while (itNext != itSec)
    {
        LIterator itDel = itSec;
        itSec = GetNextIterator(itSec);
        Remove(itDel);
    }
}

BOOL LIniParser::Save(__in_opt PCSTR lpszFileName)
{
    CHAR szFileName[MAX_PATH];
    GetFilePath(szFileName, MAX_PATH, lpszFileName);
    return LStrList::SaveToFile(szFileName,
        SLFILE_CLEAR | SLFILE_INCLUDENULL);
}

BOOL LIniParser::Save(__in_opt PCWSTR lpszFileName)
{
    WCHAR szFileName[MAX_PATH];
    GetFilePath(szFileName, MAX_PATH, lpszFileName);
    return LStrList::SaveToFile(szFileName,
        SLFILE_CLEAR | SLFILE_INCLUDENULL);
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
// ��ָ���� Section �� Key ������Ϊָ�����ַ�����
// [in] lpszSection: Ҫд��� Section ���ơ�
// [in] lpszKey: Ҫд��� Key ���ơ�
// [in] lpszValue: Ҫд����ַ�����
// [ret] ����ɹ��򷵻� TRUE�����򷵻� FALSE��

BOOL LIniParser::WriteStringA(PCSTR lpszSection, PCSTR lpszKey, PCSTR lpszValue)
{
    LStringA str;
    LIterator itSec = FindSection(lpszSection);
    if (NULL == itSec)
    {
        // ������ָ���� section
        str.Format("[%s]", lpszSection);
        LStrList::AddTail(str);
        m_secList.AddTail(&m_itTail);
        itSec = m_itTail;
    }

    str.Format("%s=%s", lpszKey, lpszValue);
    LIterator itKey = FindKey(lpszSection, lpszKey);
    if (NULL != itKey)
    {
        LStrList::SetAt(itKey, str);
        return TRUE;
    }

    // β�������µ�ֵ
    LIterator it = FindNextSection(itSec);
    if (NULL != it)
        LStrList::InsertBefore(it, str);
    else
        LStrList::AddTail(str);
    return TRUE;
}
