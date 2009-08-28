#include <pdl_string.h>
#ifdef _WIN32_WCE
#include "adaptor\wince_adaptor.h"
#endif // #ifdef _WIN32_WCE

///////////////////////////////////////////////////////////////////////////////
// LStringA

LStringA::LStringA(void)
{
    m_dwMaxLen = 0;
    m_lpszData = new char[1];
    *m_lpszData = '\0';
}

LStringA::LStringA(__in PCSTR lpszString)
{
    m_dwMaxLen = 0;
    m_lpszData = NULL;
    Copy(lpszString);
}

LStringA::LStringA(__in PCWSTR lpszString)
{
    m_dwMaxLen = 0;
    if (NULL == lpszString)
    {
        m_lpszData = new char[m_dwMaxLen + 1];
        *m_lpszData = '\0';
    }
    else
    {
        m_lpszData = NULL;
        Copy(lpszString);
    }
}

LStringA::LStringA(__in const LStringA& obj)
{
    m_lpszData = NULL;
    m_dwMaxLen = 0;
    Copy(obj.m_lpszData);
}

LStringA::~LStringA(void)
{
    if (NULL != m_lpszData)
        delete [] m_lpszData;
}

LStringA::operator PCSTR(void) const
{
    return m_lpszData;
}

LStringA::operator PSTR(void) const
{
    return m_lpszData;
}

LStringA& LStringA::operator=(__in PCSTR lpszString)
{
    if (m_lpszData != lpszString)
        Copy(lpszString);

    return *this;
}

LStringA& LStringA::operator=(__in PCWSTR lpszString)
{
    Copy(lpszString);
    return *this;
}

LStringA& LStringA::operator=(__in const LStringA& str)
{
    if (this != &str)
        Copy(str.m_lpszData);
    return *this;
}

const LStringA& LStringA::operator+=(__in PCSTR lpszString)
{
    int nLen = GetLength();
    AllocBuffer(nLen + lstrlenA(lpszString));
    strcpy(&m_lpszData[nLen], lpszString);
    return *this;
}

char LStringA::operator[](__in int idx)
{
    PDLASSERT(idx < (int)m_dwMaxLen);
    return m_lpszData[idx];
}

PSTR LStringA::AllocBuffer(__in DWORD nChars, __in BOOL bSaveData /* = TRUE */)
{
    if (m_dwMaxLen < nChars)
    {
        m_dwMaxLen = nChars;
        PSTR pszNewData = new char[nChars + 1];
        if (bSaveData)
        {
            strncpy(pszNewData, m_lpszData, nChars);
            pszNewData[nChars] = '\0';
        }
        if (NULL != m_lpszData)
            delete [] m_lpszData;
        m_lpszData = pszNewData;
    }

    if (!bSaveData)
        ZeroMemory(m_lpszData, m_dwMaxLen + 1);

    return m_lpszData;
}

void LStringA::Attach(__in PSTR lpszString)
{
    if (NULL != m_lpszData)
        delete [] m_lpszData;
    m_lpszData = lpszString;
    m_dwMaxLen = strlen(lpszString);
}

BSTR LStringA::ConvertToBSTR(void)
{
    LStringW strW = m_lpszData;
    return strW.ConvertToBSTR();
}

void LStringA::Copy(__in PCSTR lpszString)
{
    size_t len = strlen(lpszString);
    if (len > m_dwMaxLen)
    {
        m_dwMaxLen = len;
        if (NULL != m_lpszData)
            delete [] m_lpszData;
        m_lpszData = new char[len + 1];
    }
    strcpy(m_lpszData, lpszString);
}

void LStringA::Copy(__in PCWSTR lpszString)
{
    size_t len = WideCharToMultiByte(CP_ACP, 0, lpszString, -1, NULL, 0,
        NULL, NULL);
    if (len > m_dwMaxLen)
    {
        m_dwMaxLen = len;
        delete [] m_lpszData;
        m_lpszData = new char[len + 1];
    }
    WideCharToMultiByte(CP_ACP, 0, lpszString, -1, m_lpszData, len, NULL,
        NULL);
    m_lpszData[len] = '\0';
}

PSTR LStringA::Detach(void)
{
    PSTR pszRet = m_lpszData;
    m_lpszData = NULL;
    m_dwMaxLen = 0;
    return pszRet;
}

int LStringA::Find(__in char ch, int iStart /* = 0 */)
{
    if (NULL == m_lpszData || GetLength() < iStart)
        return -1;

    PCSTR p = strchr(m_lpszData + iStart, ch);
    if (NULL == p)
        return -1;
    return p - m_lpszData;
}

int LStringA::Find(__in PCSTR pszSub, int iStart /* = 0 */)
{
    if (NULL == m_lpszData || GetLength() < iStart)
        return -1;

    PCSTR p = strstr(m_lpszData + iStart, pszSub);
    if (NULL == p)
        return -1;
    return p - m_lpszData;
}

BOOL LStringA::Format(__in PCSTR lpszFormat, ...)
{
    va_list argList;
    va_start(argList, lpszFormat);
    BOOL bRet = FormatV(lpszFormat, argList);
    va_end(argList);
    return bRet;
}

BOOL LStringA::FormatV(__in PCSTR lpszFormat, __in va_list argList)
{
    CHAR szTemp[1024];
    wvsprintfA(szTemp, lpszFormat, argList);
    Copy(szTemp);
    return TRUE;
}

int LStringA::GetLength(void) const
{
    return lstrlenA(m_lpszData);
}

LStringA LStringA::Left(__in int nChars)
{
    LStringA ret;
    PSTR buf = ret.AllocBuffer(nChars);
    lstrcpynA(buf, m_lpszData, nChars + 1);
    return ret;
}

LStringA LStringA::Mid(__in int iStart, int nChars /* = -1 */)
{
    LStringA ret;
    if (nChars <= 0)
    {
        ret = m_lpszData + iStart;
    }
    else
    {
        PSTR buf = ret.AllocBuffer(nChars);
        lstrcpynA(buf, m_lpszData + iStart, nChars + 1);
    }
    return ret;
}

int LStringA::Replace(__in PCSTR pszOld, __in PCSTR pszNew)
{
    int nStrLen = GetLength();
    if (0 == nStrLen)
        return 0;

    int nOldLen = strlen(pszOld);
    if (0 == nOldLen)
        return 0;

    // 计算要替换的子串个数
    int nNewLen = strlen(pszNew);
    int cnt = 0;
    PCSTR lpStart = m_lpszData;
    PCSTR lpEnd = m_lpszData + nOldLen;
    while (lpStart < lpEnd)
    {
        PCSTR lpTarget;
        while ((lpTarget = strstr(lpStart, pszOld)) != NULL)
        {
            ++cnt;
            lpStart = lpTarget + nOldLen;
        }
    }

    if (0 == cnt)
        return 0;

    nStrLen += (nNewLen - nOldLen) * cnt;
    PSTR pNewBuf = new char[nStrLen + 1];
    PSTR dst = pNewBuf;
    PSTR src = m_lpszData;
    PSTR p = NULL;
    int cbCopy = 0;
    for (int i = 0; i < cnt; ++i)
    {
        // 查找替换串
        p = strstr(src, pszOld);

        // 复制非替换串
        cbCopy = p - src;
        strncpy(dst, src, cbCopy);
        dst += cbCopy;
        src = p;

        // 复制替换串
        strncpy(dst, pszNew, nNewLen);
        dst += nNewLen;
        src += nOldLen;
    }
    // 复制剩余的字符串
    strcpy(dst, src);

    delete [] m_lpszData;
    m_lpszData = pNewBuf;
    return cnt;
}

int LStringA::Trim(__in PSTR string, __in PCSTR trimchars)
{
    if (NULL == string)
        return 0;

    char* p;
    char* start;
    char* mark = NULL;

    // 消除起始的字符
    p = string;
    while ('\0' != *p && strchr(trimchars, *p))
        ++p;
    start = p;

    // 消除尾部字符
    while ('\0' != *p)
    {
        if (strchr(trimchars, *p))
        {
            if (NULL == mark)
                mark = p;
        }
        else
        {
            mark = NULL;
        }
        ++p;
    }
    if (NULL != mark)
        *mark = '\0';

    // 重新处理字符串
    int cnt = lstrlenA(start);
    if (start > string)
        memmove(string, start, cnt + 1);
    return cnt;
}

int LStringA::Trim(__in PCSTR trimchars)
{
    return Trim(m_lpszData, trimchars);
}

///////////////////////////////////////////////////////////////////////////////
// LStringW

LStringW::LStringW(void)
{
    m_dwMaxLen = 0;
    m_lpszData = new WCHAR[1];
    *m_lpszData = L'\0';
}

LStringW::LStringW(__in PCSTR lpszString)
{
    m_dwMaxLen = 0;
    m_lpszData = NULL;
    Copy(lpszString);
}

LStringW::LStringW(__in PCWSTR lpszString /* = NULL */)
{
    m_dwMaxLen = 0;
    if (NULL == lpszString)
    {
        m_lpszData = new WCHAR[1];
        *m_lpszData = L'\0';
    }
    else
    {
        m_lpszData = NULL;
        Copy(lpszString);
    }
}

LStringW::LStringW(__in const LStringW& obj)
{
    m_lpszData = NULL;
    m_dwMaxLen = 0;
    Copy(obj.m_lpszData);
}

LStringW::~LStringW(void)
{
    if (NULL != m_lpszData)
        delete [] m_lpszData;
}

LStringW::operator PCWSTR(void) const
{
    return m_lpszData;
}

LStringW::operator PWSTR(void) const
{
    return m_lpszData;
}

LStringW& LStringW::operator=(__in PCSTR lpszString)
{
    Copy(lpszString);
    return *this;
}

LStringW& LStringW::operator=(__in PCWSTR lpszString)
{
    if (m_lpszData != lpszString)
        Copy(lpszString);
    return *this;
}

LStringW& LStringW::operator=(__in const LStringW& str)
{
    if (this != &str)
        Copy(str.m_lpszData);
    return *this;
}

const LStringW& LStringW::operator+=(__in PCWSTR lpszString)
{
    int nLen = GetLength();
    AllocBuffer(nLen + wcslen(lpszString));
    lstrcpyW(&m_lpszData[nLen], lpszString);
    return *this;
}

WCHAR LStringW::operator[](__in int idx)
{
    PDLASSERT(idx < (int)m_dwMaxLen);
    return m_lpszData[idx];
}

PWSTR LStringW::AllocBuffer(__in DWORD nChars, __in BOOL bSaveData /* = TRUE */)
{
    if (m_dwMaxLen < nChars)
    {
        m_dwMaxLen = nChars;
        PWSTR pszNewData = new WCHAR[nChars + 1];
        if (bSaveData)
        {
            wcsncpy(pszNewData, m_lpszData, nChars);
            pszNewData[nChars] = L'\0';
        }
        if (NULL != m_lpszData)
            delete [] m_lpszData;
        m_lpszData = pszNewData;
    }

    if (!bSaveData)
        ZeroMemory(m_lpszData, (m_dwMaxLen + 1) * sizeof(WCHAR));

    return m_lpszData;
}

void LStringW::Attach(__in PWSTR lpszString)
{
    if (NULL != m_lpszData)
        delete [] m_lpszData;
    m_lpszData = lpszString;
    m_dwMaxLen = wcslen(lpszString);
}

BSTR LStringW::ConvertToBSTR(void)
{
    if (NULL == m_lpszData)
        return NULL;
    return ::SysAllocString(m_lpszData);
}

void LStringW::Copy(__in PCSTR lpszString)
{
    size_t len = MultiByteToWideChar(CP_ACP, 0, lpszString, -1, NULL, 0);
    if (len > m_dwMaxLen)
    {
        m_dwMaxLen = len;
        delete [] m_lpszData;
        m_lpszData = new WCHAR[len + 1];
    }
    MultiByteToWideChar(CP_ACP, 0, lpszString, -1, m_lpszData, len);
    m_lpszData[len] = L'\0';
}

void LStringW::Copy(__in PCWSTR lpszString)
{
    size_t len = wcslen(lpszString);
    if (len > m_dwMaxLen)
    {
        m_dwMaxLen = len;
        if (NULL != m_lpszData)
            delete [] m_lpszData;
        m_lpszData = new WCHAR[len + 1];
    }
    wcscpy(m_lpszData, lpszString);
}

PWSTR LStringW::Detach(void)
{
    PWSTR pszRet = m_lpszData;
    m_lpszData = NULL;
    m_dwMaxLen = 0;
    return pszRet;
}

int LStringW::Find(__in WCHAR ch, int iStart /* = 0 */)
{
    if (NULL == m_lpszData || GetLength() < iStart)
        return -1;

    PCWSTR p = wcschr(m_lpszData + iStart, ch);
    if (NULL == p)
        return -1;
    return p - m_lpszData;
}

int LStringW::Find(__in PCWSTR pszSub, int iStart /* = 0 */)
{
    if (NULL == m_lpszData || GetLength() < iStart)
        return -1;

    PCWSTR p = wcsstr(m_lpszData + iStart, pszSub);
    if (NULL == p)
        return -1;
    return p - m_lpszData;
}

BOOL LStringW::Format(__in PCWSTR lpszFormat, ...)
{
    va_list argList;
    va_start(argList, lpszFormat);
    BOOL bRet = FormatV(lpszFormat, argList);
    va_end(argList);
    return bRet;
}

BOOL LStringW::FormatV(__in PCWSTR lpszFormat, __in va_list argList)
{
    WCHAR szTemp[1024];
    wvsprintfW(szTemp, lpszFormat, argList);
    Copy(szTemp);
    return TRUE;
}

int LStringW::GetLength(void) const
{
    return lstrlenW(m_lpszData);
}

LStringW LStringW::Left(__in int nChars)
{
    LStringW ret;
    PWSTR buf = ret.AllocBuffer(nChars);
    lstrcpynW(buf, m_lpszData, nChars + 1);
    return ret;
}

LStringW LStringW::Mid(__in int iStart, int nChars /* = -1 */)
{
    LStringW ret;
    if (nChars <= 0)
    {
        ret = m_lpszData + iStart;
    }
    else
    {
        PWSTR buf = ret.AllocBuffer(nChars);
        lstrcpynW(buf, m_lpszData + iStart, nChars + 1);
    }
    return ret;
}

int LStringW::Replace(__in PCWSTR pszOld, __in PCWSTR pszNew)
{
    int nStrLen = GetLength();
    if (0 == nStrLen)
        return 0;

    int nOldLen = wcslen(pszOld);
    if (0 == nOldLen)
        return 0;

    // 计算要替换的子串个数
    int nNewLen = wcslen(pszNew);
    int cnt = 0;
    PCWSTR lpStart = m_lpszData;
    PCWSTR lpEnd = m_lpszData + nOldLen;
    while (lpStart < lpEnd)
    {
        PCWSTR lpTarget;
        while ((lpTarget = wcsstr(lpStart, pszOld)) != NULL)
        {
            ++cnt;
            lpStart = lpTarget + nOldLen;
        }
    }

    if (0 == cnt)
        return 0;

    nStrLen += (nNewLen - nOldLen) * cnt;
    PWSTR pNewBuf = new WCHAR[nStrLen + 1];
    PWSTR dst = pNewBuf;
    PWSTR src = m_lpszData;
    PWSTR p = NULL;
    int cbCopy = 0;
    for (int i = 0; i < cnt; ++i)
    {
        // 查找替换串
        p = wcsstr(src, pszOld);

        // 复制非替换串
        cbCopy = p - src;
        wcsncpy(dst, src, cbCopy);
        dst += cbCopy;
        src = p;

        // 复制替换串
        wcsncpy(dst, pszNew, nNewLen);
        dst += nNewLen;
        src += nOldLen;
    }
    // 复制剩余的字符串
    wcscpy(dst, src);

    delete [] m_lpszData;
    m_lpszData = pNewBuf;
    return cnt;
}

int LStringW::Trim(__in PWSTR string, __in PCWSTR trimchars)
{
    if (NULL == string)
        return 0;

    wchar_t* p;
    wchar_t* start;
    wchar_t* mark = NULL;

    // 消除起始的字符
    p = string;
    while (L'\0' != *p && wcschr(trimchars, *p))
        ++p;
    start = p;

    // 消除尾部字符
    while (L'\0' != *p)
    {
        if (wcschr(trimchars, *p))
        {
            if (NULL == mark)
                mark = p;
        }
        else
        {
            mark = NULL;
        }
        ++p;
    }
    if (NULL != mark)
        *mark = L'\0';

    // 重新处理字符串
    int cnt = lstrlenW(start);
    if (start > string)
        memmove(string, start, (cnt + 1) * sizeof(wchar_t));
    return cnt;
}

int LStringW::Trim(__in PCWSTR trimchars)
{
    return Trim(m_lpszData, trimchars);
}
