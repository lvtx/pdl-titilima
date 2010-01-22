#include "..\..\include\pdl_string.h"
#include "stringt.h"
#ifdef _WIN32_WCE
#include "..\adaptor\wince_adaptor.h"
#endif // #ifdef _WIN32_WCE

///////////////////////////////////////////////////////////////////////////////
// LStringA

const char nullstrA[] = { '\0' };

LStringA::LStringA(void)
{
    m_dwMaxLen = 0;
    m_lpszData = AllocString(nullstrA);
}

LStringA::LStringA(__in PCSTR lpszString)
{
    m_dwMaxLen = 0;
    if (NULL == lpszString || '\0' == *lpszString)
    {
        m_lpszData = AllocString(nullstrA);
    }
    else
    {
        m_lpszData = NULL;
        Copy(lpszString);
    }
}

LStringA::LStringA(__in PCWSTR lpszString, __in UINT CodePage /* = CP_ACP */)
{
    m_dwMaxLen = 0;
    if (NULL == lpszString || L'\0' == *lpszString)
    {
        m_lpszData = AllocString(nullstrA);
    }
    else
    {
        m_lpszData = NULL;
        Copy(lpszString, CodePage);
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
        FreeString(m_lpszData);
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
    AllocBuffer(nLen + CharTraitsA::Len(lpszString), TRUE);
    CharTraitsA::Copy(&m_lpszData[nLen], lpszString);
    return *this;
}

const LStringA& LStringA::operator+=(__in char ch)
{
    Append(ch);
    return *this;
}

char LStringA::operator[](__in int idx)
{
    return GetAt(idx);
}

PSTR LStringA::AllocBuffer(__in DWORD nChars, __in BOOL bSaveData /* = TRUE */)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    return str.AllocBuffer(nChars, bSaveData);
}

PSTR LStringA::AllocString(__in PCSTR lpString)
{
    if (NULL == lpString)
        lpString = nullstrA;
    return CharTraitsA::Alloc(lpString);
}

void LStringA::Append(__in char ch)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    str.Append(ch);
}

void LStringA::Attach(__in PSTR lpszString)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    str.Attach(lpszString);
}

int LStringA::Compare(__in PCSTR lpszString, __in BOOL bCase /* = TRUE */)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    return str.Compare(lpszString, bCase);
}

BSTR LStringA::ConvertToBSTR(void)
{
    LStringW strW = m_lpszData;
    return strW.ConvertToBSTR();
}

void LStringA::Copy(__in PCSTR lpszString)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    str.Copy(lpszString);
}

void LStringA::Copy(__in PCWSTR lpszString, __in UINT CodePage /* = CP_ACP */)
{
    int len = WideCharToMultiByte(CodePage, 0, lpszString, -1, NULL, 0,
        NULL, NULL);
    if (len > (int)m_dwMaxLen)
    {
        m_dwMaxLen = len;
        FreeString(m_lpszData);
        m_lpszData = CharTraitsA::Alloc(len);
    }
    WideCharToMultiByte(CodePage, 0, lpszString, -1, m_lpszData, len, NULL,
        NULL);
    m_lpszData[len] = '\0';
}

PSTR LStringA::Detach(void)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    return str.Detach();
}

void LStringA::Empty(void)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    str.Empty();
}

BOOL LStringA::ExpandEnvironment(void)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    return str.ExpandEnvironment();
}

int LStringA::Find(__in char ch, __in int iStart /* = 0 */)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    return str.Find(ch, iStart);
}

int LStringA::Find(__in PCSTR pszSub, __in int iStart /* = 0 */)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    return str.Find(pszSub, iStart);
}

int LStringA::Format(__in PCSTR lpszFormat, ...)
{
    char tmp[1024];
    va_list argList;

    va_start(argList, lpszFormat);
    int ret = CharTraitsA::VSprintF(tmp, lpszFormat, argList);
    va_end(argList);

    Copy(tmp);
    return ret;
}

void LStringA::FreeString(__in PSTR lpString)
{
    CharTraitsA::Free(lpString);
}

char LStringA::GetAt(__in int idx)
{
    PDLASSERT(idx < (int)(m_dwMaxLen + 1));
    return m_lpszData[idx];
}

int LStringA::GetLength(void) const
{
    return CharTraitsA::Len(m_lpszData);
}

BOOL LStringA::IsEmpty(void) const
{
    return (NULL == m_lpszData || '\0' == *m_lpszData);
}

LStringA LStringA::Left(__in int nChars)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);

    LStringA ret;
    ret.Attach(str.Left(nChars));
    return ret;
}

LStringA LStringA::Mid(__in int iStart, __in int nChars /* = -1 */)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);

    LStringA ret;
    ret.Attach(str.Mid(iStart, nChars));
    return ret;
}

int LStringA::Replace(__in PCSTR pszOld, __in PCSTR pszNew)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    return str.Replace(pszOld, pszNew);
}

void LStringA::ReplaceBackslashChars(void)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    str.ReplaceBackslashChars();
}

int LStringA::ReverseFind(__in char ch)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    return str.ReverseFind(ch);
}

int LStringA::SetAt(__in int pos, __in char ch)
{
    LStringT<char, CharTraitsA> str(m_lpszData, m_dwMaxLen);
    return str.SetAt(pos, ch);
}

void LStringA::ToLower(void)
{
    if (NULL == m_lpszData || '\0' == m_lpszData[0])
        return;
    _strlwr(m_lpszData);
}

void LStringA::ToUpper(void)
{
    if (NULL == m_lpszData || '\0' == m_lpszData[0])
        return;
    _strupr(m_lpszData);
}

int LStringA::Trim(__in PSTR string, __in PCSTR trimchars)
{
    return LStringT<char, CharTraitsA>::Trim(string, trimchars);
}

int LStringA::Trim(__in PCSTR trimchars)
{
    return Trim(m_lpszData, trimchars);
}

///////////////////////////////////////////////////////////////////////////////
// LStringW

const WCHAR nullstrW[] = { L'\0' };

LStringW::LStringW(void)
{
    m_dwMaxLen = 0;
    m_lpszData = AllocString(nullstrW);
}

LStringW::LStringW(__in PCSTR lpszString, __in UINT CodePage /* = CP_ACP */)
{
    m_dwMaxLen = 0;
    if (NULL == lpszString || '\0' == *lpszString)
    {
        m_lpszData = AllocString(nullstrW);
    }
    else
    {
        m_lpszData = NULL;
        Copy(lpszString, CodePage);
    }
}

LStringW::LStringW(__in PCWSTR lpszString /* = NULL */)
{
    m_dwMaxLen = 0;
    if (NULL == lpszString || L'\0' == *lpszString)
    {
        m_lpszData = AllocString(nullstrW);
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
        FreeString(m_lpszData);
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
    AllocBuffer(nLen + CharTraitsW::Len(lpszString));
    CharTraitsW::Copy(&m_lpszData[nLen], lpszString);
    return *this;
}

const LStringW& LStringW::operator+=(__in WCHAR ch)
{
    Append(ch);
    return *this;
}

WCHAR LStringW::operator[](__in int idx)
{
    return GetAt(idx);
}

PWSTR LStringW::AllocBuffer(__in DWORD nChars, __in BOOL bSaveData /* = TRUE */)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    return str.AllocBuffer(nChars, bSaveData);
}

PWSTR LStringW::AllocString(__in PCWSTR lpString)
{
    if (NULL == lpString)
        lpString = nullstrW;
    return CharTraitsW::Alloc(lpString);
}

void LStringW::Append(__in WCHAR ch)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    str.Append(ch);
}

void LStringW::Attach(__in PWSTR lpszString)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    str.Attach(lpszString);
}

int LStringW::Compare(__in PCWSTR lpszString, __in BOOL bCase /* = TRUE */)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    return str.Compare(lpszString, bCase);
}

BSTR LStringW::ConvertToBSTR(void)
{
    if (NULL == m_lpszData)
        return NULL;
    return ::SysAllocString(m_lpszData);
}

void LStringW::Copy(__in PCSTR lpszString, __in UINT CodePage /* = CP_ACP */)
{
    int len = MultiByteToWideChar(CodePage, 0, lpszString, -1, NULL, 0);
    if (len > (int)m_dwMaxLen)
    {
        m_dwMaxLen = len;
        FreeString(m_lpszData);
        m_lpszData = CharTraitsW::Alloc(len);
    }
    MultiByteToWideChar(CodePage, 0, lpszString, -1, m_lpszData, len);
    m_lpszData[len] = L'\0';
}

void LStringW::Copy(__in PCWSTR lpszString)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    str.Copy(lpszString);
}

PWSTR LStringW::Detach(void)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    return str.Detach();
}

void LStringW::Empty(void)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    str.Empty();
}

BOOL LStringW::ExpandEnvironment(void)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    return str.ExpandEnvironment();
}

int LStringW::Find(__in WCHAR ch, int iStart /* = 0 */)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    return str.Find(ch, iStart);
}

int LStringW::Find(__in PCWSTR pszSub, int iStart /* = 0 */)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    return str.Find(pszSub, iStart);
}

BOOL LStringW::Format(__in PCWSTR lpszFormat, ...)
{
    WCHAR tmp[1024];
    va_list argList;

    va_start(argList, lpszFormat);
    int ret = CharTraitsW::VSprintF(tmp, lpszFormat, argList);
    va_end(argList);

    Copy(tmp);
    return ret;
}

void LStringW::FreeString(__in PWSTR lpString)
{
    CharTraitsW::Free(lpString);
}

WCHAR LStringW::GetAt(__in int idx)
{
    PDLASSERT(idx < (int)(m_dwMaxLen + 1));
    return m_lpszData[idx];
}

int LStringW::GetLength(void) const
{
    return CharTraitsW::Len(m_lpszData);
}

BOOL LStringW::IsEmpty(void) const
{
    return (NULL == m_lpszData || L'\0' == *m_lpszData);
}

LStringW LStringW::Left(__in int nChars)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);

    LStringW ret;
    ret.Attach(str.Left(nChars));
    return ret;
}

LStringW LStringW::Mid(__in int iStart, __in int nChars /* = -1 */)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);

    LStringW ret;
    ret.Attach(str.Mid(iStart, nChars));
    return ret;
}

int LStringW::Replace(__in PCWSTR pszOld, __in PCWSTR pszNew)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    return str.Replace(pszOld, pszNew);
}

void LStringW::ReplaceBackslashChars(void)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    str.ReplaceBackslashChars();
}

int LStringW::ReverseFind(__in WCHAR ch)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    return str.ReverseFind(ch);
}

int LStringW::SetAt(__in int pos, __in WCHAR ch)
{
    LStringT<WCHAR, CharTraitsW> str(m_lpszData, m_dwMaxLen);
    return str.SetAt(pos, ch);
}

void LStringW::ToLower(void)
{
    if (NULL == m_lpszData || L'\0' == m_lpszData[0])
        return;
    _wcslwr(m_lpszData);
}

void LStringW::ToUpper(void)
{
    if (NULL == m_lpszData || L'\0' == m_lpszData[0])
        return;
    _wcsupr(m_lpszData);
}

int LStringW::Trim(__in PWSTR string, __in PCWSTR trimchars)
{
    return LStringT<WCHAR, CharTraitsW>::Trim(string, trimchars);
}

int LStringW::Trim(__in PCWSTR trimchars)
{
    return Trim(m_lpszData, trimchars);
}
