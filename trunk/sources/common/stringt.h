///////////////////////////////////////////////////////////////////////////////
// FileName:    stringt.h
// Created:     2009/10/14
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 字符串模板类
///////////////////////////////////////////////////////////////////////////////

#pragma once

class CharTraitsA
{
public:
    static PSTR Alloc(PCSTR lpString)
    {
        int len = Len(lpString);
        PSTR ret = Alloc(len);
        return Copy(ret, lpString);
    }
    static PSTR Alloc(DWORD nChars)
    {
        return new char[nChars + 1];
    }
    static PSTR Chr(PSTR str, char ch)
    {
        return ::strchr(str, ch);
    }
    static int Compare(PCSTR str1, PCSTR str2, BOOL bCase)
    {
        if (bCase)
            return ::lstrcmpA(str1, str2);
        else
            return ::lstrcmpiA(str1, str2);
    }
    static PSTR Copy(PSTR dst, PCSTR src)
    {
        return ::lstrcpyA(dst, src);
    }
    static PSTR CopyN(PSTR dst, PCSTR src, int n)
    {
        return ::lstrcpynA(dst, src, n);
    }
    static void Free(PSTR str)
    {
        delete [] str;
    }
    static int Len(PCSTR lpString)
    {
        return ::lstrlenA(lpString);
    }
    static PSTR RChr(PSTR str, char ch)
    {
        return ::strrchr(str, ch);
    }
    static PSTR Str(PSTR lpString, PCSTR lpSubStr)
    {
        return ::strstr(lpString, lpSubStr);
    }
    static int VSprintF(PSTR buf, PCSTR lpszFormat, va_list argList)
    {
        return ::wvsprintfA(buf, lpszFormat, argList);
    }
};

class CharTraitsW
{
public:
    static PWSTR Alloc(PCWSTR lpString)
    {
        int len = Len(lpString);
        PWSTR ret = Alloc(len);
        return Copy(ret, lpString);
    }
    static PWSTR Alloc(DWORD nChars)
    {
        return new WCHAR[nChars + 1];
    }
    static PWSTR Chr(PWSTR str, WCHAR ch)
    {
        return wcschr(str, ch);
    }
    static PWSTR Copy(PWSTR dst, PCWSTR src)
    {
        return ::lstrcpyW(dst, src);
    }
    static PWSTR CopyN(PWSTR dst, PCWSTR src, int n)
    {
        return ::lstrcpynW(dst, src, n);
    }
    static int Compare(PCWSTR str1, PCWSTR str2, BOOL bCase)
    {
        if (bCase)
            return ::lstrcmpW(str1, str2);
        else
            return ::lstrcmpiW(str1, str2);
    }
    static void Free(PWSTR str)
    {
        delete [] str;
    }
    static int Len(PCWSTR lpString)
    {
        return ::lstrlenW(lpString);
    }
    static PWSTR RChr(PWSTR str, WCHAR ch)
    {
        return ::wcsrchr(str, ch);
    }
    static PWSTR Str(PWSTR lpString, PCWSTR lpSubStr)
    {
        return ::wcsstr(lpString, lpSubStr);
    }
    static int VSprintF(PWSTR buf, PCWSTR lpszFormat, va_list argList)
    {
        return ::wvsprintfW(buf, lpszFormat, argList);
    }
};

template <typename CharT, class CharTraits>
class LStringT
{
    typedef CharT *PSTRT;
    typedef const CharT *PCSTRT;
public:
    LStringT(PSTRT& str, DWORD& len) : m_str(str), m_len(len) { /* Nothing */ }
public:
    PSTRT AllocBuffer(DWORD nChars, BOOL bSaveData);
    void Append(CharT ch);
    void Attach(PSTRT lpszString);
    int Compare(PCSTRT lpszString, BOOL bCase);
    void Copy(PCSTRT lpszString);
    PSTRT Detach(void);
    void Empty(void);
    int Find(CharT ch, int iStart);
    int Find(PCSTRT pszSub, int iStart);
    PSTRT Left(int nChars);
    PSTRT Mid(int iStart, int nChars);
    int Replace(PCSTRT pszOld, PCSTRT pszNew);
    int ReverseFind(CharT ch);
    int SetAt(int pos, CharT ch);
    static int Trim(PSTRT string, PCSTRT trimchars);
protected:
    PSTRT& m_str;
    DWORD& m_len;
};

template <typename CharT, class CharTraits>
CharT* LStringT<CharT, CharTraits>::AllocBuffer(DWORD nChars, BOOL bSaveData)
{
    if (0 == nChars)
        nChars = 1;
    if (m_len < nChars)
    {
        m_len = nChars;
        PSTRT buf = CharTraits::Alloc(nChars);
        if (bSaveData)
            CharTraits::CopyN(buf, m_str, nChars + 1);
        if (NULL != m_str)
            CharTraits::Free(m_str);
        m_str = buf;
    }

    if (!bSaveData)
        ZeroMemory(m_str, (m_len + 1) * sizeof(CharT));
    return m_str;
}

template <typename CharT, class CharTraits>
void LStringT<CharT, CharTraits>::Append(CharT ch)
{
    int len = CharTraits::Len(m_str);
    if (len == m_len)
    {
        if (0 == m_len)
            m_len = 1;
        else
            m_len *= 2;
        CharT* buf = CharTraits::Alloc(m_len);
        CharTraits::Copy(buf, m_str);
        CharTraits::Free(m_str);
        m_str = buf;
    }

    m_str[len] = ch;
    m_str[len + 1] = CharT('\0');
}

template <typename CharT, class CharTraits>
void LStringT<CharT, CharTraits>::Attach(PSTRT lpszString)
{
    if (NULL != m_str)
        CharTraits::Free(m_str);
    m_str = lpszString;
    m_len = CharTraits::Len(lpszString);
}

template <typename CharT, class CharTraits>
int LStringT<CharT, CharTraits>::Compare(PCSTRT lpszString, BOOL bCase)
{
    return CharTraits::Compare(m_str, lpszString, bCase);
}

template <typename CharT, class CharTraits>
void LStringT<CharT, CharTraits>::Copy(PCSTRT lpszString)
{
    int len = CharTraits::Len(lpszString);
    if (len > (int)m_len)
    {
        m_len = len;
        if (NULL != m_str)
            CharTraits::Free(m_str);
        m_str = CharTraits::Alloc(len);
    }
    CharTraits::Copy(m_str, lpszString);
}

template <typename CharT, class CharTraits>
CharT* LStringT<CharT, CharTraits>::Detach(void)
{
    PSTRT ret = m_str;
    m_str = NULL;
    m_len = 0;
    return ret;
}

template <typename CharT, class CharTraits>
void LStringT<CharT, CharTraits>::Empty(void)
{
    if (NULL == m_str)
        m_str = CharTraits::Alloc(1);
    m_str[0] = CharT('\0');
}

template <typename CharT, class CharTraits>
int LStringT<CharT, CharTraits>::Find(CharT ch, int iStart)
{
    if (NULL == m_str || CharTraits::Len(m_str) < iStart)
        return -1;

    PCSTRT p = CharTraits::Chr(m_str + iStart, ch);
    if (NULL == p)
        return -1;
    return p - m_str;
}

template <typename CharT, class CharTraits>
int LStringT<CharT, CharTraits>::Find(PCSTRT pszSub, int iStart)
{
    if (NULL == m_str || CharTraits::Len(m_str) < iStart)
        return -1;

    PCSTRT p = CharTraits::Str(m_str + iStart, pszSub);
    if (NULL == p)
        return -1;
    return p - m_str;
}

template <typename CharT, class CharTraits>
CharT* LStringT<CharT, CharTraits>::Left(int nChars)
{
    PSTRT ret = CharTraits::Alloc(nChars);
    return CharTraits::CopyN(ret, m_str, nChars + 1);
}

template <typename CharT, class CharTraits>
CharT* LStringT<CharT, CharTraits>::Mid(int iStart, int nChars)
{
    PSTRT ret;
    if (nChars <= 0)
    {
        ret = CharTraits::Alloc(m_str + iStart);
    }
    else
    {
        ret = CharTraits::Alloc(nChars);
        CharTraits::CopyN(ret, m_str + iStart, nChars + 1);
    }
    return ret;
}

template <typename CharT, class CharTraits>
int LStringT<CharT, CharTraits>::Replace(PCSTRT pszOld, PCSTRT pszNew)
{
    int nStrLen = CharTraits::Len(m_str);
    if (0 == nStrLen)
        return 0;

    int nOldLen = CharTraits::Len(pszOld);
    if (0 == nOldLen)
        return 0;

    // 计算要替换的子串个数
    int nNewLen = CharTraits::Len(pszNew);
    int cnt = 0;
    PSTRT lpStart = m_str;
    PCSTRT lpEnd = m_str + nOldLen;
    while (lpStart < lpEnd)
    {
        PSTRT lpTarget;
        while ((lpTarget = CharTraits::Str(lpStart, pszOld)) != NULL)
        {
            ++cnt;
            lpStart = lpTarget + nOldLen;
        }
    }

    if (0 == cnt)
        return 0;

    nStrLen += (nNewLen - nOldLen) * cnt;
    PSTRT pNewBuf = CharTraits::Alloc(nStrLen);
    PSTRT dst = pNewBuf;
    PSTRT src = m_str;
    PSTRT p = NULL;
    int cbCopy = 0;
    for (int i = 0; i < cnt; ++i)
    {
        // 查找替换串
        p = CharTraits::Str(src, pszOld);

        // 复制非替换串
        cbCopy = p - src;
        CharTraits::CopyN(dst, src, cbCopy + 1);
        dst += cbCopy;
        src = p;

        // 复制替换串
        CharTraits::CopyN(dst, pszNew, nNewLen + 1);
        dst += nNewLen;
        src += nOldLen;
    }
    // 复制剩余的字符串
    CharTraits::Copy(dst, src);

    CharTraits::Free(m_str);
    m_str = pNewBuf;
    m_len = nStrLen;
    return cnt;
}

template <typename CharT, class CharTraits>
int LStringT<CharT, CharTraits>::ReverseFind(CharT ch)
{
    PCSTRT p = CharTraits::RChr(m_str, ch);
    if (NULL == p)
        return -1;
    return p - m_str;
}

template <typename CharT, class CharTraits>
int LStringT<CharT, CharTraits>::SetAt(int pos, CharT ch)
{
    if (NULL == m_str || CharT('\0') == *m_str)
        return -1;

    int len = CharTraits::Len(m_str);
    if (pos < 0 || pos >= len)
        pos = len - 1;
    int ret = m_str[pos];
    m_str[pos] = ch;
    return ret;
}

template <typename CharT, class CharTraits>
int LStringT<CharT, CharTraits>::Trim(PSTRT string, PCSTRT trimchars)
{
    if (NULL == string)
        return 0;

    CharT* p;
    CharT* start;
    CharT* mark = NULL;

    // 消除起始的字符
    p = string;
    while (CharT('\0') != *p && CharTraits::Chr((PSTRT)trimchars, *p))
        ++p;
    start = p;

    // 消除尾部字符
    while (CharT('\0') != *p)
    {
        if (CharTraits::Chr((PSTRT)trimchars, *p))
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
        *mark = CharT('\0');

    // 重新处理字符串
    int cnt = CharTraits::Len(start);
    if (start > string)
        memmove(string, start, (cnt + 1) * sizeof(CharT));
    return cnt;
}
