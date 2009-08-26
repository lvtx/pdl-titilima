/**
 * \file pdl_string.h
 * \brief PDL ×Ö·û´®·â×°
 */

#pragma once

#include <pdl_base.h>
#include <comdef.h>

/**
 * \class LStringA
 * \brief PDL ANSI ×Ö·û´®Àà
 */

class LStringA
{
public:
    LStringA(void);
    LStringA(__in PCSTR lpszString);
    LStringA(__in PCWSTR lpszString);
    LStringA(__in const LStringA& obj);
    ~LStringA(void);
    operator PCSTR(void) const;
    operator PSTR(void) const;
    LStringA& operator=(__in PCSTR lpszString);
    LStringA& operator=(__in PCWSTR lpszString);
    const LStringA& operator+=(__in PCSTR lpszString);
    char operator[](__in int idx);
public:
    PSTR AllocBuffer(__in DWORD nChars, __in BOOL bSaveData = TRUE);
    void Attach(__in PSTR lpszString);
    BSTR ConvertToBSTR(void);
    void Copy(__in PCSTR lpszString);
    void Copy(__in PCWSTR lpszString);
    PSTR Detach(void);
    int Find(__in char ch, int iStart = 0);
    int Find(__in PCSTR pszSub, int iStart = 0);
    BOOL Format(__in PCSTR lpszFormat, ...);
    BOOL FormatV(__in PCSTR lpszFormat, __in va_list argList);
    int GetLength(void) const;
    LStringA Left(__in int nChars);
    LStringA Mid(__in int iStart, int nChars = -1);
    int Replace(__in PCSTR pszOld, __in PCSTR pszNew);
    static int Trim(__in PSTR string, __in PCSTR trimchars);
    int Trim(__in PCSTR trimchars);
private:
    PSTR m_lpszData;
    DWORD m_dwMaxLen;
};

/**
 * \class LStringW
 * \brief PDL Unicode ×Ö·û´®·â×°
 */

class LStringW
{
public:
    LStringW(void);
    LStringW(__in PCSTR lpszString);
    LStringW(__in PCWSTR lpszString);
    LStringW(__in const LStringW& obj);
    ~LStringW(void);
    operator PCWSTR(void) const;
    operator PWSTR(void) const;
    LStringW& operator=(__in PCSTR lpszString);
    LStringW& operator=(__in PCWSTR lpszString);
    const LStringW& operator+=(__in PCWSTR lpszString);
    WCHAR operator[](__in int idx);
public:
    PWSTR AllocBuffer(__in DWORD nChars, __in BOOL bSaveData = TRUE);
    void Attach(__in PWSTR lpszString);
    void Copy(__in PCSTR lpszString);
    void Copy(__in PCWSTR lpszString);
    BSTR ConvertToBSTR(void);
    PWSTR Detach(void);
    int Find(__in WCHAR ch, int iStart = 0);
    int Find(__in PCWSTR pszSub, int iStart = 0);
    BOOL Format(__in PCWSTR lpszFormat, ...);
    BOOL FormatV(__in PCWSTR lpszFormat, __in va_list argList);
    int GetLength(void) const;
    LStringW Left(__in int nChars);
    LStringW Mid(__in int iStart, int nChars = -1);
    int Replace(__in PCWSTR pszOld, __in PCWSTR pszNew);
    static int Trim(__in PWSTR string, __in PCWSTR trimchars);
    int Trim(__in PCWSTR trimchars);
private:
    PWSTR m_lpszData;
    DWORD m_dwMaxLen;
};

#ifdef UNICODE
typedef LStringW LString;
#else // !UNICODE
typedef LStringA LString;
#endif // UNICODE
