/**
 * \file pdl_string.h
 * \brief PDL ×Ö·û´®·â×°
 */

#pragma once

#include "pdl_base.h"
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
    LStringA& operator=(__in const LStringA& str);
    const LStringA& operator+=(__in PCSTR lpszString);
    const LStringA& operator+=(__in char ch);
    char operator[](__in int idx);
public:
    PSTR AllocBuffer(__in DWORD nChars, __in BOOL bSaveData = TRUE);
    static PSTR AllocString(__in PCSTR lpString);
    void Append(__in char ch);
    void Attach(__in PSTR lpszString);
    int Compare(__in PCSTR lpszString, __in BOOL bCase = TRUE);
    BSTR ConvertToBSTR(void);
    void Copy(__in PCSTR lpszString);
    void Copy(__in PCWSTR lpszString);
    PSTR Detach(void);
    void Empty(void);
    BOOL ExpandEnvironment(void);
    int Find(__in char ch, int iStart = 0);
    int Find(__in PCSTR pszSub, int iStart = 0);
    int Format(__in PCSTR lpszFormat, ...);
    static void FreeString(__in PSTR lpString);
    int GetLength(void) const;
    BOOL IsEmpty(void) const;
    LStringA Left(__in int nChars);
    LStringA Mid(__in int iStart, __in int nChars = -1);
    int Replace(__in PCSTR pszOld, __in PCSTR pszNew);
    void ReplaceBackslashChars(void);
    int ReverseFind(__in char ch);
    int SetAt(__in int pos, __in char ch);
    void ToLower(void);
    void ToUpper(void);
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
    LStringW& operator=(__in const LStringW& str);
    const LStringW& operator+=(__in PCWSTR lpszString);
    const LStringW& operator+=(__in WCHAR ch);
    WCHAR operator[](__in int idx);
public:
    PWSTR AllocBuffer(__in DWORD nChars, __in BOOL bSaveData = TRUE);
    static PWSTR AllocString(__in PCWSTR lpString);
    void Append(__in WCHAR ch);
    void Attach(__in PWSTR lpszString);
    int Compare(__in PCWSTR lpszString, __in BOOL bCase = TRUE);
    void Copy(__in PCSTR lpszString);
    void Copy(__in PCWSTR lpszString);
    BSTR ConvertToBSTR(void);
    PWSTR Detach(void);
    void Empty(void);
    BOOL ExpandEnvironment(void);
    int Find(__in WCHAR ch, int iStart = 0);
    int Find(__in PCWSTR pszSub, int iStart = 0);
    int Format(__in PCWSTR lpszFormat, ...);
    static void FreeString(__in PWSTR lpString);
    int GetLength(void) const;
    BOOL IsEmpty(void) const;
    LStringW Left(__in int nChars);
    LStringW Mid(__in int iStart, __in int nChars = -1);
    int Replace(__in PCWSTR pszOld, __in PCWSTR pszNew);
    void ReplaceBackslashChars(void);
    int ReverseFind(__in WCHAR ch);
    int SetAt(__in int pos, __in WCHAR ch);
    void ToLower(void);
    void ToUpper(void);
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
