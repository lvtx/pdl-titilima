/**
 * \file pdl_registry.h
 * \brief PDL 注册表操作封装
 */

#pragma once

#include "pdl_base.h"
#include "pdl_string.h"

/**
 * \class LRegKey
 * \brief PDL 注册表操作类
 */

#define HKCR        HKEY_CLASSES_ROOT
#define HKCU        HKEY_CURRENT_USER
#define HKLM        HKEY_LOCAL_MACHINE
#define DEFVALUE    ((PCTSTR)NULL)

#ifdef UNICODE
#define QueryInfo   QueryInfoW
#else
#define QueryInfo   QueryInfoA
#endif // UNICODE

class LRegKey
{
public:
    LRegKey(void);
    ~LRegKey(void);
    operator HKEY(void) const;
public:
    LONG Close(void);
    LONG Create(__in HKEY hKeyParent, __in PCSTR lpszKeyName,
        __in REGSAM samDesired = KEY_ALL_ACCESS);
    LONG Create(__in HKEY hKeyParent, __in PCWSTR lpszKeyName,
        __in REGSAM samDesired = KEY_ALL_ACCESS);
    LONG DeleteKey(__in PCSTR lpSubKey);
    LONG DeleteKey(__in PCWSTR lpSubKey);
    LONG DeleteValue(__in PCSTR lpValueName);
    LONG DeleteValue(__in PCWSTR lpValueName);
    LONG EnumKey(__in DWORD dwIndex, __out PSTR lpName,
        __out LPDWORD lpcName, __out PFILETIME lpftLastWriteTime);
    LONG EnumKey(__in DWORD dwIndex, __out PWSTR lpName,
        __out LPDWORD lpcName, __out PFILETIME lpftLastWriteTime);
    LONG EnumValue(__in DWORD dwIndex, __out PSTR lpValueName,
        __inout LPDWORD lpcValueName, __out LPDWORD lpType,
        __out LPBYTE lpData, __inout LPDWORD lpcbData);
    LONG EnumValue(__in DWORD dwIndex, __out PWSTR lpValueName,
        __inout LPDWORD lpcValueName, __out LPDWORD lpType,
        __out LPBYTE lpData, __inout LPDWORD lpcbData);
    LONG Open(__in HKEY hKeyParent, __in PCSTR lpszKeyName,
        __in REGSAM samDesired = KEY_ALL_ACCESS);
    LONG Open(__in HKEY hKeyParent, __in PCWSTR lpszKeyName,
        __in REGSAM samDesired = KEY_ALL_ACCESS);
    LONG QueryInfoA(__out LPDWORD lpcSubKeys, __out LPDWORD lpcMaxSubKeyLen,
        __out LPDWORD lpcValues, __out LPDWORD lpcMaxValueNameLen,
        __out LPDWORD lpcMaxValueLen, __out PFILETIME lpftLastWriteTime);
    LONG QueryInfoW(__out LPDWORD lpcSubKeys, __out LPDWORD lpcMaxSubKeyLen,
        __out LPDWORD lpcValues, __out LPDWORD lpcMaxValueNameLen,
        __out LPDWORD lpcMaxValueLen, __out PFILETIME lpftLastWriteTime);
    LONG QueryStringValue(__in PCSTR lpValueName, __out LStringA *strRet);
    LONG QueryStringValue(__in PCWSTR lpValueName, __out LStringW *strRet);
    LONG QueryValue(__in PCSTR lpValueName, __out_opt PDWORD lpType,
        __out PBYTE lpData, __inout PDWORD lpcbData);
    LONG QueryValue(__in PCWSTR lpValueName, __out_opt PDWORD lpType,
        __out PBYTE lpData, __inout PDWORD lpcbData);
    LONG SetStringValue(__in PCSTR lpValueName, __in PCSTR lpValue);
    LONG SetStringValue(__in PCWSTR lpValueName, __in PCWSTR lpValue);
    LONG SetValue(__in PCSTR lpValueName, __in DWORD dwType,
        __in const BYTE* lpData, __in DWORD cbData);
    LONG SetValue(__in PCWSTR lpValueName, __in DWORD dwType,
        __in const BYTE* lpData, __in DWORD cbData);
protected:
    HKEY m_hKey;
};
