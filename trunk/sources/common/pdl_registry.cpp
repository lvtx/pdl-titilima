#include "..\..\include\pdl_registry.h"
#ifdef _WIN32_WCE
#include "..\adaptor\wince_adaptor.h"
#endif // #ifdef _WIN32_WCE

//////////////////////////////////////////////////////////////////////////
// LRegKey

LRegKey::LRegKey(void)
{
    m_hKey = NULL;
}

LRegKey::~LRegKey(void)
{
    Close();
}

LRegKey::operator HKEY(void) const
{
    return m_hKey;
}

LONG LRegKey::Close(void)
{
    LONG lRet = ERROR_SUCCESS;
    if (NULL != m_hKey)
    {
        lRet = ::RegCloseKey(m_hKey);
        m_hKey = NULL;
    }
    return lRet;
}

LONG LRegKey::Create(
    __in HKEY hKeyParent,
    __in PCSTR lpszKeyName,
    __in REGSAM samDesired /* = KEY_ALL_ACCESS */)
{
    PDLASSERT(NULL != hKeyParent);

    DWORD dw;
    HKEY hKey = NULL;
    LONG lRet = ::RegCreateKeyExA(hKeyParent, lpszKeyName, 0, NULL,
        REG_OPTION_NON_VOLATILE, samDesired, NULL, &hKey, &dw);
    if (ERROR_SUCCESS == lRet)
    {
        lRet = Close();
        PDLASSERT(ERROR_SUCCESS == lRet);
        m_hKey = hKey;
    }
    return lRet;
}

LONG LRegKey::Create(
    __in HKEY hKeyParent,
    __in PCWSTR lpszKeyName,
    __in REGSAM samDesired /* = KEY_ALL_ACCESS */)
{
    PDLASSERT(NULL != hKeyParent);

    DWORD dw;
    HKEY hKey = NULL;
    LONG lRet = ::RegCreateKeyExW(hKeyParent, lpszKeyName, 0, NULL,
        REG_OPTION_NON_VOLATILE, samDesired, NULL, &hKey, &dw);
    if (ERROR_SUCCESS == lRet)
    {
        lRet = Close();
        PDLASSERT(ERROR_SUCCESS == lRet);
        m_hKey = hKey;
    }
    return lRet;
}

LONG LRegKey::DeleteKey(__in PCSTR lpSubKey)
{
    PDLASSERT(NULL != m_hKey);
    return ::RegDeleteKeyA(m_hKey, lpSubKey);
}

LONG LRegKey::DeleteKey(__in PCWSTR lpSubKey)
{
    PDLASSERT(NULL != m_hKey);
    return ::RegDeleteKeyW(m_hKey, lpSubKey);
}

LONG LRegKey::DeleteValue(__in PCSTR lpValueName)
{
    PDLASSERT(NULL != m_hKey);
    return ::RegDeleteValueA(m_hKey, lpValueName);
}

LONG LRegKey::DeleteValue(__in PCWSTR lpValueName)
{
    PDLASSERT(NULL != m_hKey);
    return ::RegDeleteValueW(m_hKey, lpValueName);
}

LONG LRegKey::EnumKey(
    __in DWORD dwIndex,
    __out PSTR lpName,
    __out LPDWORD lpcName,
    __out PFILETIME lpftLastWriteTime)
{
    PDLASSERT(NULL != m_hKey);
    return ::RegEnumKeyExA(m_hKey, dwIndex, lpName, lpcName, NULL, NULL,
        NULL, lpftLastWriteTime);
}

LONG LRegKey::EnumKey(
    __in DWORD dwIndex,
    __out PWSTR lpName,
    __out LPDWORD lpcName,
    __out PFILETIME lpftLastWriteTime)
{
    PDLASSERT(NULL != m_hKey);
    return ::RegEnumKeyExW(m_hKey, dwIndex, lpName, lpcName, NULL, NULL,
        NULL, lpftLastWriteTime);
}

LONG LRegKey::EnumValue(
    __in DWORD dwIndex,
    __out PSTR lpValueName,
    __inout LPDWORD lpcValueName,
    __out LPDWORD lpType,
    __out LPBYTE lpData,
    __inout LPDWORD lpcbData)
{
    PDLASSERT(NULL != m_hKey);
    return ::RegEnumValueA(m_hKey, dwIndex, lpValueName, lpcValueName, NULL, lpType,
        lpData, lpcbData);
}

LONG LRegKey::EnumValue(
    __in DWORD dwIndex,
    __out PWSTR lpValueName,
    __inout LPDWORD lpcValueName,
    __out LPDWORD lpType,
    __out LPBYTE lpData,
    __inout LPDWORD lpcbData)
{
    PDLASSERT(NULL != m_hKey);
    return ::RegEnumValueW(m_hKey, dwIndex, lpValueName, lpcValueName, NULL, lpType,
        lpData, lpcbData);
}

LONG LRegKey::Open(
    __in HKEY hKeyParent,
    __in PCSTR lpszKeyName,
    __in REGSAM samDesired /* = KEY_ALL_ACCESS */)
{
    PDLASSERT(NULL != hKeyParent);

    HKEY hKey = NULL;
    LONG lRet = ::RegOpenKeyExA(hKeyParent, lpszKeyName, 0, samDesired, &hKey);
    if (ERROR_SUCCESS == lRet)
    {
        lRet = Close();
        PDLASSERT(ERROR_SUCCESS == lRet);
        m_hKey = hKey;
    }
    return lRet;
}

LONG LRegKey::Open(
    __in HKEY hKeyParent,
    __in PCWSTR lpszKeyName,
    __in REGSAM samDesired /* = KEY_ALL_ACCESS */)
{
    PDLASSERT(NULL != hKeyParent);

    HKEY hKey = NULL;
    LONG lRet = ::RegOpenKeyExW(hKeyParent, lpszKeyName, 0, samDesired, &hKey);
    if (ERROR_SUCCESS == lRet)
    {
        lRet = Close();
        PDLASSERT(ERROR_SUCCESS == lRet);
        m_hKey = hKey;
    }
    return lRet;
}

LONG LRegKey::QueryInfoA(
    __out LPDWORD lpcSubKeys,
    __out LPDWORD lpcMaxSubKeyLen,
    __out LPDWORD lpcValues,
    __out LPDWORD lpcMaxValueNameLen,
    __out LPDWORD lpcMaxValueLen,
    __out PFILETIME lpftLastWriteTime)
{
    PDLASSERT(NULL != m_hKey);
    return ::RegQueryInfoKeyA(m_hKey, NULL, NULL, NULL, lpcSubKeys, lpcMaxSubKeyLen,
        NULL, lpcValues, lpcMaxValueNameLen, lpcMaxValueLen, NULL, lpftLastWriteTime);
}

LONG LRegKey::QueryInfoW(
    __out LPDWORD lpcSubKeys,
    __out LPDWORD lpcMaxSubKeyLen,
    __out LPDWORD lpcValues,
    __out LPDWORD lpcMaxValueNameLen,
    __out LPDWORD lpcMaxValueLen,
    __out PFILETIME lpftLastWriteTime)
{
    PDLASSERT(NULL != m_hKey);
    return ::RegQueryInfoKeyW(m_hKey, NULL, NULL, NULL, lpcSubKeys, lpcMaxSubKeyLen,
        NULL, lpcValues, lpcMaxValueNameLen, lpcMaxValueLen, NULL, lpftLastWriteTime);
}

LONG LRegKey::QueryStringValue(
    __in PCSTR lpValueName,
    __out LStringA *strRet,
    __in BOOL bExpand /* = TRUE */)
{
    CHAR strTemp[1] = { '\0' };
    DWORD dwType;
    DWORD dwNeeded = sizeof(CHAR);

    LONG lRet = QueryValue(lpValueName, &dwType, (PBYTE)strTemp, &dwNeeded);
    if (ERROR_SUCCESS != lRet && ERROR_MORE_DATA != lRet)
        return lRet;
    if (REG_SZ != dwType && REG_EXPAND_SZ != dwType)
        return ERROR_INVALID_DATA;

    PSTR buf = strRet->AllocBuffer(dwNeeded / sizeof(CHAR) - 1, FALSE);
    QueryValue(lpValueName, NULL, (LPBYTE)buf, &dwNeeded);
    if (REG_EXPAND_SZ == dwType && bExpand)
        strRet->ExpandEnvironment();
    return ERROR_SUCCESS;
}

LONG LRegKey::QueryStringValue(
    __in PCWSTR lpValueName,
    __out LStringW *strRet,
    __in BOOL bExpand /* = TRUE */)
{
    WCHAR strTemp[1] = { L'\0' };
    DWORD dwType;
    DWORD dwNeeded = sizeof(WCHAR);

    LONG lRet = QueryValue(lpValueName, &dwType, (PBYTE)strTemp, &dwNeeded);
    if (ERROR_SUCCESS != lRet && ERROR_MORE_DATA != lRet)
        return lRet;
    if (REG_SZ != dwType && REG_EXPAND_SZ != dwType)
        return ERROR_INVALID_DATA;

    PWSTR buf = strRet->AllocBuffer(dwNeeded / sizeof(WCHAR) - 1, FALSE);
    QueryValue(lpValueName, NULL, (LPBYTE)buf, &dwNeeded);
    if (REG_EXPAND_SZ == dwType && bExpand)
        strRet->ExpandEnvironment();
    return ERROR_SUCCESS;
}

LONG LRegKey::QueryValue(
    __in PCSTR lpValueName,
    __out_opt LPDWORD lpType,
    __out LPBYTE lpData,
    __inout LPDWORD lpcbData)
{
    return ::RegQueryValueExA(m_hKey, lpValueName, NULL, lpType,
        lpData, lpcbData);
}

LONG LRegKey::QueryValue(
    __in PCWSTR lpValueName,
    __out_opt LPDWORD lpType,
    __out LPBYTE lpData,
    __inout LPDWORD lpcbData)
{
    return ::RegQueryValueExW(m_hKey, lpValueName, NULL, lpType,
        lpData, lpcbData);
}

LONG LRegKey::SetStringValue(__in PCSTR lpValueName, __in PCSTR lpValue)
{
    return SetValue(lpValueName, REG_SZ, (const BYTE*)lpValue,
        strlen(lpValue) + 1);
}

LONG LRegKey::SetStringValue(__in PCWSTR lpValueName, __in PCWSTR lpValue)
{
    return SetValue(lpValueName, REG_SZ, (const BYTE*)lpValue,
        (wcslen(lpValue) + 1) * sizeof(WCHAR));
}

LONG LRegKey::SetValue(
    __in PCSTR lpValueName,
    __in DWORD dwType,
    __in const BYTE* lpData,
    __in DWORD cbData)
{
    return ::RegSetValueExA(m_hKey, lpValueName, NULL, dwType, lpData, cbData);
}

LONG LRegKey::SetValue(
    __in PCWSTR lpValueName,
    __in DWORD dwType,
    __in const BYTE* lpData,
    __in DWORD cbData)
{
    return ::RegSetValueExW(m_hKey, lpValueName, NULL, dwType, lpData, cbData);
}
