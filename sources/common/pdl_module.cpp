#include "..\..\include\pdl_module.h"
#include "..\..\include\pdl_file.h"
#include "..\..\include\pdl_registry.h"
#include <comdef.h>

#define PDL_INIT_WNDDATA    2
#define RT_STRINGW          MAKEINTRESOURCEW(6)

LAppModule* LAppModule::m_pApp = NULL;
UINT WM_PDL_GETOBJECTA = 0;
UINT WM_PDL_GETOBJECTW = 0;
UINT WM_PDL_GETNOTIFY = 0;
static LFile* g_log = NULL;

LAppModule::LAppModule(__in HINSTANCE hInstance)
{
    m_hInstance = hInstance;
    m_cntWndData = 0;
    m_maxWndData = PDL_INIT_WNDDATA;
    m_pvWndData = new PVOID[PDL_INIT_WNDDATA];

    // 检测日志标志，并决定是否生成日志
    LString strPath, strFile;
    GetAppPath(&strPath);
    strFile = strPath;
    strFile += _T("debuglog.yes");
    if (LFile::Exists(strFile))
    {
        SYSTEMTIME st;
        LString str;
        GetLocalTime(&st);
        str.Format(_T("%d%02d%02d%02d%02d%02d.log"),
            st.wYear, st.wMonth, st.wDay,
            st.wHour, st.wMinute, st.wSecond);
        strFile = strPath;
        strFile += str;

        g_log = new LFile;
        g_log->Create(strFile, GENERIC_WRITE, FILE_SHARE_READ, CREATE_ALWAYS);
        PDLTRACE(_T("[PDL] log file: %s\n"), (PCTSTR)strFile);
    }

    // 初始化 PDL 窗口消息
    WM_PDL_GETOBJECTA = ::RegisterWindowMessage(_T("WM_PDL_GETOBJECTA"));
    WM_PDL_GETOBJECTW = ::RegisterWindowMessage(_T("WM_PDL_GETOBJECTW"));
    WM_PDL_GETNOTIFY = ::RegisterWindowMessage(_T("WM_PDL_GETNOTIFY"));
}

LAppModule::~LAppModule(void)
{
    if (NULL != g_log)
    {
        g_log->Close();
        delete g_log;
        g_log = NULL;
    }
}

void LAppModule::AddWndData(__in PVOID lpWndData)
{
    if (m_cntWndData == m_maxWndData)
    {
        m_maxWndData *= 2;
        PVOID* newData = new PVOID[m_maxWndData];
        CopyMemory(newData, m_pvWndData, sizeof(PVOID) * m_cntWndData);
        delete [] m_pvWndData;
        m_pvWndData = newData;
    }

    m_pvWndData[m_cntWndData] = lpWndData;
    ++m_cntWndData;
}

void LAppModule::DebugPrint(__in PCSTR lpszFormat, ...)
{
    if (NULL == g_log)
        return;

    char str[1024];
    va_list arglist;
    va_start(arglist, lpszFormat);
    int cnt = wvsprintfA(str, lpszFormat, arglist);
    va_end(arglist);

    g_log->Write(str, cnt);
}

void LAppModule::DebugPrint(__in PCWSTR lpszFormat, ...)
{
    if (NULL == g_log)
        return;

    WCHAR str[1024];
    va_list arglist;
    va_start(arglist, lpszFormat);
    int cnt = wvsprintfW(str, lpszFormat, arglist);
    va_end(arglist);

    LStringA strA = str;
    g_log->Write(strA, strA.GetLength());
}

BOOL LAppModule::Destroy(void)
{
    if (NULL != m_pApp)
    {
        delete m_pApp;
        m_pApp = NULL;
        return TRUE;
    }
    return FALSE;
}

PVOID LAppModule::ExtractWndData(void)
{
    PVOID ret = m_pvWndData[0];
    --m_cntWndData;
    MoveMemory(m_pvWndData, m_pvWndData + 1, sizeof(PVOID) * m_cntWndData);
    return ret;
}

HRSRC LAppModule::FindResourceA(__in PCSTR lpName, __in PCSTR lpType)
{
    return ::FindResourceA(m_hInstance, lpName, lpType);
}

HRSRC LAppModule::FindResourceW(__in PCWSTR lpName, __in PCWSTR lpType)
{
    return ::FindResourceW(m_hInstance, lpName, lpType);
}

LAppModule* LAppModule::GetApp(void)
{
    return m_pApp;
}

BOOL PDLAPI LAppModule::GetAppName(__out LStringA* name, __in BOOL bFullPath)
{
#ifndef _WIN32_WCE
    LStringA str;
    DWORD dwSize = MAX_PATH;
    PSTR buf = str.AllocBuffer(dwSize, FALSE);
    DWORD ret = ::GetModuleFileNameA(NULL, buf, dwSize);
    if (0 == ret)
        return FALSE;

    while (ret == dwSize && ERROR_INSUFFICIENT_BUFFER == ::GetLastError())
    {
        dwSize *= 2;
        buf = str.AllocBuffer(dwSize, FALSE);
        ret = ::GetModuleFileNameA(NULL, buf, dwSize);
    }

    if (bFullPath)
        name->Copy(buf);
    else
        name->Copy(strrchr(buf, '\\') + 1);
#else
    LStringW strW;
    if (!GetAppName(&strW, bFullPath))
        return FALSE;

    name->Copy(strW);
#endif // _WIN32_WCE
    return TRUE;
}

BOOL PDLAPI LAppModule::GetAppName(__out LStringW* name, __in BOOL bFullPath)
{
    LStringW str;
    DWORD dwSize = MAX_PATH;
    PWSTR buf = str.AllocBuffer(dwSize, FALSE);
    DWORD ret = ::GetModuleFileNameW(NULL, buf, dwSize);
    if (0 == ret)
        return FALSE;

    while (ret == dwSize && ERROR_INSUFFICIENT_BUFFER == ::GetLastError())
    {
        dwSize *= 2;
        buf = str.AllocBuffer(dwSize, FALSE);
        ret = ::GetModuleFileNameW(NULL, buf, dwSize);
    }

    if (bFullPath)
        name->Copy(buf);
    else
        name->Copy(wcsrchr(buf, L'\\') + 1);
    return TRUE;
}

BOOL LAppModule::GetAppPath(__out LStringA* path)
{
#ifndef _WIN32_WCE
    DWORD dwSize = MAX_PATH;
    PSTR buf = path->AllocBuffer(dwSize, FALSE);
    DWORD ret = ::GetModuleFileNameA(NULL, buf, dwSize);
    if (0 == ret)
        return FALSE;

    while (ret == dwSize && ERROR_INSUFFICIENT_BUFFER == ::GetLastError())
    {
        dwSize *= 2;
        buf = path->AllocBuffer(dwSize, FALSE);
        ret = ::GetModuleFileNameA(NULL, buf, dwSize);
    }

    *(strrchr(buf, '\\') + 1) = '\0';
#else
    LStringW strW;
    if (!GetAppPath(&strW))
        return FALSE;

    path->Copy(strW);
#endif // _WIN32_WCE
    return TRUE;
}

BOOL LAppModule::GetAppPath(__out LStringW* path)
{
    DWORD dwSize = MAX_PATH;
    PWSTR buf = path->AllocBuffer(dwSize, FALSE);
    DWORD ret = ::GetModuleFileNameW(NULL, buf, dwSize);
    if (0 == ret)
        return FALSE;

    while (ret == dwSize && ERROR_INSUFFICIENT_BUFFER == ::GetLastError())
    {
        dwSize *= 2;
        buf = path->AllocBuffer(dwSize, FALSE);
        ret = ::GetModuleFileNameW(NULL, buf, dwSize);
    }

    *(wcsrchr(buf, L'\\') + 1) = L'\0';
    return TRUE;
}

HINSTANCE LAppModule::GetInstance(void) const
{
    return m_hInstance;
}

LAppModule* LAppModule::Initialize(__in HINSTANCE hInstance)
{
    if (NULL == hInstance)
        hInstance = ::GetModuleHandle(NULL);

    if (NULL == m_pApp)
        m_pApp = new LAppModule(hInstance);
    return m_pApp;
}

HACCEL LAppModule::LoadAcceleratorsA(__in PCSTR lpTableName)
{
    return ::LoadAcceleratorsA(m_hInstance, lpTableName);
}

HACCEL LAppModule::LoadAcceleratorsW(__in PCWSTR lpTableName)
{
    return ::LoadAcceleratorsW(m_hInstance, lpTableName);
}

HBITMAP LAppModule::LoadBitmapA(__in PCSTR lpBitmapName)
{
#ifdef _WIN32_WCE
    return NULL;
#else
    return ::LoadBitmapA(m_hInstance, lpBitmapName);
#endif // _WIN32_WCE
}

HBITMAP LAppModule::LoadBitmapW(__in PCWSTR lpBitmapName)
{
    return ::LoadBitmapW(m_hInstance, lpBitmapName);
}

HCURSOR LAppModule::LoadCursorA(__in PCSTR lpCursorName)
{
#ifdef _WIN32_WCE
    return NULL;
#else
    return ::LoadCursorA(m_hInstance, lpCursorName);
#endif // _WIN32_WCE
}

HCURSOR LAppModule::LoadCursorW(__in PCWSTR lpCursorName)
{
    return ::LoadCursorW(m_hInstance, lpCursorName);
}

HICON LAppModule::LoadIconA(__in PCSTR lpIconName)
{
#ifdef _WIN32_WCE
    return NULL;
#else
    return ::LoadIconA(m_hInstance, lpIconName);
#endif // _WIN32_WCE
}

HICON LAppModule::LoadIconW(__in PCWSTR lpIconName)
{
    return ::LoadIconW(m_hInstance, lpIconName);
}

HANDLE LAppModule::LoadImageA(
    __in PCSTR name,
    __in UINT type,
    __in int cx, __in int cy,
    __in UINT fuLoad)
{
#ifdef _WIN32_WCE
    return NULL;
#else
    return ::LoadImageA(m_hInstance, name, type, cx, cy, fuLoad);
#endif // _WIN32_WCE
}

HANDLE LAppModule::LoadImageW(
    __in PCWSTR name,
    __in UINT type,
    __in int cx, __in int cy,
    __in UINT fuLoad)
{
    return ::LoadImageW(m_hInstance, name, type, cx, cy, fuLoad);
}

HMENU LAppModule::LoadMenuA(__in PCSTR lpMenuName)
{
#ifdef _WIN32_WCE
    return NULL;
#else
    return ::LoadMenuA(m_hInstance, lpMenuName);
#endif // _WIN32_WCE
}

HMENU LAppModule::LoadMenuW(__in PCWSTR lpMenuName)
{
    return ::LoadMenuW(m_hInstance, lpMenuName);
}

HGLOBAL LAppModule::LoadResource(__in HRSRC hResInfo)
{
    return ::LoadResource(m_hInstance, hResInfo);
}

int LAppModule::LoadStringA(
    __in UINT uID,
    __out PSTR lpBuffer,
    __in int nBufferMax)
{
#ifdef _WIN32_WCE
    return 0;
#else
    PDLASSERT(NULL != lpBuffer);

    return ::LoadStringA(m_hInstance, uID, lpBuffer, nBufferMax);
#endif // _WIN32_WCE
}

int LAppModule::LoadStringW(
    __in UINT uID,
    __out PWSTR lpBuffer,
    __in int nBufferMax)
{
    PDLASSERT(NULL != lpBuffer);
    return ::LoadStringW(m_hInstance, uID, lpBuffer, nBufferMax);
}

int LAppModule::LoadStringA(__in UINT id, __out LStringA* str)
{
    LStringW strW;
    LoadStringW(id, &strW);
    str->Copy(strW);
    return str->GetLength();
}

int LAppModule::LoadStringW(__in UINT id, __out LStringW* str)
{
    HRSRC hResInfo = FindResourceW(MAKEINTRESOURCEW((id >> 4) + 1),
        RT_STRINGW);
    HGLOBAL hResData = LoadResource(hResInfo);

    PCWSTR p = (PCWSTR)::LockResource(hResData);
    id &= 0x0f;
    DWORD cch = 0;
    for (;;)
    {
        cch = *p;
        ++p;
        if (0 == id)
            break;
        --id;
        p += cch;
    }
    wcsncpy(str->AllocBuffer(cch, FALSE), p, cch);

    ::FreeResource(hResData);
    return cch;
}

DWORD LAppModule::SizeofResource(__in HRSRC hResInfo)
{
    return ::SizeofResource(m_hInstance, hResInfo);
}
