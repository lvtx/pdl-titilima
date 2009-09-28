#include "..\..\include\pdl_window.h"
#include "..\..\include\pdl_module.h"
#include "..\..\include\pdl_gdi.h"
#include "..\..\include\pdl_parser.h"
#include "thunk.h"

#define MAKEINTATOMA(i)  (PSTR)((ULONG_PTR)((WORD)(i)))
#define MAKEINTATOMW(i)  (PWSTR)((ULONG_PTR)((WORD)(i)))

///////////////////////////////////////////////////////////////////////////////
// LWnd

LWnd::LWnd(HWND hWnd /* = NULL */) : m_hWnd(hWnd)
{
}

BOOL LWnd::AnimateWindow(__in DWORD dwTime, __in DWORD dwFlags)
{
    PDLASSERT(NULL != m_hWnd);
    return ::AnimateWindow(m_hWnd, dwTime, dwFlags);
}

void LWnd::Attach(__in HWND hWnd)
{
    PDLASSERT(NULL == m_hWnd);
    m_hWnd = hWnd;
}

BOOL LWnd::BringWindowToTop(void)
{
    PDLASSERT(NULL != m_hWnd);
    return ::BringWindowToTop(m_hWnd);
}

void LWnd::CenterWindow(
    __in LPCRECT lprc,
    __in DWORD dwPos,
    __in BOOL bRedraw /* = TRUE */)
{
    PDLASSERT(IsWindow());

    RECT rcParent, rcClient;
    if (NULL == lprc)
    {
        HWND hParent = GetParent();
        if (NULL == hParent)
            hParent = ::GetDesktopWindow();

        ::GetClientRect(hParent, &rcParent);
    }
    else
    {
        ::CopyRect(&rcParent, lprc);
    }

    GetClientRect(&rcClient);

    RECT rcPos;
    GetRectInParent(&rcPos);
    if (WNDPOS_HCENTER & dwPos)
    {
        rcPos.left = (rcParent.right + rcParent.left + rcClient.left
            - rcClient.right) / 2;
        rcPos.right = rcPos.left + rcClient.right;
    }
    if (WNDPOS_VCENTER & dwPos)
    {
        rcPos.top = (rcParent.bottom + rcParent.top + rcClient.top
            - rcClient.bottom) / 2;
        rcPos.bottom = rcPos.top + rcClient.bottom;
    }

    UINT uFlags = SWP_NOZORDER | SWP_NOSIZE;
#ifndef _WIN32_WCE
    if (!bRedraw)
    {
        uFlags |= SWP_NOREDRAW;
    }
#endif // _WIN32_WCE
    SetWindowPos(NULL, &rcPos, uFlags);
}

BOOL LWnd::CheckDlgButton(__in int nIDButton, __in UINT uCheck)
{
    PDLASSERT(IsWindow());
    return ::CheckDlgButton(m_hWnd, nIDButton, uCheck);
}

BOOL LWnd::CheckRadioButton(__in int nIDFirstButton, __in int nIDLastButton,
                            __in int nIDCheckButton)
{
    PDLASSERT(IsWindow());
    return ::CheckRadioButton(m_hWnd, nIDFirstButton, nIDLastButton, nIDCheckButton);
}

BOOL LWnd::ClientToScreen(__inout LPPOINT lpPoint)
{
    PDLASSERT(IsWindow());
    return ::ClientToScreen(m_hWnd, lpPoint);
}

void LWnd::Copy(void)
{
    SendMessage(WM_COPY);
}

BOOL LWnd::Create(
    __in PCSTR lpClassName,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    if (NULL == lpRect)
    {
        if (NULL == hWndParent)
        {
            return Create(lpClassName, lpWindowName, dwStyle, CW_USEDEFAULT,
                CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, hWndParent,
                (HMENU)(UINT_PTR)nID, lpParam);
        }
        else
        {
            RECT rc;
            ::GetClientRect(hWndParent, &rc);
            return Create(lpClassName, lpWindowName, dwStyle,
                rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top,
                hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
        }
    }
    else
    {
        return Create(lpClassName, lpWindowName, dwStyle, lpRect->left,
            lpRect->top, lpRect->right - lpRect->left,
            lpRect->bottom - lpRect->top, hWndParent, (HMENU)(UINT_PTR)nID,
            lpParam);
    }
}

BOOL LWnd::Create(
    __in PCWSTR lpClassName,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    if (NULL == lpRect)
    {
        if (NULL == hWndParent)
        {
            return Create(lpClassName, lpWindowName, dwStyle,
                CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
                hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
        }
        else
        {
            RECT rc;
            ::GetClientRect(hWndParent, &rc);
            return Create(lpClassName, lpWindowName, dwStyle,
                rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top,
                hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
        }
    }
    else
    {
        return Create(lpClassName, lpWindowName, dwStyle, lpRect->left,
            lpRect->top, lpRect->right - lpRect->left,
            lpRect->bottom - lpRect->top, hWndParent,
            (HMENU)(UINT_PTR)nID, lpParam);
    }
}

BOOL LWnd::Create(
    __in PCSTR lpClassName,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in HMENU hMenu,
    __in PVOID lpParam)
{
#ifdef _WIN32_WCE
    PDLASSERT(FALSE);
    return FALSE;
#else
    LAppModule *theApp = LAppModule::GetApp();

    m_hWnd = ::CreateWindowA(lpClassName, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, hMenu, theApp->GetInstance(), lpParam);

    return NULL != m_hWnd;
#endif // _WIN32_WCE
}

BOOL LWnd::Create(
    __in PCWSTR lpClassName,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in HMENU hMenu,
    __in PVOID lpParam)
{
    LAppModule *theApp = LAppModule::GetApp();

    m_hWnd = ::CreateWindowW(lpClassName, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, hMenu, theApp->GetInstance(), lpParam);

    return NULL != m_hWnd;
}

BOOL LWnd::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpClassName,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    if (NULL == lpRect)
    {
        if (NULL == hWndParent)
        {
            return CreateEx(dwExStyle, lpClassName, lpWindowName, dwStyle,
                CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
                hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
        }
        else
        {
            RECT rc;
            ::GetClientRect(hWndParent, &rc);
            return CreateEx(dwExStyle, lpClassName, lpWindowName, dwStyle,
                rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top,
                hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
        }
    }
    else
    {
        return CreateEx(dwExStyle, lpClassName, lpWindowName, dwStyle,
            lpRect->left, lpRect->top, lpRect->right - lpRect->left,
            lpRect->bottom - lpRect->top, hWndParent,
            (HMENU)(UINT_PTR)nID, lpParam);
    }
}

BOOL LWnd::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpClassName,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    if (NULL == lpRect)
    {
        if (NULL == hWndParent)
        {
            return CreateEx(dwExStyle, lpClassName, lpWindowName, dwStyle,
                CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
                hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
        }
        else
        {
            RECT rc;
            ::GetClientRect(hWndParent, &rc);
            return CreateEx(dwExStyle, lpClassName, lpWindowName, dwStyle,
                rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top,
                hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
        }
    }
    else
    {
        return CreateEx(dwExStyle, lpClassName, lpWindowName, dwStyle,
            lpRect->left, lpRect->top, lpRect->right - lpRect->left,
            lpRect->bottom - lpRect->top, hWndParent,
            (HMENU)(UINT_PTR)nID, lpParam);
    }
}

BOOL LWnd::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpClassName,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in HMENU hMenu,
    __in PVOID lpParam)
{
#ifdef _WIN32_WCE
    PDLASSERT(FALSE);
    return FALSE;
#else
    LAppModule *theApp = LAppModule::GetApp();

    m_hWnd = ::CreateWindowExA(dwExStyle, lpClassName, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, hMenu, theApp->GetInstance(),
        lpParam);

    return NULL != m_hWnd;
#endif // _WIN32_WCE
}

BOOL LWnd::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpClassName,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in HMENU hMenu,
    __in PVOID lpParam)
{
    LAppModule *theApp = LAppModule::GetApp();

    m_hWnd = ::CreateWindowExW(dwExStyle, lpClassName, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, hMenu, theApp->GetInstance(),
        lpParam);

    return NULL != m_hWnd;
}

BOOL LWnd::DestroyWindow(void)
{
    return ::DestroyWindow(m_hWnd);
}

HWND LWnd::Detach(void)
{
    HWND hRet = m_hWnd;
    m_hWnd    = NULL;
    return hRet;
}

BOOL LWnd::EnableDlgItem(__in int nIDDlgItem, __in BOOL bEnable /* = TRUE */)
{
    HWND hCtrl = GetDlgItem(nIDDlgItem);
    if (NULL == hCtrl)
    {
        return FALSE;
    }
    return ::EnableWindow(hCtrl, bEnable);
}

BOOL LWnd::EnableWindow(__in BOOL bEnable /* = TRUE */)
{
    return ::EnableWindow(m_hWnd, bEnable);
}

int LWnd::ErrorBox(
    __in DWORD dwErrCode,
    __in HWND hWnd,
    __in PCSTR lpText,
    __in PCSTR lpCaption,
    __in UINT uType)
{
    PSTR pszMsg = NULL;
    DWORD dwLen = FormatMessageA(
        FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER,
        NULL, dwErrCode, GetSystemDefaultLangID(), (PSTR)&pszMsg, 0, NULL);

    PSTR pszText = NULL;
    if (NULL != pszMsg)
    {
        DWORD dwSize = dwLen + 12; // 12 = "[XXXXXXXX] \0"
        if (NULL != lpText)
        {
            dwSize += strlen(lpText) + 4; // 4 = "\r\n\r\n"
            pszText = new char[dwSize];
            wsprintfA(pszText, "%s\r\n\r\n[%08X] %s", lpText, dwErrCode,
                pszMsg);
        }
        else
        {
            pszText = new char[dwSize];
            wsprintfA(pszText, "[%08X] %s", dwErrCode, pszMsg);
        }
        LocalFree((HLOCAL)pszMsg);
    }
    else
    {
        DWORD dwSize = 21; // 21 = "Error code: XXXXXXXX\0"
        if (NULL != lpText)
        {
            dwSize += strlen(lpText) + 4; // 4 = "\r\n\r\n"
            pszText = new char[dwSize];
            wsprintfA(pszText, "%s\r\n\r\nError code: %08X", lpText,
                dwErrCode);
        }
        else
        {
            pszText = new char[dwSize];
            wsprintfA(pszText, "Error code: %08X", dwErrCode);
        }
    }

    int nRet = ::MessageBoxA(hWnd, pszText, lpCaption, uType);
    delete [] pszText;

    return nRet;
}

int LWnd::ErrorBox(
    __in DWORD dwErrCode,
    __in HWND hWnd,
    __in PCWSTR lpText,
    __in PCWSTR lpCaption,
    __in UINT uType)
{
    PWSTR pszMsg = NULL;
    DWORD dwLen = FormatMessageW(
        FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER,
        NULL, dwErrCode, GetSystemDefaultLangID(), (PWSTR)&pszMsg, 0, NULL);

    PWSTR pszText = NULL;
    if (NULL != pszMsg)
    {
        DWORD dwSize = dwLen + 12; // 12 = "[XXXXXXXX] \0"
        if (NULL != lpText)
        {
            dwSize += wcslen(lpText) + 4; // 4 = "\r\n\r\n"
            pszText = new WCHAR[dwSize];
            wsprintfW(pszText, L"%s\r\n\r\n[%08X] %s", lpText, dwErrCode,
                pszMsg);
        }
        else
        {
            pszText = new WCHAR[dwSize];
            wsprintfW(pszText, L"[%08X] %s", dwErrCode, pszMsg);
        }
        LocalFree((HLOCAL)pszMsg);
    }
    else
    {
        DWORD dwSize = 21; // 21 = "Error code: XXXXXXXX\0"
        if (NULL != lpText)
        {
            dwSize += wcslen(lpText) + 4; // 4 = "\r\n\r\n"
            pszText = new WCHAR[dwSize];
            wsprintfW(pszText, L"%s\r\n\r\nError code: %08X", lpText,
                dwErrCode);
        }
        else
        {
            pszText = new WCHAR[dwSize];
            wsprintfW(pszText, L"Error code: %08X", dwErrCode);
        }
    }

    int nRet = ::MessageBoxW(hWnd, pszText, lpCaption, uType);
    delete [] pszText;

    return nRet;
}

DWORD LWnd::GetClassLongA(__in int nIndex)
{
    PDLASSERT(IsWindow());
    return ::GetClassLongA(m_hWnd, nIndex);
}

DWORD LWnd::GetClassLongW(__in int nIndex)
{
    PDLASSERT(IsWindow());
    return ::GetClassLongW(m_hWnd, nIndex);
}

BOOL LWnd::GetClientRect(__out LPRECT lpRect)
{
    PDLASSERT(IsWindow());
    return ::GetClientRect(m_hWnd, lpRect);
}

int LWnd::GetDlgCtrlID(void)
{
    return ::GetDlgCtrlID(m_hWnd);
}

HWND LWnd::GetDlgItem(__in int nIDDlgItem)
{
    return ::GetDlgItem(m_hWnd, nIDDlgItem);
}

UINT LWnd::GetDlgItemInt(
     __in int nIDDlgItem,
     __out_opt BOOL *lpTranslated,
     __in BOOL bSigned)
{
    return ::GetDlgItemInt(m_hWnd, nIDDlgItem, lpTranslated, bSigned);
}

UINT LWnd::GetDlgItemTextA(
     __in int nIDDlgItem,
     __in PSTR PSTRing,
     __in int nMaxCount)
{
#ifdef _WIN32_WCE
    PDLASSERT(FALSE);
    return 0;
#else
    return ::GetDlgItemTextA(m_hWnd, nIDDlgItem, PSTRing, nMaxCount);
#endif // _WIN32_WCE
}

UINT LWnd::GetDlgItemTextW(
     __in int nIDDlgItem,
     __in PWSTR PSTRing,
     __in int nMaxCount)
{
    return ::GetDlgItemTextW(m_hWnd, nIDDlgItem, PSTRing, nMaxCount);
}

DWORD LWnd::GetExStyle(void)
{
    return GetWindowLong(GWL_EXSTYLE);
}

HMENU LWnd::GetMenu(void)
{
#ifndef _WIN32_WCE
    return ::GetMenu(m_hWnd);
#else
    PDLASSERT(FALSE);
    return NULL;
#endif // _WIN32_WCE
}

HWND LWnd::GetParent(void)
{
    return ::GetParent(m_hWnd);
}

BOOL LWnd::GetRectInParent(__out LPRECT lpRect)
{
    PDLASSERT(NULL != lpRect);

    HWND hParent = GetParent();
    if (NULL == hParent)
        hParent = ::GetDesktopWindow();

    LWnd wndParent = hParent;

    // �������ڵ����Ͻ�ת��Ϊ��Ļ����
    POINT ptParent = { 0, 0 };
    if (!wndParent.ClientToScreen(&ptParent))
        return FALSE;

    // ȡ���ڵ���Ļ����
    RECT rc;
    if (!GetWindowRect(&rc))
        return FALSE;

    return SetRect(lpRect, rc.left - ptParent.x, rc.top - ptParent.y,
        rc.right - ptParent.x, rc.bottom - ptParent.y);
}

HWND LWnd::GetSafeHWND(void) const
{
    return IsWindow() ? m_hWnd : NULL;
}

BOOL LWnd::GetScrollInfo(__in int nBar, __inout LPSCROLLINFO lpsi)
{
    lpsi->cbSize = sizeof(SCROLLINFO);
    return ::GetScrollInfo(m_hWnd, nBar, lpsi);
}

BOOL LWnd::GetSizeRect(__out LPRECT lpRect)
{
    PDLASSERT(NULL != lpRect);

    RECT rc;
    if (!GetWindowRect(&rc))
        return FALSE;

    lpRect->left = 0;
    lpRect->top = 0;
    lpRect->right = rc.right - rc.left;
    lpRect->bottom = rc.bottom - rc.top;
    return TRUE;
}

DWORD LWnd::GetStyle(void)
{
    return GetWindowLong(GWL_STYLE);
}

HWND LWnd::GetWindow(__in UINT uCmd)
{
    return ::GetWindow(m_hWnd, uCmd);
}

LONG LWnd::GetWindowLongA(__in int nIndex)
{
    return ::GetWindowLongA(m_hWnd, nIndex);
}

LONG LWnd::GetWindowLongW(__in int nIndex)
{
    return ::GetWindowLongW(m_hWnd, nIndex);
}

BOOL LWnd::GetWindowRect(__out LPRECT lpRect)
{
    return ::GetWindowRect(m_hWnd, lpRect);
}

int LWnd::GetWindowTextA(__out PSTR lpString, __in int nMaxCount)
{
#ifdef _WIN32_WCE
    LStringA str;
    int len = GetWindowTextA(&str);
    strcpy(lpString, str, nMaxCount);
    return len > nMaxCount ? nMaxCount : len;
#else
    return ::GetWindowTextA(m_hWnd, lpString, nMaxCount);
#endif // _WIN32_WCE
}

int LWnd::GetWindowTextW(__out PWSTR lpString, __in int nMaxCount)
{
    return ::GetWindowTextW(m_hWnd, lpString, nMaxCount);
}

int LWnd::GetWindowTextA(__out LStringA *pStr)
{
    PDLASSERT(NULL != pStr);
#ifdef _WIN32_WCE
    LStringW str;
    GetWindowTextW(&str);
    *pStr = str;
    return pStr->GetLength();
#else
    int nLength = GetWindowTextLengthA();
    PSTR buf = pStr->AllocBuffer(nLength, FALSE);
    GetWindowTextA(buf, nLength + 1);
    return nLength;
#endif // _WIN32_WCE
}

int LWnd::GetWindowTextW(__out LStringW *pStr)
{
    PDLASSERT(NULL != pStr);

    int nLength = GetWindowTextLengthW();
    PWSTR buf = pStr->AllocBuffer(nLength, FALSE);
    GetWindowTextW(buf, nLength + 1);
    return nLength;
}

int LWnd::GetWindowTextLengthA(void)
{
#ifdef _WIN32_WCE
    LStringW strW;
    GetWindowText(&strW);
    LStringA strA = strW;
    return strA->GetLength();
#else
    return ::GetWindowTextLengthA(m_hWnd);
#endif // _WIN32_WCE
}

int LWnd::GetWindowTextLengthW(void)
{
    return ::GetWindowTextLengthW(m_hWnd);
}

BOOL LWnd::KillTimer(__in UINT_PTR uIDEvent)
{
    return ::KillTimer(m_hWnd, uIDEvent);
}

BOOL LWnd::Invalidate(__in BOOL bErase /* = TRUE */)
{
    return InvalidateRect(NULL, bErase);
}

BOOL LWnd::InvalidateRect(__in LPCRECT lpRect, __in BOOL bErase /* = TRUE */)
{
    return ::InvalidateRect(m_hWnd, lpRect, bErase);
}

UINT LWnd::IsDlgButtonChecked(__in int nIDButton)
{
    return ::IsDlgButtonChecked(m_hWnd, nIDButton);
}

BOOL LWnd::IsWindow(void) const
{
    return ::IsWindow(m_hWnd);
}

BOOL LWnd::IsWindowUnicode(void)
{
    return ::IsWindowUnicode(m_hWnd);
}

BOOL LWnd::IsWindowVisible(void)
{
    return ::IsWindowVisible(m_hWnd);
}

int LWnd::MessageBoxA(
    __in PCSTR lpszText,
    __in PCSTR lpszCaption /* = NULL */,
    __in UINT nType /* = MB_OK */)
{
    PDLASSERT(IsWindow());
#ifdef _WIN32_WCE
    LStringW strText = lpszText;
    LStringW strCaption = lpszCaption;
    return ::MessageBoxW(m_hWnd, strText, strCaption, nType);
#else
    return ::MessageBoxA(m_hWnd, lpszText, lpszCaption, nType);
#endif // _WIN32_WCE
}

int LWnd::MessageBoxW(
    __in PCWSTR lpszText,
    __in PCWSTR lpszCaption /* = NULL */,
    __in UINT nType /* = MB_OK */)
{
    PDLASSERT(IsWindow());
    return ::MessageBoxW(m_hWnd, lpszText, lpszCaption, nType);
}

BOOL LWnd::MoveWindow(__in LPCRECT lprc, __in BOOL bRepaint /* = TRUE */)
{
    PDLASSERT(NULL != lprc);

    return MoveWindow(lprc->left, lprc->top, lprc->right - lprc->left,
        lprc->bottom - lprc->top, bRepaint);
}

BOOL LWnd::MoveWindow(__in int X, __in int Y, __in int nWidth,
                      __in int nHeight, __in BOOL bRepaint /* = TRUE */)
{
    PDLASSERT(IsWindow());
    return ::MoveWindow(m_hWnd, X, Y, nWidth, nHeight, bRepaint);
}

BOOL LWnd::OpenClipboard(void)
{
    PDLASSERT(IsWindow());
    return ::OpenClipboard(m_hWnd);
}

BOOL LWnd::PostMessageA(
    __in UINT uMsg,
    __in WPARAM wParam /* = 0 */,
    __in LPARAM lParam /* = 0 */)
{
#ifdef _WIN32_WCE
    return FALSE;
#else
    PDLASSERT(IsWindow());
    return ::PostMessageA(m_hWnd, uMsg, wParam, lParam);
#endif // _WIN32_WCE
}

BOOL LWnd::PostMessageW(
    __in UINT uMsg,
    __in WPARAM wParam /* = 0 */,
    __in LPARAM lParam /* = 0 */)
{
    PDLASSERT(IsWindow());
    return ::PostMessageW(m_hWnd, uMsg, wParam, lParam);
}

BOOL LWnd::ScreenToClient(__inout LPPOINT lpPoint)
{
    PDLASSERT(IsWindow());
    return ::ScreenToClient(m_hWnd, lpPoint);
}

BOOL LWnd::ScrollWindow(
    __in int XAmount, __in int YAmount,
    __in_opt CONST RECT *lpRect,
    __in_opt CONST RECT *lpClipRect)
{
    PDLASSERT(IsWindow());
    return ::ScrollWindow(m_hWnd, XAmount, YAmount, lpRect, lpClipRect);
}

LRESULT LWnd::SendMessageA(
    __in UINT uMsg,
    __in WPARAM wParam /* = 0 */,
    __in LPARAM lParam /* = 0 */)
{
#ifdef _WIN32_WCE
    PDLASSERT(FALSE);
    return 0;
#else
    PDLASSERT(IsWindow());
    return ::SendMessageA(m_hWnd, uMsg, wParam, lParam);
#endif // _WIN32_WCE
}

LRESULT LWnd::SendMessageW(
    __in UINT uMsg,
    __in WPARAM wParam /* = 0 */,
    __in LPARAM lParam /* = 0 */)
{
    PDLASSERT(IsWindow());
    return ::SendMessageW(m_hWnd, uMsg, wParam, lParam);
}

HWND LWnd::SetCapture(void)
{
    PDLASSERT(IsWindow());
    return ::SetCapture(m_hWnd);
}

BOOL LWnd::SetDlgItemInt(
    __in int nIDDlgItem,
    __in UINT uValue,
    __in BOOL bSigned)
{
    PDLASSERT(IsWindow());
    return ::SetDlgItemInt(m_hWnd, nIDDlgItem, uValue, bSigned);
}

BOOL LWnd::SetDlgItemTextA(__in int nID, __in PCSTR lpszString)
{
    PDLASSERT(IsWindow());
#ifdef _WIN32_WCE
    LStringW str = lpszString;
    return ::SetDlgItemTextW(m_hWnd, nID, str);
#else
    return ::SetDlgItemTextA(m_hWnd, nID, lpszString);
#endif // _WIN32_WCE
}

BOOL LWnd::SetDlgItemTextW(__in int nID, __in PCWSTR lpszString)
{
    PDLASSERT(IsWindow());
    return ::SetDlgItemTextW(m_hWnd, nID, lpszString);
}

HWND LWnd::SetFocus(void)
{
    PDLASSERT(IsWindow());
    return ::SetFocus(m_hWnd);
}

void LWnd::SetFont(__in HFONT hFont, __in BOOL bRedraw /* = TRUE */)
{
    PDLASSERT(IsWindow());
    SendMessage(WM_SETFONT, (WPARAM)hFont, (LPARAM)bRedraw);
}

BOOL LWnd::SetForegroundWindow(void)
{
    PDLASSERT(IsWindow());
    return ::SetForegroundWindow(m_hWnd);
}

HICON LWnd::SetIcon(__in HICON hIcon, __in BOOL bBigIcon)
{
    PDLASSERT(IsWindow());
    return (HICON)SendMessage(WM_SETICON, bBigIcon, (LPARAM)hIcon);
}

int LWnd::SetScrollInfo(
    __in int nBar,
    __in LPCSCROLLINFO lpsi,
    __in BOOL redraw)
{
    PDLASSERT(IsWindow());
    return ::SetScrollInfo(m_hWnd, nBar, lpsi, redraw);
}

UINT_PTR LWnd::SetTimer(
     __in UINT_PTR nIDEvent,
     __in UINT uElapse,
     __in TIMERPROC lpTimerFunc /* = NULL */)
{
    PDLASSERT(IsWindow());
    return ::SetTimer(m_hWnd, nIDEvent, uElapse, lpTimerFunc);
}

LONG LWnd::SetWindowLongA(__in int nIndex, __in LONG dwNewLong)
{
#ifdef _WIN32_WCE
    return 0;
#else
    PDLASSERT(IsWindow());
    return ::SetWindowLongA(m_hWnd, nIndex, dwNewLong);
#endif // _WIN32_WCE
}

LONG LWnd::SetWindowLongW(__in int nIndex, __in LONG dwNewLong)
{
    PDLASSERT(IsWindow());
    return ::SetWindowLongW(m_hWnd, nIndex, dwNewLong);
}

BOOL LWnd::SetWindowPos(
    __in HWND hWndInsertAfter,
    __in LPCRECT lprc,
    __in UINT uFlags)
{
    PDLASSERT(IsWindow());
    PDLASSERT(NULL != lprc);
    return SetWindowPos(hWndInsertAfter, lprc->left, lprc->top,
        lprc->right - lprc->left, lprc->bottom - lprc->top, uFlags);
}

BOOL LWnd::SetWindowPos(
    __in HWND hWndInsertAfter,
    __in int X, __in int Y,
    __in int cx, __in int cy,
    __in UINT uFlags)
{
    PDLASSERT(IsWindow());
    return ::SetWindowPos(m_hWnd, hWndInsertAfter, X, Y, cx, cy, uFlags);
}

int LWnd::SetWindowRgn(__in_opt HRGN hRgn, __in BOOL bRedraw)
{
    PDLASSERT(IsWindow());
    return ::SetWindowRgn(m_hWnd, hRgn, bRedraw);
}

BOOL LWnd::SetWindowTextA(__in PCSTR lpszString)
{
    PDLASSERT(IsWindow());
#ifdef _WIN32_WCE
    LStringW str = lpszString;
    return ::SetWindowTextW(m_hWnd, str);
#else
    return ::SetWindowTextA(m_hWnd, lpszString);
#endif // _WIN32_WCE
}

BOOL LWnd::SetWindowTextW(__in PCWSTR lpszString)
{
    PDLASSERT(IsWindow());
    return ::SetWindowTextW(m_hWnd, lpszString);
}

BOOL LWnd::SizeToContent(__in BOOL bRedraw /* = TRUE */)
{
    // ��������Ӵ��ڣ��򷵻�FALSE
    if (!(WS_CHILD & GetWindowLong(GWL_STYLE)))
        return FALSE;

    // ��ȡ�����ı�
    int nLen = GetWindowTextLengthA();
    if (0 == nLen)
        return FALSE;

    LString strText;
    GetWindowText(&strText);

    RECT rc = { 0 };
    LClientDC dc(m_hWnd);
    dc.DrawText(strText, -1, &rc, DT_CALCRECT);

    UINT uFlags = SWP_NOZORDER | SWP_NOMOVE;
#ifndef _WIN32_WCE
    if (!bRedraw)
        uFlags |= SWP_NOREDRAW;
#endif // _WIN32_WCE
    return SetWindowPos(NULL, 0, 0, rc.right, rc.bottom, uFlags);
}

BOOL LWnd::ShowWindow(__in int nCmdShow)
{
    PDLASSERT(IsWindow());
    return ::ShowWindow(m_hWnd, nCmdShow);
}

BOOL LWnd::UpdateWindow(void)
{
    PDLASSERT(IsWindow());
    return ::UpdateWindow(m_hWnd);
}

///////////////////////////////////////////////////////////////////////////////
// LMsgWnd

LMsgWnd::LMsgWnd(void)
{
    m_thunk = new LThunk;
}

LMsgWnd::~LMsgWnd(void)
{
    delete m_thunk;
}

WNDPROC LMsgWnd::Attach(__in HWND hWnd, __in WNDPROC proc)
{
    PDLASSERT(::IsWindow(hWnd));

    m_thunk->Init(proc, this);
    WNDPROC pProc = (WNDPROC)&(m_thunk->GetThunk());
    WNDPROC pfnWndProc = NULL;
    if (::IsWindowUnicode(hWnd))
    {
        pfnWndProc = (WNDPROC)::SetWindowLongW(hWnd, GWL_WNDPROC,
            (LONG)(LONG_PTR)pProc);
    }
    else
    {
        pfnWndProc = (WNDPROC)::SetWindowLongA(hWnd, GWL_WNDPROC,
            (LONG)(LONG_PTR)pProc);
    }

    return pfnWndProc;
}

void LMsgWnd::HandleCommand(WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
    WORD id = LOWORD(wParam);
    WORD wCode = HIWORD(lParam);
    HWND hCtrl = (HWND)lParam;

    bHandled = FALSE;
    do
    {
        if (0 == wCode || 1 == wCode)
            break;

        LNotify* n = (LNotify*)::SendMessage(hCtrl, WM_PDL_GETNOTIFY,
            PDL_NOTIFY, 0);
        if (NULL == n)
            break;

        bHandled = TRUE;
        n->OnCmdNotify(id, wCode, hCtrl, bHandled);
    } while (FALSE);

    if (!bHandled)
        OnCommand(HIWORD(wParam), LOWORD(wParam), (HWND)lParam, bHandled);
}

LRESULT LMsgWnd::HandleNotify(WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
    int id = (int)wParam;
    LPNMHDR nmh = (LPNMHDR)lParam;
    LRESULT ret = 0;

    // �Ի���Ϣ
    bHandled = TRUE;
    if (NM_CUSTOMDRAW == nmh->code)
    {
        LCustomDraw* cd = (LCustomDraw*)::SendMessage(nmh->hwndFrom,
            WM_PDL_GETNOTIFY, PDL_NOTIFY_CUSTOMDRAW, 0);
        if (NULL != cd)
        {
            LPNMCUSTOMDRAW nmcd = (LPNMCUSTOMDRAW)nmh;
            switch (nmcd->dwDrawStage)
            {
            case CDDS_PREPAINT:
                ret = cd->OnPrePaint(id, nmcd);
                break;
            case CDDS_POSTPAINT:
                ret = cd->OnPostPaint(id, nmcd);
                break;
            case CDDS_PREERASE:
                ret = cd->OnPreErase(id, nmcd);
                break;
            case CDDS_POSTERASE:
                ret = cd->OnPostErase(id, nmcd);
                break;
            case CDDS_ITEMPREPAINT:
                ret = cd->OnItemPrePaint(id, nmcd);
                break;
            case CDDS_ITEMPOSTPAINT:
                ret = cd->OnItemPostPaint(id, nmcd);
                break;
            case CDDS_ITEMPREERASE:
                ret = cd->OnItemPreErase(id, nmcd);
                break;
            case CDDS_ITEMPOSTERASE:
                ret = cd->OnItemPostErase(id, nmcd);
                break;
            case (CDDS_ITEMPREPAINT | CDDS_SUBITEM):
                ret = cd->OnSubItemPrePaint(id, nmcd);
                break;
            default:
                bHandled = FALSE;
            }
        }
        else
        {
            bHandled = FALSE;
        }
    }
    else
    {
        bHandled = FALSE;
    }
    if (bHandled)
        return ret;

    // �ؼ��Դ���
    bHandled = TRUE;
    LNotify* n = (LNotify*)::SendMessage(nmh->hwndFrom, WM_PDL_GETNOTIFY,
        PDL_NOTIFY, 0);
    if (NULL != n)
        ret = n->OnMsgNotify(id, nmh, bHandled);
    else
        bHandled = FALSE;
    if (bHandled)
        return ret;

    // Ĭ�ϴ���
    bHandled = TRUE;
    return OnNotify(id, nmh, bHandled);
}

LRESULT LMsgWnd::HandlePDLMessage(UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    if (WM_PDL_GETOBJECTA == uMsg)
        return (LRESULT)OnGetPDLObject((PSTR)lParam, wParam);
    else if (WM_PDL_GETOBJECTW == uMsg)
        return (LRESULT)OnGetPDLObject((PWSTR)lParam, wParam);
    else if (WM_PDL_GETNOTIFY == uMsg)
        return OnGetPDLNotify(wParam);
    return 0;
}

void LMsgWnd::OnMsgProcceded(
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam,
    LRESULT lRet)
{
    // Dummy
}

void LMsgWnd::OnActivate(UINT nState, HWND hWndOther, BOOL bMinimized, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnClose(BOOL& bHandled)
{
    bHandled = FALSE;
}

int LMsgWnd::OnCreate(LPCREATESTRUCTA lpCs, BOOL& bHandled)
{
    bHandled = FALSE;
    return 0;
}

int LMsgWnd::OnCreate(LPCREATESTRUCTW lpCs, BOOL& bHandled)
{
    bHandled = FALSE;
    return 0;
}

void LMsgWnd::OnDestroy(BOOL& bHandled)
{
    bHandled = FALSE;
}

#ifndef _WIN32_WCE
void LMsgWnd::OnDropFiles(HDROP hDropInfo, BOOL& bHandled)
{
    bHandled = FALSE;
}
#endif // _WIN32_WCE

BOOL LMsgWnd::OnEraseBkgnd(HDC hdc, BOOL& bHandled)
{
    bHandled = FALSE;
    return FALSE;
}

LRESULT LMsgWnd::OnGetPDLNotify(UINT nType)
{
    return 0;
}

PVOID LMsgWnd::OnGetPDLObject(PSTR lpClassName, DWORD dwSize)
{
    return NULL;
}

PVOID LMsgWnd::OnGetPDLObject(PWSTR lpClassName, DWORD dwSize)
{
    return NULL;
}

void LMsgWnd::OnHScroll(UINT nCode, UINT nPos, HWND hScrollBar, BOOL& bHandled)
{
    bHandled = FALSE;
}

BOOL LMsgWnd::OnInitDialog(HWND hCtrlFocus, LPARAM lParam, BOOL& bHandled)
{
    bHandled = FALSE;
    return FALSE;
}

void LMsgWnd::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnLButtonDblClk(UINT uFlags, int x, int y, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnLButtonDown(UINT uFlags, int x, int y, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnLButtonUp(UINT uFlags, int x, int y, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnMouseLeave(BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnMouseMove(UINT uFlags, int x, int y, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnNcCalcSize(
    BOOL bCalcValidRects,
    LPNCCALCSIZE_PARAMS lpncsp,
    BOOL& bHandled)
{
    bHandled = FALSE;
}

LRESULT LMsgWnd::OnNcHitTest(int x, int y, BOOL& bHandled)
{
    bHandled = FALSE;
    return 0;
}

void LMsgWnd::OnPaint(BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnRButtonDblClk(UINT uFlags, int x, int y, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnRButtonDown(UINT uFlags, int x, int y, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnRButtonUp(UINT uFlags, int x, int y, BOOL& bHandled)
{
    bHandled = FALSE;
}

BOOL LMsgWnd::OnSetCursor(HWND hWnd, UINT nHitTest, UINT message, BOOL& bHandled)
{
    bHandled = FALSE;
    return FALSE;
}

void LMsgWnd::OnShowWindow(BOOL bShow, UINT nStatus, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnSize(UINT nType, int cx, int cy, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnSizing(UINT nSize, LPRECT lpRect, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnTimer(UINT_PTR nIDEvent, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnVScroll(UINT nCode, UINT nPos, HWND hScrollBar, BOOL& bHandled)
{
    bHandled = FALSE;
}

void LMsgWnd::OnCommand(
    WORD wNotifyCode,
    WORD wID,
    HWND hWndCtrl,
    BOOL& bHandled)
{
    bHandled = FALSE;
}

LRESULT LMsgWnd::OnMessage(
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam,
    BOOL& bHandled)
{
    LRESULT lRet = 0;
    switch (uMsg)
    {
    case WM_ACTIVATE:
        {
            OnActivate(LOWORD(wParam), (HWND)lParam, HIWORD(wParam), bHandled);
        }
        break;
    case WM_CLOSE:
        {
            OnClose(bHandled);
        }
        break;
    case WM_COMPAREITEM:
        {
            PCOMPAREITEMSTRUCT cis = (PCOMPAREITEMSTRUCT)lParam;

            LDrawItem* di = (LDrawItem*)::SendMessage(cis->hwndItem,
                WM_PDL_GETNOTIFY, PDL_NOTIFY_DRAWITEM, 0);
            if (NULL != di)
                lRet = di->OnCompareItem(wParam, cis);
            else
                bHandled = FALSE;
        }
        break;
    case WM_CREATE:
        {
            if (::IsWindowUnicode(GetHandle()))
                lRet = (LRESULT)OnCreate((LPCREATESTRUCTW)lParam, bHandled);
            else
                lRet = (LRESULT)OnCreate((LPCREATESTRUCTA)lParam, bHandled);
        }
        break;
    case WM_DELETEITEM:
        {
            PDELETEITEMSTRUCT dis = (PDELETEITEMSTRUCT)lParam;
            LDrawItem* di = (LDrawItem*)::SendMessage(dis->hwndItem,
                WM_PDL_GETNOTIFY, PDL_NOTIFY_DRAWITEM, 0);
            if (NULL != di)
                lRet = di->OnDeleteItem(wParam, dis);
            else
                bHandled = FALSE;
        }
        break;
    case WM_DESTROY:
        {
            OnDestroy(bHandled);
        }
        break;
    case WM_DRAWITEM:
        {
            PDRAWITEMSTRUCT dis = (PDRAWITEMSTRUCT)lParam;

            HWND h = dis->hwndItem;
            if (ODT_MENU == dis->CtlType)
                h = GetHandle();

            LDrawItem* di = (LDrawItem*)::SendMessage(dis->hwndItem,
                    WM_PDL_GETNOTIFY, PDL_NOTIFY_DRAWITEM, 0);
            if (NULL != di)
                lRet = di->OnDrawItem(wParam, dis);
            else
                bHandled = FALSE;
        }
        break;
#ifndef _WIN32_WCE
    case WM_DROPFILES:
        {
            OnDropFiles((HDROP)wParam, bHandled);
        }
        break;
#endif // _WIN32_WCE
    case WM_ERASEBKGND:
        {
            lRet = (LRESULT)OnEraseBkgnd((HDC)wParam, bHandled);
        }
        break;
    case WM_HSCROLL:
        {
            OnHScroll(LOWORD(wParam), HIWORD(wParam), (HWND)lParam, bHandled);
        }
        break;
    case WM_INITDIALOG:
        {
            lRet = (LRESULT)OnInitDialog((HWND)wParam, lParam, bHandled);
        }
        break;
    case WM_KEYDOWN:
        {
            OnKeyDown((UINT)wParam, LOWORD(lParam), HIWORD(lParam), bHandled);
        }
        break;
    case WM_LBUTTONDBLCLK:
        {
            OnLButtonDblClk((UINT)wParam, GET_X_LPARAM(lParam),
                GET_Y_LPARAM(lParam), bHandled);
        }
        break;
    case WM_LBUTTONDOWN:
        {
            OnLButtonDown((UINT)wParam, GET_X_LPARAM(lParam),
                GET_Y_LPARAM(lParam), bHandled);
        }
        break;
    case WM_LBUTTONUP:
        {
            OnLButtonUp((UINT)wParam, GET_X_LPARAM(lParam),
                GET_Y_LPARAM(lParam), bHandled);
        }
        break;
    case WM_MEASUREITEM:
        {
            PMEASUREITEMSTRUCT mis = (PMEASUREITEMSTRUCT)lParam;

            LDrawItem* di = NULL;
            if (ODT_MENU == mis->CtlType)
            {
                di = (LDrawItem*)OnGetPDLNotify(PDL_NOTIFY_DRAWITEM);
            }
            else
            {
                di = (LDrawItem*)::SendDlgItemMessage(GetHandle(), mis->CtlID,
                    WM_PDL_GETNOTIFY, PDL_NOTIFY_DRAWITEM, 0);
            }
            if (NULL != di)
                lRet = di->OnMeasureItem(wParam, mis);
            else
                bHandled = FALSE;
        }
        break;
#ifndef _WIN32_WCE
    case WM_MOUSELEAVE:
        {
            OnMouseLeave(bHandled);
        }
        break;
#endif // _WIN32_WCE
    case WM_MOUSEMOVE:
        {
            OnMouseMove((UINT)wParam, GET_X_LPARAM(lParam),
                GET_Y_LPARAM(lParam), bHandled);
        }
        break;
    case WM_NCCALCSIZE:
        {
            OnNcCalcSize((BOOL)wParam, (LPNCCALCSIZE_PARAMS)lParam, bHandled);
        }
        break;
    case WM_NCHITTEST:
        {
            OnNcHitTest(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam), bHandled);
        }
        break;
    case WM_PAINT:
        {
            OnPaint(bHandled);
        }
        break;
    case WM_RBUTTONDBLCLK:
        {
            OnRButtonDblClk((UINT)wParam, GET_X_LPARAM(lParam),
                GET_Y_LPARAM(lParam), bHandled);
        }
        break;
    case WM_RBUTTONDOWN:
        {
            OnRButtonDown((UINT)wParam, GET_X_LPARAM(lParam),
                GET_Y_LPARAM(lParam), bHandled);
        }
        break;
    case WM_RBUTTONUP:
        {
            OnRButtonUp((UINT)wParam, GET_X_LPARAM(lParam),
                GET_Y_LPARAM(lParam), bHandled);
        }
        break;
    case WM_SETCURSOR:
        {
            lRet = OnSetCursor((HWND)wParam, LOWORD(lParam), HIWORD(lParam),
                bHandled);
        }
        break;
    case WM_SHOWWINDOW:
        {
            OnShowWindow((BOOL)wParam, (UINT)lParam, bHandled);
        }
        break;
    case WM_SIZE:
        {
            OnSize((UINT)wParam, LOWORD(lParam), HIWORD(lParam), bHandled);
        }
        break;
    case WM_SIZING:
        {
            OnSizing((UINT)wParam, (LPRECT)lParam, bHandled);
            if (bHandled)
                lRet = TRUE;
        }
        break;
    case WM_TIMER:
        {
            OnTimer(wParam, bHandled);
        }
        break;
    case WM_VSCROLL:
        {
            OnVScroll(LOWORD(wParam), HIWORD(wParam), (HWND)lParam, bHandled);
        }
        break;
    default:
        bHandled = FALSE;
    }
    return lRet;
}

LRESULT LMsgWnd::OnNotify(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
    LRESULT ret = 0;
    bHandled = FALSE;
    return ret;
}

//////////////////////////////////////////////////////////////////////////
// LSubclassWnd

LSubclassWnd::LSubclassWnd(void)
{
    /* Dummy */
}

BOOL LSubclassWnd::SubclassWindow(__in HWND hWnd)
{
    PDLASSERT(::IsWindow(hWnd));

    WNDPROC pfnWndProc = Attach(hWnd, (WNDPROC)WindowProc);
    if (NULL == pfnWndProc)
        return FALSE;

    m_pfnWndProc = pfnWndProc;
    return TRUE;
}

LRESULT CALLBACK LSubclassWnd::WindowProc(
    LSubclassWnd* This,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam)
{
    BOOL bHandled = TRUE;
    LRESULT lRet = This->HandlePDLMessage(uMsg, wParam, lParam);
    if (0 != lRet)
        return lRet;

    if (WM_COMMAND == uMsg)
        This->HandleCommand(wParam, lParam, bHandled);
    else if (WM_NOTIFY == uMsg)
        lRet = This->HandleNotify(wParam, lParam, bHandled);
    else
        bHandled = FALSE;

    if (!bHandled)
    {
        bHandled = TRUE;
        lRet = This->OnMessage(uMsg, wParam, lParam, bHandled);
    }

    if (!bHandled)
    {
        lRet = ::CallWindowProc(This->m_pfnWndProc, This->GetHandle(), uMsg,
            wParam, lParam);
    }

    This->OnMsgProcceded(uMsg, wParam, lParam, lRet);
    return lRet;
}

///////////////////////////////////////////////////////////////////////////////
// LWindow

PDL_DEFINE_WINCLASS(LWindow)

LWindow::LWindow(void) : LWnd(NULL)
{
    m_atom = NULL;
    m_pfnWndProc = NULL;
}

LWindow::LWindow(__in HWND hWnd) : LWnd(hWnd)
{
    PDLASSERT(NULL != hWnd);
    m_atom = (ATOM)LWnd::GetClassLong(GCW_ATOM);
    m_pfnWndProc = LMsgWnd::Attach(hWnd, (WNDPROC)WindowProc);
}

LWindow::LWindow(__in LPWNDCLASSA wc) : LWnd(NULL)
{
    m_atom = NULL;
    Register(wc);
    m_pfnWndProc = ::DefWindowProcA;
}

LWindow::LWindow(__in LPWNDCLASSW wc) : LWnd(NULL)
{
    m_atom = NULL;
    Register(wc);
    m_pfnWndProc = ::DefWindowProcW;
}

void LWindow::Attach(__in HWND hWnd)
{
    PDLASSERT(NULL == m_hWnd);
    LWnd::Attach(hWnd);
    m_atom = (ATOM)LWnd::GetClassLong(GCW_ATOM);
    m_pfnWndProc = LMsgWnd::Attach(hWnd, (WNDPROC)WindowProc);
}

BOOL LWindow::Create(
    __in PCSTR lpClassName,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    if (NULL == lpClassName)
        lpClassName = MAKEINTATOMA(m_atom);
    if (NULL == lpClassName)
        return FALSE;

    PDLASSERT(NULL == m_hWnd);
    LAppModule* theApp = LAppModule::GetApp();
    theApp->AddWndData(this);
    if (NULL == m_pfnWndProc)
        m_pfnWndProc = ::DefWindowProcA;
    return LWnd::Create(lpClassName, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LWindow::Create(
    __in PCWSTR lpClassName,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    if (NULL == lpClassName)
        lpClassName = MAKEINTATOMW(m_atom);
    if (NULL == lpClassName)
        return FALSE;

    PDLASSERT(NULL == m_hWnd);
    LAppModule* theApp = LAppModule::GetApp();
    theApp->AddWndData(this);
    if (NULL == m_pfnWndProc)
        m_pfnWndProc = ::DefWindowProcW;
    return LWnd::Create(lpClassName, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LWindow::Create(
    __in PCSTR lpClassName,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in HMENU hMenu,
    __in PVOID lpParam)
{
    if (NULL == lpClassName)
        lpClassName = MAKEINTATOMA(m_atom);
    if (NULL == lpClassName)
        return FALSE;

    PDLASSERT(NULL == m_hWnd);
    LAppModule* theApp = LAppModule::GetApp();
    theApp->AddWndData(this);
    if (NULL == m_pfnWndProc)
        m_pfnWndProc = ::DefWindowProcA;
    return LWnd::Create(lpClassName, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, hMenu, lpParam);
}

BOOL LWindow::Create(
    __in PCWSTR lpClassName,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in HMENU hMenu,
    __in PVOID lpParam)
{
    if (NULL == lpClassName)
        lpClassName = MAKEINTATOMW(m_atom);
    if (NULL == lpClassName)
        return FALSE;

    PDLASSERT(NULL == m_hWnd);
    LAppModule* theApp = LAppModule::GetApp();
    theApp->AddWndData(this);
    if (NULL == m_pfnWndProc)
        m_pfnWndProc = ::DefWindowProcW;
    return LWnd::Create(lpClassName, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, hMenu, lpParam);
}

BOOL LWindow::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpClassName,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    if (NULL == lpClassName)
        lpClassName = MAKEINTATOMA(m_atom);
    if (NULL == lpClassName)
        return FALSE;

    PDLASSERT(NULL == m_hWnd);
    LAppModule* theApp = LAppModule::GetApp();
    theApp->AddWndData(this);
    if (NULL == m_pfnWndProc)
        m_pfnWndProc = ::DefWindowProcA;
    return LWnd::CreateEx(dwExStyle, lpClassName, lpWindowName,
        dwStyle, lpRect, hWndParent, nID, lpParam);
}

BOOL LWindow::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpClassName,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    if (NULL == lpClassName)
        lpClassName = MAKEINTATOMW(m_atom);
    if (NULL == lpClassName)
        return FALSE;

    PDLASSERT(NULL == m_hWnd);
    LAppModule* theApp = LAppModule::GetApp();
    theApp->AddWndData(this);
    if (NULL == m_pfnWndProc)
        m_pfnWndProc = ::DefWindowProcW;
    return LWnd::CreateEx(dwExStyle, lpClassName, lpWindowName,
        dwStyle, lpRect, hWndParent, nID, lpParam);
}

BOOL LWindow::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpClassName,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in HMENU hMenu,
    __in PVOID lpParam)
{
    if (NULL == lpClassName)
        lpClassName = MAKEINTATOMA(m_atom);
    if (NULL == lpClassName)
        return FALSE;

    PDLASSERT(NULL == m_hWnd);
    LAppModule* theApp = LAppModule::GetApp();
    theApp->AddWndData(this);
    if (NULL == m_pfnWndProc)
        m_pfnWndProc = ::DefWindowProcA;
    return LWnd::CreateEx(dwExStyle, lpClassName, lpWindowName,
        dwStyle, X, Y, nWidth, nHeight, hWndParent, hMenu, lpParam);
}

BOOL LWindow::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpClassName,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in HMENU hMenu,
    __in PVOID lpParam)
{
    if (NULL == lpClassName)
        lpClassName = MAKEINTATOMW(m_atom);
    if (NULL == lpClassName)
        return FALSE;

    PDLASSERT(NULL == m_hWnd);
    LAppModule* theApp = LAppModule::GetApp();
    theApp->AddWndData(this);
    if (NULL == m_pfnWndProc)
        m_pfnWndProc = ::DefWindowProcW;
    return LWnd::CreateEx(dwExStyle, lpClassName, lpWindowName,
        dwStyle, X, Y, nWidth, nHeight, hWndParent, hMenu, lpParam);
}

BOOL LWindow::Register(__in LPWNDCLASSA wc)
{
    if (NULL != m_atom)
        return FALSE;

    wc->lpfnWndProc = StartWndProc;
    m_atom = ::RegisterClassA(wc);
    m_pfnWndProc = ::DefWindowProcA;
    return NULL != m_atom;
}

BOOL LWindow::Register(__in LPWNDCLASSW wc)
{
    if (NULL != m_atom)
        return FALSE;

    wc->lpfnWndProc = StartWndProc;
    m_atom = ::RegisterClassW(wc);
    m_pfnWndProc = ::DefWindowProcW;
    return NULL != m_atom;
}

LRESULT CALLBACK LWindow::StartWndProc(
    HWND hWnd,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam)
{
    LAppModule *theApp = LAppModule::GetApp();

    LWindow* pThis = (LWindow*)theApp->ExtractWndData();
    pThis->m_hWnd = hWnd;
    pThis->m_thunk->Init(WindowProc, pThis);

    WNDPROC pProc = (WNDPROC)&(pThis->m_thunk->GetThunk());
    if (pThis->IsWindowUnicode())
    {
        pThis->SetWindowLongW(GWL_WNDPROC, (LONG)(LONG_PTR)pProc);
        pThis->m_pfnWndProc = ::DefWindowProcW;
    }
    else
    {
        pThis->SetWindowLongA(GWL_WNDPROC, (LONG)(LONG_PTR)pProc);
        pThis->m_pfnWndProc = ::DefWindowProcA;
    }

    return pProc(hWnd, uMsg, wParam, lParam);
}

LRESULT CALLBACK LWindow::WindowProc(
    LWindow* This,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam)
{
    BOOL bHandled = TRUE;
    LRESULT lRet = This->HandlePDLMessage(uMsg, wParam, lParam);
    if (0 != lRet)
        return lRet;

    if (WM_COMMAND == uMsg)
        This->HandleCommand(wParam, lParam, bHandled);
    else if (WM_NOTIFY == uMsg)
        lRet = This->HandleNotify(wParam, lParam, bHandled);
    else
        bHandled = FALSE;

    if (!bHandled)
    {
        bHandled = TRUE;
        lRet = This->OnMessage(uMsg, wParam, lParam, bHandled);
    }

    if (!bHandled)
    {
        if (This->IsWindowUnicode())
        {
            lRet = ::CallWindowProcW(This->m_pfnWndProc, This->m_hWnd, uMsg,
                wParam, lParam);
        }
        else
        {
            lRet = ::CallWindowProcA(This->m_pfnWndProc, This->m_hWnd, uMsg,
                wParam, lParam);
        }
    }

    This->OnMsgProcceded(uMsg, wParam, lParam, lRet);
    return lRet;
}

//////////////////////////////////////////////////////////////////////////
// LDialog

PDL_DEFINE_WINCLASS(LDialog)

LDialog::LDialog(__in UINT uIDDialog, LIniParser* lang)
    : m_uId(uIDDialog)
    , m_lang(lang)
{
    // Dummy
}

BOOL LDialog::Create(
    __in HWND hParent /* = ::GetActiveWindow() */,
    __in LPARAM lParam /* = 0 */)
{
    LAppModule *theApp = LAppModule::GetApp();
    theApp->AddWndData(this);
    return NULL != ::CreateDialogParam(theApp->GetInstance(),
        MAKEINTRESOURCE(m_uId), hParent, StartDlgProc, lParam);
}


int LDialog::DoModal(
    __in HWND hParent /* = ::GetActiveWindow() */,
    __in LPARAM lParam /* = 0 */)
{
    LAppModule *theApp = LAppModule::GetApp();
    theApp->AddWndData(this);
    return (int)::DialogBoxParam(theApp->GetInstance(),
        MAKEINTRESOURCE(m_uId), hParent, StartDlgProc, lParam);
}

INT_PTR LDialog::EndDialog(__in INT_PTR nResult)
{
    return ::EndDialog(m_hWnd, nResult);
}

BOOL LDialog::LoadLanguageRes(void)
{
    if (NULL == m_lang)
        return FALSE;

    char section[16];
    wsprintfA(section, "%d", m_uId);

    LString text;
    char key[16];
    LWnd ctrl = GetWindow(GW_CHILD);
    UINT id;
    while (NULL != ctrl.GetSafeHWND())
    {
        id = ctrl.GetDlgCtrlID();
        if (id > 0)
        {
            wsprintfA(key, "%d", id);
            if (m_lang->GetString(section, key, _T(""), &text) > 0)
            {
                ctrl.SetWindowText(text);
                ctrl.SizeToContent(FALSE);
            }
        }
        ctrl = ctrl.GetWindow(GW_HWNDNEXT);
    }
    return TRUE;
}

void LDialog::SetFocusCtrl(__in HWND hCtrl)
{
    SendMessage(WM_NEXTDLGCTL, (WPARAM)hCtrl, TRUE);
}

void LDialog::SetFont(
    __in HFONT hFont,
    __in BOOL bAllCtrls /* = TRUE */,
    __in BOOL bRedraw /* = TRUE */)
{
    LWnd::SetFont(hFont, bRedraw);

    LWnd ctrl = GetWindow(GW_CHILD);
    while (NULL != ctrl.GetSafeHWND())
    {
        ctrl.SetFont(hFont, bRedraw);
        ctrl = ctrl.GetWindow(GW_HWNDNEXT);
    }
}

INT_PTR CALLBACK LDialog::StartDlgProc(
    HWND hDlg,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam)
{
    LAppModule *theApp = LAppModule::GetApp();

    LDialog* pThis = (LDialog*)theApp->ExtractWndData();
    pThis->m_hWnd = hDlg;
    pThis->m_thunk->Init(DialogProc, pThis);

    DLGPROC pProc = (DLGPROC)&(pThis->m_thunk->GetThunk());
    pThis->SetWindowLongW(DWL_DLGPROC, (LONG)(LONG_PTR)pProc);

    return pProc(hDlg, uMsg, wParam, lParam);
}

INT_PTR CALLBACK LDialog::DialogProc(
    LDialog* This,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam)
{
    BOOL bHandled = TRUE;
    LRESULT lRet = This->HandlePDLMessage(uMsg, wParam, lParam);
    if (0 != lRet)
        return lRet;

    if (WM_COMMAND == uMsg)
        This->HandleCommand(wParam, lParam, bHandled);
    else if (WM_NOTIFY == uMsg)
        lRet = This->HandleNotify(wParam, lParam, bHandled);
    else
        bHandled = FALSE;

    if (WM_INITDIALOG == uMsg)
        This->LoadLanguageRes();

    if (!bHandled)
    {
        bHandled = TRUE;
        lRet = This->OnMessage(uMsg, wParam, lParam, bHandled);
    }

    This->OnMsgProcceded(uMsg, wParam, lParam, lRet);
    return bHandled ? lRet : 0;
}

///////////////////////////////////////////////////////////////////////////////
// LDrawItem

int LDrawItem::OnCompareItem(UINT idCtl, PCOMPAREITEMSTRUCT cis)
{
    return 0;
}

BOOL LDrawItem::OnDeleteItem(UINT idCtl, PDELETEITEMSTRUCT dis)
{
    return FALSE;
}

BOOL LDrawItem::OnDrawItem(UINT idCtl, PDRAWITEMSTRUCT dis)
{
    return FALSE;
}

BOOL LDrawItem::OnMeasureItem(UINT idCtl, PMEASUREITEMSTRUCT mis)
{
    return FALSE;
}

///////////////////////////////////////////////////////////////////////////////
// LCustomDraw

DWORD LCustomDraw::OnPrePaint(int idCtl, LPNMCUSTOMDRAW cd)
{
    return CDRF_DODEFAULT;
}

DWORD LCustomDraw::OnPostPaint(int idCtl, LPNMCUSTOMDRAW cd)
{
    return CDRF_DODEFAULT;
}

DWORD LCustomDraw::OnPreErase(int idCtl, LPNMCUSTOMDRAW cd)
{
    return CDRF_DODEFAULT;
}

DWORD LCustomDraw::OnPostErase(int idCtl, LPNMCUSTOMDRAW cd)
{
    return CDRF_DODEFAULT;
}

DWORD LCustomDraw::OnItemPrePaint(int idCtl, LPNMCUSTOMDRAW cd)
{
    return CDRF_DODEFAULT;
}

DWORD LCustomDraw::OnItemPostPaint(int idCtl, LPNMCUSTOMDRAW cd)
{
    return CDRF_DODEFAULT;
}

DWORD LCustomDraw::OnItemPreErase(int idCtl, LPNMCUSTOMDRAW cd)
{
    return CDRF_DODEFAULT;
}

DWORD LCustomDraw::OnItemPostErase(int idCtl, LPNMCUSTOMDRAW cd)
{
    return CDRF_DODEFAULT;
}

DWORD LCustomDraw::OnSubItemPrePaint(int idCtl, LPNMCUSTOMDRAW cd)
{
    return CDRF_DODEFAULT;
}

void LNotify::OnCmdNotify(WORD id, WORD wCode, HWND hCtrl, BOOL& bHandled)
{
    bHandled = FALSE;
}

LRESULT LNotify::OnMsgNotify(int id, LPNMHDR nmh, BOOL& bHandled)
{
    bHandled = FALSE;
    return 0;
}