#include "..\..\include\pdl_ctrl.h"
#include "..\..\include\pdl_gdi.h"

///////////////////////////////////////////////////////////////////////////////
// LComboBox

LComboBox::LComboBox(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
}

LComboBox& LComboBox::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

LComboBox::operator HWND(void)
{
    return m_hWnd;
}

BOOL LComboBox::Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
                       __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
                       __in PVOID lpParam)
{
    return LWnd::Create("ComboBox", lpWindowName, dwStyle | WS_CHILD,
        lpRect, hWndParent, nID, lpParam);
}

BOOL LComboBox::Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
                       __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
                       __in PVOID lpParam)
{
    return LWnd::Create(L"ComboBox", lpWindowName, dwStyle | WS_CHILD,
        lpRect, hWndParent, nID, lpParam);
}

int LComboBox::AddString(__in PCSTR lpszString)
{
    return (int)SendMessageA(CB_ADDSTRING, 0, (LPARAM)lpszString);
}

int LComboBox::AddString(__in PCWSTR lpszString)
{
    return (int)SendMessageW(CB_ADDSTRING, 0, (LPARAM)lpszString);
}

int LComboBox::FindString(__in int nStartAfter, __in PCSTR lpszString)
{
    return (int)SendMessageA(CB_FINDSTRING, nStartAfter, (LPARAM)lpszString);
}

int LComboBox::FindString(__in int nStartAfter, __in PCWSTR lpszString)
{
    return (int)SendMessageW(CB_FINDSTRING, nStartAfter, (LPARAM)lpszString);
}

int LComboBox::GetCount(void)
{
    return (int)SendMessage(CB_GETCOUNT);
}

int LComboBox::GetCurSel(void)
{
    return (int)SendMessage(CB_GETCURSEL);
}

HWND LComboBox::GetEdit(void)
{
#ifdef _WIN32_WCE
    return NULL;
#else
    return ::FindWindowEx(m_hWnd, NULL, _T("Edit"), NULL);
#endif // _WIN32_WCE
}

DWORD_PTR LComboBox::GetItemData(__in int nIndex)
{
    return SendMessage(CB_GETITEMDATA, nIndex);
}

int LComboBox::GetLBText(__in int nIndex, __out PSTR lpszText)
{
    return (int)SendMessageA(CB_GETLBTEXT, nIndex, (LPARAM)lpszText);
}

int LComboBox::GetLBText(__in int nIndex, __out PWSTR lpszText)
{
    return (int)SendMessageW(CB_GETLBTEXT, nIndex, (LPARAM)lpszText);
}

int LComboBox::GetLBText(__in int nIndex, __out LStringA *pStr)
{
    int len = GetLBTextLenA(nIndex);
    if (CB_ERR == len)
        return CB_ERR;

    PSTR buf = pStr->AllocBuffer(len, FALSE);
    GetLBText(nIndex, buf);
    return len;
}

int LComboBox::GetLBText(__in int nIndex, __out LStringW *pStr)
{
    int len = GetLBTextLenW(nIndex);
    if (CB_ERR == len)
        return CB_ERR;

    PWSTR buf = pStr->AllocBuffer(len, FALSE);
    GetLBText(nIndex, buf);
    return len;
}

int LComboBox::GetLBTextLenA(__in int nIndex)
{
    return (int)SendMessageA(CB_GETLBTEXTLEN, nIndex);
}

int LComboBox::GetLBTextLenW(__in int nIndex)
{
    return (int)SendMessageW(CB_GETLBTEXTLEN, nIndex);
}

HWND LComboBox::GetListBox(void)
{
#ifdef _WIN32_WCE
    return NULL;
#else
    return ::FindWindowEx(m_hWnd, NULL, _T("ComboLBox"), NULL);
#endif // _WIN32_WCE
}

void LComboBox::ResetContent(void)
{
    SendMessage(CB_RESETCONTENT);
}

int LComboBox::SetCurSel(__in int nSelect)
{
    return (int)SendMessage(CB_SETCURSEL, nSelect);
}

int LComboBox::SetItemData(__in int nIndex, __in DWORD_PTR dwItemData)
{
    return (int)SendMessage(CB_SETITEMDATA, nIndex, dwItemData);
}

///////////////////////////////////////////////////////////////////////////////
// LEdit

LEdit::LEdit(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
}

LEdit& LEdit::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

LEdit::operator HWND(void)
{
    return m_hWnd;
}

BOOL LEdit::Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
                   __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
                   __in PVOID lpParam)
{
    return LWnd::Create("Edit", lpWindowName, dwStyle | WS_CHILD, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LEdit::Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
                   __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
                   __in PVOID lpParam)
{
    return LWnd::Create(L"Edit", lpWindowName, dwStyle | WS_CHILD, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LEdit::CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
                     __in DWORD dwStyle, __in LPCRECT lpRect,
                     __in HWND hWndParent, __in UINT nID, __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, "Edit", lpWindowName, dwStyle | WS_CHILD,
        lpRect, hWndParent, nID, lpParam);
}

BOOL LEdit::CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
                     __in DWORD dwStyle, __in LPCRECT lpRect,
                     __in HWND hWndParent, __in UINT nID, __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, L"Edit", lpWindowName, dwStyle | WS_CHILD,
        lpRect, hWndParent, nID, lpParam);
}

void LEdit::LimitTextA(__in int nMaxChars)
{
    SendMessageA(EM_LIMITTEXT, nMaxChars);
}

void LEdit::LimitTextW(__in int nMaxChars)
{
    SendMessageW(EM_LIMITTEXT, nMaxChars);
}

void LEdit::ReplaceSel(
    __in PCSTR lpszNewText,
    __in BOOL bCanUndo /* = FALSE */)
{
    SendMessageA(EM_REPLACESEL, bCanUndo, (LPARAM)lpszNewText);
}

void LEdit::ReplaceSel(
    __in PCWSTR lpszNewText,
    __in BOOL bCanUndo /* = FALSE */)
{
    SendMessageW(EM_REPLACESEL, bCanUndo, (LPARAM)lpszNewText);
}

void LEdit::SetSelA(__in int nStartChar, __in int nEndChar)
{
    SendMessageA(EM_SETSEL, nStartChar, nEndChar);
}

void LEdit::SetSelW(__in int nStartChar, __in int nEndChar)
{
    SendMessageW(EM_SETSEL, nStartChar, nEndChar);
}

//////////////////////////////////////////////////////////////////////////
// LListBox

LListBox& LListBox::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

int LListBox::AddString(__in PCSTR lpszString)
{
    return (int)SendMessageA(LB_ADDSTRING, 0, (LPARAM)lpszString);
}

int LListBox::AddString(__in PCWSTR lpszString)
{
    return (int)SendMessageW(LB_ADDSTRING, 0, (LPARAM)lpszString);
}

int LListBox::DeleteString(__in int nIndex)
{
    return (int)SendMessage(LB_DELETESTRING, (WPARAM)nIndex);
}

int LListBox::FindString(__in int nStartAfter, __in PCSTR lpszItem)
{
    return static_cast<int>(
        SendMessageA(LB_FINDSTRING, nStartAfter,
        reinterpret_cast<LPARAM>(lpszItem)));
}

int LListBox::FindString(__in int nStartAfter, __in PCWSTR lpszItem)
{
    return static_cast<int>(
        SendMessageW(LB_FINDSTRING, nStartAfter,
        reinterpret_cast<LPARAM>(lpszItem)));
}

int LListBox::GetCount(void)
{
    return (int)SendMessage(LB_GETCOUNT);
}

int LListBox::GetCurSel(void)
{
    return (int)SendMessage(LB_GETCURSEL);
}

DWORD_PTR LListBox::GetItemData(__in int nIndex)
{
    return (DWORD_PTR)SendMessage(LB_GETITEMDATA, (WPARAM)nIndex);
}

int LListBox::GetText(__in int nIndex, __in PSTR lpszBuffer)
{
    PDLASSERT(IsWindow());
    return (int)SendMessageA(LB_GETTEXT, (WPARAM)nIndex, (LPARAM)lpszBuffer);
}

int LListBox::GetText(__in int nIndex, __in PWSTR lpszBuffer)
{
    PDLASSERT(IsWindow());
    return (int)SendMessageW(LB_GETTEXT, (WPARAM)nIndex, (LPARAM)lpszBuffer);
}

void LListBox::ResetContent(void)
{
    SendMessage(LB_RESETCONTENT);
}

int LListBox::SetCurSel(__in int nSelect)
{
    return (int)SendMessage(LB_SETCURSEL, (WPARAM)nSelect);
}

int LListBox::SetItemData(__in int nIndex, __in DWORD_PTR dwItemData)
{
    return (int)SendMessage(LB_SETITEMDATA, (WPARAM)nIndex,
        (LPARAM)dwItemData);
}

//////////////////////////////////////////////////////////////////////////
// LStatic

LStatic::LStatic(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
}

LStatic& LStatic::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

BOOL LStatic::Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
                     __in LPCRECT lpRect, __in HWND hWndParent,
                     __in UINT nID, __in PVOID lpParam)
{
    return LWnd::Create("Static", lpWindowName, dwStyle, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LStatic::Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
                     __in LPCRECT lpRect, __in HWND hWndParent,
                     __in UINT nID, __in PVOID lpParam)
{
    return LWnd::Create(L"Static", lpWindowName, dwStyle, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LStatic::Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
                     __in int x, __in int y, __in int nWidth,
                     __in int nHeight, __in HWND hWndParent,
                     __in HMENU hMenu, __in PVOID lpParam)
{
    return LWnd::Create("Static", lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, hMenu, lpParam);
}

BOOL LStatic::Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
                     __in int x, __in int y, __in int nWidth,
                     __in int nHeight, __in HWND hWndParent,
                     __in HMENU hMenu, __in PVOID lpParam)
{
    return LWnd::Create(L"Static", lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, hMenu, lpParam);
}

BOOL LStatic::CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
                       __in DWORD dwStyle, __in LPCRECT lpRect,
                       __in HWND hWndParent, __in UINT nID,
                       __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, "Static", lpWindowName, dwStyle,
        lpRect, hWndParent, nID, lpParam);
}

BOOL LStatic::CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
                       __in DWORD dwStyle, __in LPCRECT lpRect,
                       __in HWND hWndParent, __in UINT nID,
                       __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, L"Static", lpWindowName, dwStyle,
        lpRect, hWndParent, nID, lpParam);
}

BOOL LStatic::CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
                       __in DWORD dwStyle, __in int X, __in int Y,
                       __in int nWidth, __in int nHeight, __in HWND hWndParent,
                       __in HMENU hMenu, __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, "Static", lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, hMenu, lpParam);
}

BOOL LStatic::CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
                       __in DWORD dwStyle, __in int X, __in int Y,
                       __in int nWidth, __in int nHeight, __in HWND hWndParent,
                       __in HMENU hMenu, __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, L"Static", lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, hMenu, lpParam);
}

HICON LStatic::SetIcon(__in HICON hIcon)
{
#ifdef _WIN32_WCE
    return reinterpret_cast<HICON>(
        SendMessage(STM_SETIMAGE, IMAGE_ICON,
        reinterpret_cast<LPARAM>(hIcon)));
#else
    return reinterpret_cast<HICON>(
        SendMessage(STM_SETICON, reinterpret_cast<WPARAM>(hIcon)));
#endif // _WIN32_WCE
}
