#include "..\..\include\pdl_ctrl.h"
#include "..\..\include\pdl_gdi.h"

///////////////////////////////////////////////////////////////////////////////
// LButton

LButton::LButton(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
}

LButton& LButton::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

LButton::operator HWND(void)
{
    return m_hWnd;
}

BOOL LButton::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_BUTTONA, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, NULL);
}

BOOL LButton::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_BUTTONW, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, NULL);
}

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

int LComboBox::AddString(__in PCSTR lpszString)
{
    return (int)SendMessageA(CB_ADDSTRING, 0, (LPARAM)lpszString);
}

int LComboBox::AddString(__in PCWSTR lpszString)
{
    return (int)SendMessageW(CB_ADDSTRING, 0, (LPARAM)lpszString);
}

BOOL LComboBox::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_COMBOBOXA, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, NULL);
}

BOOL LComboBox::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_COMBOBOXW, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, NULL);
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

BOOL LEdit::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_EDITA, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, NULL);
}

BOOL LEdit::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_EDITW, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, NULL);
}

BOOL LEdit::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::CreateEx(dwExStyle, WC_EDITA, lpWindowName,
        dwStyle, lpRect, hWndParent, nID, NULL);
}

BOOL LEdit::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::CreateEx(dwExStyle, WC_EDITW, lpWindowName,
        dwStyle, lpRect, hWndParent, nID, NULL);
}

DWORD LEdit::GetSelA(__out PDWORD lpdwStart, __out PDWORD lpdwEnd)
{
    return SendMessageA(EM_GETSEL, (WPARAM)lpdwStart, (LPARAM)lpdwEnd);
}

DWORD LEdit::GetSelW(__out PDWORD lpdwStart, __out PDWORD lpdwEnd)
{
    return SendMessageW(EM_GETSEL, (WPARAM)lpdwStart, (LPARAM)lpdwEnd);
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

LListBox::LListBox(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
    // Nothing
}

LListBox& LListBox::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

BOOL LListBox::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_LISTBOXA, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, NULL);
}

BOOL LListBox::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_LISTBOXW, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, NULL);
}

BOOL LListBox::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_LISTBOXA, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, (HMENU)nID, NULL);
}

BOOL LListBox::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_LISTBOXW, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, (HMENU)nID, NULL);
}

BOOL LListBox::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::CreateEx(dwExStyle, WC_LISTBOXA, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, NULL);
}

BOOL LListBox::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::CreateEx(dwExStyle, WC_LISTBOXW, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, NULL);
}

BOOL LListBox::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::CreateEx(dwExStyle, WC_LISTBOXA, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, (HMENU)nID, NULL);
}

BOOL LListBox::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::CreateEx(dwExStyle, WC_LISTBOXW, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, (HMENU)nID, NULL);
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

int LListBox::GetItemHeight(int nIndex)
{
    return (int)SendMessage(LB_GETITEMHEIGHT, nIndex);
}

int LListBox::GetText(__in int nIndex, __out PSTR lpszBuffer)
{
    return (int)SendMessageA(LB_GETTEXT, (WPARAM)nIndex, (LPARAM)lpszBuffer);
}

int LListBox::GetText(__in int nIndex, __out PWSTR lpszBuffer)
{
    return (int)SendMessageW(LB_GETTEXT, (WPARAM)nIndex, (LPARAM)lpszBuffer);
}

int LListBox::GetText(__in int nIndex, __out LStringA* str)
{
    int len = GetTextLenA(nIndex);
    if (LB_ERR == len)
        return LB_ERR;

    PSTR buf = str->AllocBuffer(len, FALSE);
    return GetText(nIndex, buf);
}

int LListBox::GetText(__in int nIndex, __out LStringW* str)
{
    int len = GetTextLenW(nIndex);
    if (LB_ERR == len)
        return LB_ERR;

    PWSTR buf = str->AllocBuffer(len, FALSE);
    return GetText(nIndex, buf);
}

int LListBox::GetTextLenA(__in int nIndex)
{
    return (int)SendMessageA(LB_GETTEXTLEN, nIndex);
}

int LListBox::GetTextLenW(__in int nIndex)
{
    return (int)SendMessageW(LB_GETTEXTLEN, nIndex);
}

int LListBox::GetTopIndex(void)
{
    return (int)SendMessage(LB_GETTOPINDEX);
}

int LListBox::InsertString(__in int nIndex, __in PCSTR lpszString)
{
    return (int)SendMessageA(LB_INSERTSTRING, (WPARAM)nIndex,
        (LPARAM)lpszString);
}

int LListBox::InsertString(__in int nIndex, __in PCWSTR lpszString)
{
    return (int)SendMessageW(LB_INSERTSTRING, (WPARAM)nIndex,
        (LPARAM)lpszString);
}

int LListBox::ItemFromPoint(__in int x, __in int y)
{
    return (int)SendMessage(LB_ITEMFROMPOINT, 0, MAKELPARAM(x, y));
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

int LListBox::SetTopIndex(__in int nIndex)
{
    return (int)SendMessage(LB_SETTOPINDEX, (WPARAM)nIndex);
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

BOOL LStatic::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_STATICA, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, NULL);
}

BOOL LStatic::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_STATICW, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, NULL);
}

BOOL LStatic::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_STATICA, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, (HMENU)nID, NULL);
}

BOOL LStatic::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::Create(WC_STATICW, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, (HMENU)nID, NULL);
}

BOOL LStatic::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::CreateEx(dwExStyle, WC_STATICA, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, NULL);
}

BOOL LStatic::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::CreateEx(dwExStyle, WC_STATICW, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, NULL);
}

BOOL LStatic::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::CreateEx(dwExStyle, WC_STATICA, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, (HMENU)nID, NULL);
}

BOOL LStatic::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID)
{
    return LWnd::CreateEx(dwExStyle, WC_STATICW, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, (HMENU)nID, NULL);
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
