/**
 * \file pdl_ctrl.h
 * \brief PDL 基本控件类
 * \details 这个文件中包括了 PDL 基本控件的类定义，如下：
 *   \li \c LButton PDL 按钮控件类
 *   \li \c LComboBox PDL 组合框控件类
 *   \li \c LEdit PDL 编辑框控件类
 *   \li \c LListBox PDL 列表框控件类
 *   \li \c LStatic PDL 静态文本控件类
 */

#pragma once

#include "pdl_base.h"
#include "pdl_window.h"

/**
 * \class LButton
 * \brief PDL 按钮控件类
 */

class LButton : public LWnd
{
public:
    LButton(__in HWND hWnd = NULL);
    LButton& operator=(__in HWND hWnd);
    operator HWND(void);
public:
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID);
};

/**
 * \class LComboBox
 * \brief PDL 组合框控件类
 */

#ifdef UNICODE
#define GetLBTextLen    GetLBTextLenW
#else
#define GetLBTextLen    GetLBTextLenA
#endif // UNICODE

class LComboBox : public LWnd
{
public:
    LComboBox(__in HWND hWnd = NULL);
    LComboBox& operator=(__in HWND hWnd);
    operator HWND(void);
public:
    int AddString(__in PCSTR lpszString);
    int AddString(__in PCWSTR lpszString);
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID);
    int FindString(__in int nStartAfter, __in PCSTR lpszString);
    int FindString(__in int nStartAfter, __in PCWSTR lpszString);
    int GetCount(void);
    int GetCurSel(void);

    /**
     * 获取 ComboBox 之中的 Edit 控件句柄。
     * @return 如果 ComboBox 之中含有 Edit 控件则返回其句柄，否则返回 NULL。
     */
    HWND GetEdit(void);

    DWORD_PTR GetItemData(__in int nIndex);
    int GetLBText(__in int nIndex, __out PSTR lpszText);
    int GetLBText(__in int nIndex, __out PWSTR lpszText);
    int GetLBText(__in int nIndex, __out LStringA *pStr);
    int GetLBText(__in int nIndex, __out LStringW *pStr);
    int GetLBTextLen(__in int nIndex);
#ifdef UNICODE
    int GetLBTextLenA(__in int nIndex);
#else
    int GetLBTextLenW(__in int nIndex);
#endif // UNICODE

    /**
     * 获取 ComboBox 之中的 List 控件句柄。
     * @return 如果 ComboBox 之中含有 List 控件则返回其句柄，否则返回 NULL。
     */
    HWND GetListBox(void);

    void ResetContent(void);
    int SetCurSel(__in int nSelect);
    int SetItemData(__in int nIndex, __in DWORD_PTR dwItemData);
};

/**
 * \class LEdit
 * \brief PDL 编辑框控件类
 */

#ifdef UNICODE
#define GetSel      GetSelW
#define LimitText   LimitTextW
#define SetSel      SetSelW
#else
#define GetSel      GetSelA
#define LimitText   LimitTextA
#define SetSel      SetSelA
#endif // UNICODE

class LEdit : public LWnd
{
public:
    LEdit(__in HWND hWnd = NULL);
    LEdit& operator=(__in HWND hWnd);
    operator HWND(void);
public:
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    DWORD GetSel(__out PDWORD lpdwStart, __out PDWORD lpdwEnd);
#ifdef UNICODE
    DWORD GetSelA(__out PDWORD lpdwStart, __out PDWORD lpdwEnd);
#else
    DWORD GetSelW(__out PDWORD lpdwStart, __out PDWORD lpdwEnd);
#endif // UNICODE
    void LimitText(__in int nMaxChars);
#ifdef UNICODE
    void LimitTextA(__in int nMaxChars);
#else
    void LimitTextW(__in int nMaxChars);
#endif // UNICODE
    void ReplaceSel(__in PCSTR lpszNewText,
        __in BOOL bCanUndo = FALSE);
    void ReplaceSel(__in PCWSTR lpszNewText,
        __in BOOL bCanUndo = FALSE);
    void SetSel(__in int nStartChar, __in int nEndChar);
#ifdef UNICODE
    void SetSelA(__in int nStartChar, __in int nEndChar);
#else
    void SetSelW(__in int nStartChar, __in int nEndChar);
#endif // UNICODE
};

/**
 * \class LListBox
 * \brief PDL 列表框控件类
 */

#ifdef UNICODE
#define GetTextLen  GetTextLenW
#else
#define GetTextLen  GetTextLenA
#endif // UNICODE

class LListBox : public LWnd
{
public:
    LListBox(__in HWND hWnd = NULL);
    LListBox& operator=(__in HWND hWnd);
public:
    int AddString(__in PCSTR lpszString);
    int AddString(__in PCWSTR lpszString);
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID);
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in int x, __in int y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in UINT nID);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in int x, __in int y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in int X, __in int Y, __in int nWidth,
        __in int nHeight, __in HWND hWndParent, __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in int X, __in int Y, __in int nWidth,
        __in int nHeight, __in HWND hWndParent, __in UINT nID);
    int DeleteString(__in int nIndex);
    int FindString(__in int nStartAfter, __in PCSTR lpszItem);
    int FindString(__in int nStartAfter, __in PCWSTR lpszItem);
    int GetCount(void);
    int GetCurSel(void);
    DWORD_PTR GetItemData(__in int nIndex);
    int GetItemHeight(int nIndex);
    int GetText(__in int nIndex, __out PSTR lpszBuffer);
    int GetText(__in int nIndex, __out PWSTR lpszBuffer);
    int GetText(__in int nIndex, __out LStringA* str);
    int GetText(__in int nIndex, __out LStringW* str);
    int GetTextLen(__in int nIndex);
#ifdef UNICODE
    int GetTextLenA(__in int nIndex);
#else
    int GetTextLenW(__in int nIndex);
#endif // UNICODE
    int GetTopIndex(void);
    int InsertString(__in int nIndex, __in PCSTR lpszString);
    int InsertString(__in int nIndex, __in PCWSTR lpszString);
    int ItemFromPoint(__in int x, __in int y);
    void ResetContent(void);
    int SetCurSel(__in int nSelect);
    int SetItemData(__in int nIndex, __in DWORD_PTR dwItemData);
    int SetTopIndex(__in int nIndex);
};

/**
 * \class LStatic
 * \brief PDL 静态文本控件类
 */

class LStatic : public LWnd
{
public:
    LStatic(__in HWND hWnd = NULL);
    LStatic& operator=(__in HWND hWnd);
public:
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID);
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in int x, __in int y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in UINT nID);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in int x, __in int y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in int X, __in int Y, __in int nWidth,
        __in int nHeight, __in HWND hWndParent, __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in int X, __in int Y, __in int nWidth,
        __in int nHeight, __in HWND hWndParent, __in UINT nID);
    HICON SetIcon(__in HICON hIcon);
};
