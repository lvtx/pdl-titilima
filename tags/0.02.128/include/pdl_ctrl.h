/**
 * \file pdl_ctrl.h
 * \brief PDL 基本控件类
 * \details 这个文件中包括了 PDL 基本控件的类定义，如下：
 *   \li \c LComboBox PDL 组合框控件类
 *   \li \c LEdit PDL 编辑框控件类
 *   \li \c LListBox PDL 列表框控件类
 *   \li \c LStatic PDL 静态文本控件类
 */

#pragma once

#include <pdl_base.h>
#include <pdl_window.h>

/**
 * \class LComboBox
 * \brief PDL 组合框控件类
 */

class LComboBox : public LWnd
{
public:
    LComboBox(__in HWND hWnd = NULL);
    LComboBox& operator=(__in HWND hWnd);
    operator HWND(void);
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
        __in_opt PVOID lpParam);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
        __in_opt PVOID lpParam);
public:
    int AddString(__in PCSTR lpszString);
    int AddString(__in PCWSTR lpszString);
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
    int GetLBTextLenA(__in int nIndex);
    int GetLBTextLenW(__in int nIndex);

    /**
     * 获取 ComboBox 之中的 List 控件句柄。
     * @return 如果 ComboBox 之中含有 List 控件则返回其句柄，否则返回 NULL。
     */
    HWND GetListBox(void);

    void ResetContent(void);
    int SetCurSel(__in int nSelect);
    int SetItemData(__in int nIndex, __in DWORD_PTR dwItemData);
};

#ifdef UNICODE
#define GetLBTextLen    GetLBTextLenW
#else
#define GetLBTextLen    GetLBTextLenA
#endif // UNICODE

/**
 * \class LEdit
 * \brief PDL 编辑框控件类
 */

class LEdit : public LWnd
{
public:
    LEdit(__in HWND hWnd = NULL);
    LEdit& operator=(__in HWND hWnd);
    operator HWND(void);
public:
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
        __in PVOID lpParam);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
        __in PVOID lpParam);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);
    void LimitTextA(__in int nMaxChars);
    void LimitTextW(__in int nMaxChars);
    void ReplaceSel(__in PCSTR lpszNewText,
        __in BOOL bCanUndo = FALSE);
    void ReplaceSel(__in PCWSTR lpszNewText,
        __in BOOL bCanUndo = FALSE);
    void SetSelA(__in int nStartChar, __in int nEndChar);
    void SetSelW(__in int nStartChar, __in int nEndChar);
};

#ifdef UNICODE
#define LimitText   LimitTextW
#define SetSel      SetSelW
#else
#define LimitText   LimitTextA
#define SetSel      SetSelA
#endif // UNICODE

/**
 * \class LListBox
 * \brief PDL 列表框控件类
 */

class LListBox : public LWnd
{
public:
    LListBox& operator=(__in HWND hWnd);
public:
    int AddString(__in PCSTR lpszString);
    int AddString(__in PCWSTR lpszString);
    int DeleteString(__in int nIndex);
    int FindString(__in int nStartAfter, __in PCSTR lpszItem);
    int FindString(__in int nStartAfter, __in PCWSTR lpszItem);
    int GetCount(void);
    int GetCurSel(void);
    DWORD_PTR GetItemData(__in int nIndex);
    int GetText(__in int nIndex, __in PSTR lpszBuffer);
    int GetText(__in int nIndex, __in PWSTR lpszBuffer);
    void ResetContent(void);
    int SetCurSel(__in int nSelect);
    int SetItemData(__in int nIndex, __in DWORD_PTR dwItemData);
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
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
        __in PVOID lpParam);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
        __in PVOID lpParam);
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in int x, __in int y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in HMENU hMenu, __in PVOID lpParam);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in int x, __in int y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in HMENU hMenu, __in PVOID lpParam);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in int X, __in int Y, __in int nWidth,
        __in int nHeight, __in HWND hWndParent, __in HMENU hMenu,
        __in PVOID lpParam);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in int X, __in int Y, __in int nWidth,
        __in int nHeight, __in HWND hWndParent, __in HMENU hMenu,
        __in PVOID lpParam);
    HICON SetIcon(__in HICON hIcon);
};
