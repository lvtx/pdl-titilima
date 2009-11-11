/**
 * \file pdl_window.h
 * \brief PDL 窗口类定义
 * \details 这个文件中定义了 PDL 中所有基本的窗口类：
 *   \li \c LWnd PDL 基本窗口类
 *   \li \c LMsgWnd PDL 窗口消息类
 *   \li \c LSubclassWnd PDL 子类化类
 *   \li \c LWindow PDL 基本窗口封装类
 *   \li \c LDialog PDL 基本对话框封装类
 *   \li \c LDrawItem PDL 自绘辅助类
 *   \li \c LCustomDraw PDL 自绘辅助类
 *   \li \c LNotify PDL 通知消息自处理类
 */

#pragma once

#include "pdl_base.h"
#include "pdl_string.h"
#include <ShellAPI.h>

/**
 * \def PDL_DECLARE_WINCLASS
 * \brief 用于声明 PDL 窗口类的 OnGetPDLObject 与 GetHandle 成员函数。
 * \note 请在 PDL 窗口类的定义中使用。
 */
#define PDL_DECLARE_WINCLASS(x)                             \
    protected:                                              \
    PVOID OnGetPDLObject(PSTR lpClassName, DWORD dwSize);   \
    PVOID OnGetPDLObject(PWSTR lpClassName, DWORD dwSize);  \
    private:                                                \
    HWND GetHandle(void);

/**
 * \def PDL_DEFINE_WINCLASS
 * \brief 用于实现 PDL 窗口类的 OnGetPDLObject 与 GetHandle 成员函数。
 * \note 请在 PDL 窗口类的实现文件中使用。
 */

#define PDL_DEFINE_WINCLASS(x)                              \
PVOID x::OnGetPDLObject(PSTR lpClassName, DWORD dwSize)     \
{                                                           \
    strncpy(lpClassName, #x, dwSize);                       \
    return this;                                            \
}                                                           \
PVOID x::OnGetPDLObject(PWSTR lpClassName, DWORD dwSize)    \
{                                                           \
    wcsncpy(lpClassName, L###x, dwSize);                    \
    return this;                                            \
}                                                           \
HWND x::GetHandle(void)                                     \
{                                                           \
    return m_hWnd;                                          \
}

/**
 * \def PDL_ENABLE_NOTIFY
 * \brief 用于启用 PDL 窗口类的通知机制，包括 LDrawItem、LCustomDraw 和 LNotify。
 * \note 请在 PDL 窗口类的定义中使用。
 * \sa LDrawItem
 * \sa LCustomDraw
 * \sa LNotify
 */

#define PDL_ENABLE_NOTIFY()                                 \
    private: LRESULT OnGetPDLNotify(UINT nType);

/**
 * \def BEGIN_NOTIFY_MAP
 * \brief 用于定义 PDL 窗口类的通知支持。
 * \note 请在 PDL 窗口类的实现文件中使用。
 */
#define BEGIN_NOTIFY_MAP(Class)                             \
    LRESULT Class::OnGetPDLNotify(UINT nType)               \
    {

/**
 * \def NOTIFY_ITEM
 * \brief 用于定义 PDL 窗口类的通知支持。
 * \note 请在 PDL 窗口类的实现文件中使用。
 */
#define NOTIFY_ITEM(x, t)                                      \
    if (x == nType)                                         \
        return (LRESULT)(t*)this;

/**
 * \def END_NOTIFY_MAP
 * \brief 用于定义 PDL 窗口类的通知支持。
 * \note 请在 PDL 窗口类的实现文件中使用。
 */
#define END_NOTIFY_MAP(Class)                               \
        return 0;                                           \
    }

/**
 * \def WNDPOS_HCENTER
 * \brief 使窗口横向居中。
 * \sa LWnd::CenterWindow
 */
#define WNDPOS_HCENTER      0x00000001
/**
 * \def WNDPOS_VCENTER
 * \brief 使窗口纵向居中。
 * \sa LWnd::CenterWindow
 */
#define WNDPOS_VCENTER      0x00000002

/**
 * \class LWnd
 * \brief PDL 基本窗口类
 * \details LWnd 是 PDL 最基本的窗口类封装，它封装了大部分常用的窗口 API 函数。
 */
class LWnd
{
public:
    /**
     * 构造函数
     * @param [in] hWnd 一个窗口句柄。
     */
    LWnd(__in HWND hWnd = NULL);
public:
    BOOL AnimateWindow(__in DWORD dwTime, __in DWORD dwFlags);

    /**
     * 将 LWnd 对象附加到一个窗口上。
     * @param [in] hWnd 一个窗口句柄。
     * \sa Detach
     */
    void Attach(__in HWND hWnd);

    BOOL BringWindowToTop(void);

    /**
     * 将窗口居中。
     * @param [in] lprc 居中的相对矩形。当此参数为空时，相对矩形为父窗口的所在矩形。
     * @param [in] dwPos 居中样式。可以取下列值的组合：
     * @param [in] bRedraw 是否重绘窗口。
     * \li \c WNDPOS_HCENTER 使窗口横向居中。
     * \li \c WNDPOS_VCENTER 使窗口纵向居中。
     */
    void CenterWindow(__in LPCRECT lprc, __in DWORD dwPos,
        __in BOOL bRedraw = TRUE);

    BOOL CheckDlgButton(__in int nIDButton, __in UINT uCheck);
    BOOL CheckRadioButton(__in int nIDFirstButton, __in int nIDLastButton,
        __in int nIDCheckButton);
    BOOL ClientToScreen(__inout LPPOINT lpPoint);

    /**
     * 将窗口内容复制到剪贴板。
     * \note 该成员函数向窗口发送 WM_COPY 消息，所以通常只对 Edit 控件有效。欲使其能应用于自定义窗口，请响应 WM_COPY 消息。
     */
    void Copy(void);

    /**
     * 创建一个窗口。
     * @param [in] lpClassName 要创建的窗口类名。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] lpRect 窗口所占矩形。此参数为空时，将以 CW_USEDEFAULT 作为窗口的坐标参数。
     * @param [in] hWndParent 父窗口。
     * @param [in] nID 窗口的 ID。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in PCSTR lpClassName, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] lpClassName 要创建的窗口类名。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] lpRect 窗口所占矩形。此参数为空时，将以 CW_USEDEFAULT 作为窗口的坐标参数。
     * @param [in] hWndParent 父窗口。
     * @param [in] nID 窗口的 ID。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in PCWSTR lpClassName, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] lpClassName 要创建的窗口类名。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] x 窗口的横坐标。
     * @param [in] y 窗口的纵坐标。
     * @param [in] nWidth 窗口的宽度。
     * @param [in] nHeight 窗口的高度。
     * @param [in] hWndParent 父窗口。
     * @param [in] hMenu 窗口的菜单。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in PCSTR lpClassName, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in int x, __in int y, __in int nWidth,
        __in int nHeight, __in HWND hWndParent, __in HMENU hMenu,
        __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] lpClassName 要创建的窗口类名。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] x 窗口的横坐标。
     * @param [in] y 窗口的纵坐标。
     * @param [in] nWidth 窗口的宽度。
     * @param [in] nHeight 窗口的高度。
     * @param [in] hWndParent 父窗口。
     * @param [in] hMenu 窗口的菜单。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in PCWSTR lpClassName, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in int x, __in int y, __in int nWidth,
        __in int nHeight, __in HWND hWndParent, __in HMENU hMenu,
        __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] dwExStyle 窗口的附加样式。
     * @param [in] lpClassName 要创建的窗口类名。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] lpRect 窗口所占矩形。此参数为空时，将以 CW_USEDEFAULT 作为窗口的坐标参数。
     * @param [in] hWndParent 父窗口。
     * @param [in] nID 窗口的 ID。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpClassName,
        __in PCSTR lpWindowName, __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] dwExStyle 窗口的附加样式。
     * @param [in] lpClassName 要创建的窗口类名。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] lpRect 窗口所占矩形。此参数为空时，将以 CW_USEDEFAULT 作为窗口的坐标参数。
     * @param [in] hWndParent 父窗口。
     * @param [in] nID 窗口的 ID。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpClassName,
        __in PCWSTR lpWindowName, __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] dwExStyle 窗口的附加样式。
     * @param [in] lpClassName 要创建的窗口类名。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] X 窗口的横坐标。
     * @param [in] Y 窗口的纵坐标。
     * @param [in] nWidth 窗口的宽度。
     * @param [in] nHeight 窗口的高度。
     * @param [in] hWndParent 父窗口。
     * @param [in] hMenu 窗口的菜单。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpClassName,
        __in PCSTR lpWindowName, __in DWORD dwStyle,
        __in int X, __in int Y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in HMENU hMenu, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] dwExStyle 窗口的附加样式。
     * @param [in] lpClassName 要创建的窗口类名。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] X 窗口的横坐标。
     * @param [in] Y 窗口的纵坐标。
     * @param [in] nWidth 窗口的宽度。
     * @param [in] nHeight 窗口的高度。
     * @param [in] hWndParent 父窗口。
     * @param [in] hMenu 窗口的菜单。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpClassName,
        __in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in int X, __in int Y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in HMENU hMenu, __in PVOID lpParam);

    BOOL DestroyWindow(void);

    /**
     * 解除 LWnd 对象对窗口的附加状态。
     * @return 原附加的窗口句柄。
     * \sa Attach
     */
    HWND Detach(void);

    /**
     * 改变一个子窗口的可用状态。
     * @param [in] nIDDlgItem 子窗口的 ID。
     * @param [in] bEnable 设置该子窗口是否有效。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL EnableDlgItem(__in int nIDDlgItem, __in BOOL bEnable = TRUE);

    BOOL EnableWindow(__in BOOL bEnable = TRUE);

    /**
     * 弹出包含错误代码及描述的消息对话框。
     * @param [in] dwErrCode 一个由 GetLastError 返回的有效错误代码。
     * @param [in] hWnd 消息框父窗口。
     * @param [in] lpText 消息框文本。
     * @param [in] lpCaption 消息框标题。
     * @param [in] uType 消息框样式，取值可参考 MSDN 中的 MessageBox 函数。
     * @return 返回值可参考 MSDN 中的 MessageBox 函数。
     */
    static int ErrorBox(__in DWORD dwErrCode, __in_opt HWND hWnd,
        __in_opt PCSTR lpText, __in_opt PCSTR lpCaption, __in UINT uType);

    /**
     * 弹出包含错误代码及描述的消息对话框。
     * @param [in] dwErrCode 一个由 GetLastError 返回的有效错误代码。
     * @param [in] hWnd 消息框父窗口。
     * @param [in] lpText 消息框文本。
     * @param [in] lpCaption 消息框标题。
     * @param [in] uType 消息框样式，取值可参考 MSDN 中的 MessageBox 函数。
     * @return 返回值可参考 MSDN 中的 MessageBox 函数。
     */
    static int ErrorBox(__in DWORD dwErrCode, __in_opt HWND hWnd,
        __in_opt PCWSTR lpText, __in_opt PCWSTR lpCaption, __in UINT uType);

    DWORD GetClassLong(__in int nIndex);
#ifdef UNICODE
    DWORD GetClassLongA(__in int nIndex);
#else
    DWORD GetClassLongW(__in int nIndex);
#endif // UNICODE
    BOOL GetClientRect(__out LPRECT lpRect);
    int GetDlgCtrlID(void);
    HWND GetDlgItem(__in int nIDDlgItem);
    UINT GetDlgItemInt(__in int nIDDlgItem, __out_opt BOOL *lpTranslated,
        __in BOOL bSigned);
    UINT GetDlgItemText(__in int nIDDlgItem, __out PTSTR lpString,
        __in int nMaxCount);
#ifdef UNICODE
    UINT GetDlgItemTextA(__in int nIDDlgItem, __out PSTR lpString,
        __in int nMaxCount);
#else
    UINT GetDlgItemTextW(__in int nIDDlgItem, __out PWSTR lpString,
        __in int nMaxCount);
#endif // UNICODE

    /**
     * 获取窗口的扩展样式。
     * @return 窗口的扩展样式。
     */
    DWORD GetExStyle(void);

    HMENU GetMenu(void);
    HWND GetParent(void);

    /**
     * 获取窗口在父窗口中的矩形。
     * @param [out] lpRect 用于接收窗口的矩形信息。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetRectInParent(__out LPRECT lpRect);

    /**
     * 获取窗口的句柄。
     * @return 如果 LWnd 对象所附加的窗口句柄是有效窗口则返回这个句柄，否则返回 NULL。
     */
    HWND GetSafeHWND(void) const;

    BOOL GetScrollInfo(__in int nBar, __inout LPSCROLLINFO lpsi);

    /**
     * 获取窗口的尺寸，包括非客户区的面积。
     * @param [out] size 用于接收窗口的尺寸信息。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetSize(__out LPSIZE size);

    /**
     * 获取窗口的样式。
     * @return 窗口的样式。
     */
    DWORD GetStyle(void);

    HWND GetWindow(__in UINT uCmd);
    LONG GetWindowLong(__in int nIndex);
#ifdef UNICODE
    LONG GetWindowLongA(__in int nIndex);
#else
    LONG GetWindowLongW(__in int nIndex);
#endif // UNICODE
    BOOL GetWindowRect(__out LPRECT lpRect);
    int GetWindowText(__out PTSTR lpString, __in int nMaxCount);
#ifdef UNICODE
    int GetWindowTextA(__out PSTR lpString, __in int nMaxCount);
#else
    int GetWindowTextW(__out PWSTR lpString, __in int nMaxCount);
#endif // UNICODE

    /**
     * 获取窗口的文本。
     * @param [out] pStr 用于接收窗口文本的 LString 指针。
     * @return 如果成功则返回 pStr 的长度，否则返回 0。
     */
    int GetWindowText(__out LString *pStr);
#ifdef UNICODE
    int GetWindowTextA(__out LStringA *pStr);
#else
    int GetWindowTextW(__out LStringW *pStr);
#endif // UNICODE

    int GetWindowTextLength(void);
#ifdef UNICODE
    int GetWindowTextLengthA(void);
#else
    int GetWindowTextLengthW(void);
#endif // UNICODE
    BOOL KillTimer(__in UINT_PTR uIDEvent);

    /**
     * 刷新窗口。
     * @param [in] bErase 是否擦除背景。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Invalidate(__in BOOL bErase = TRUE);

    BOOL InvalidateRect(__in LPCRECT lpRect, __in BOOL bErase = TRUE);
    UINT IsDlgButtonChecked(__in int nIDButton);
    BOOL IsWindow(void) const;
    BOOL IsWindowUnicode(void);
    BOOL IsWindowVisible(void);
    int MessageBox(__in PCTSTR lpszText, __in PCTSTR lpszCaption = NULL,
        __in UINT nType = MB_OK);
#ifdef UNICODE
    int MessageBoxA(__in PCSTR lpszText, __in PCSTR lpszCaption = NULL,
        __in UINT nType = MB_OK);
#else
    int MessageBoxW(__in PCWSTR lpszText, __in PCWSTR lpszCaption = NULL,
        __in UINT nType = MB_OK);
#endif // UNICODE
    BOOL MoveWindow(__in LPCRECT lprc, __in BOOL bRepaint = TRUE);
    BOOL MoveWindow(__in int X, __in int Y, __in int nWidth,
        __in int nHeight, __in BOOL bRepaint = TRUE);
    BOOL OpenClipboard(void);
    BOOL PostMessage(__in UINT uMsg, __in WPARAM wParam = 0,
        __in LPARAM lParam = 0);
#ifdef UNICODE
    BOOL PostMessageA(__in UINT uMsg, __in WPARAM wParam = 0,
        __in LPARAM lParam = 0);
#else
    BOOL PostMessageW(__in UINT uMsg, __in WPARAM wParam = 0,
        __in LPARAM lParam = 0);
#endif // UNICODE
    BOOL ScreenToClient(__inout LPPOINT lpPoint);
    BOOL ScrollWindow(__in int XAmount, __in int YAmount,
        __in_opt CONST RECT *lpRect, __in_opt CONST RECT *lpClipRect);
    LRESULT SendMessage(__in UINT uMsg, __in WPARAM wParam = 0,
        __in LPARAM lParam = 0);
#ifdef UNICODE
    LRESULT SendMessageA(__in UINT uMsg, __in WPARAM wParam = 0,
        __in LPARAM lParam = 0);
#else
    LRESULT SendMessageW(__in UINT uMsg, __in WPARAM wParam = 0,
        __in LPARAM lParam = 0);
#endif // UNICODE
    HWND SetCapture(void);
    BOOL SetDlgItemInt(__in int nIDDlgItem, __in UINT uValue,
        __in BOOL bSigned);
    BOOL SetDlgItemText(__in int nID, __in PCTSTR lpszString);
#ifdef UNICODE
    BOOL SetDlgItemTextA(__in int nID, __in PCSTR lpszString);
#else
    BOOL SetDlgItemTextW(__in int nID, __in PCWSTR lpszString);
#endif // UNICODE
    HWND SetFocus(void);

    /**
     * 设置窗口的字体。
     * @param [in] hFont 要设置的字体句柄。
     * @param [in] bRedraw 是否重绘窗口。
     */
    void SetFont(__in HFONT hFont, __in BOOL bRedraw = TRUE);

    BOOL SetForegroundWindow(void);

    /**
     * 设置窗口的图标。
     * @param [in] hIcon 要设置的图标句柄。
     * @param [in] bBigIcon 是否大图标。
     * @return 设置图标前的图标句柄。
     */
    HICON SetIcon(__in HICON hIcon, __in BOOL bBigIcon);

    int SetScrollInfo(__in int nBar, __in LPCSCROLLINFO lpsi,
        __in BOOL redraw);
    UINT_PTR SetTimer(__in UINT_PTR nIDEvent, __in UINT uElapse,
        __in TIMERPROC lpTimerFunc = NULL);
    LONG SetWindowLongA(__in int nIndex, __in LONG dwNewLong);
    LONG SetWindowLongW(__in int nIndex, __in LONG dwNewLong);
    BOOL SetWindowPos(__in HWND hWndInsertAfter, __in LPCRECT lprc,
        __in UINT uFlags);
    BOOL SetWindowPos(__in HWND hWndInsertAfter, __in int X, __in int Y,
        __in int cx, __in int cy, __in UINT uFlags);
    int SetWindowRgn(__in_opt HRGN hRgn, __in BOOL bRedraw);
    BOOL SetWindowText(__in PCTSTR lpszString);
#ifdef UNICODE
    BOOL SetWindowTextA(__in PCSTR lpszString);
#else
    BOOL SetWindowTextW(__in PCWSTR lpszString);
#endif // UNICODE

    /**
     * 按照窗口的文本内容改变窗口的大小。
     * @param [in] bRedraw 是否重绘窗口。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL SizeToContent(__in BOOL bRedraw = TRUE);

    BOOL ShowWindow(__in int nCmdShow);
    BOOL UpdateWindow(void);
protected:
    /**
     * 窗口的句柄
     */
    HWND m_hWnd;
};


/**
 * \class LMsgWnd
 * \brief PDL 窗口消息类
 * \details LMsgWnd 是 PDL 中用于处理窗口消息的类，它封装了大部分常用的窗口消息处理函数。
 */

class LThunk;
class LDrawItem;
class LCustomDraw;
class LNotify;

class PDL_NO_VTABLE LMsgWnd
{
    friend class LDrawItem;
    friend class LCustomDraw;
public:
    LMsgWnd(void);
    ~LMsgWnd(void);
protected:

    /**
     * 使用一个 LMsgWnd 子类化一个窗口。
     * @param [in] hWnd 要子类化的窗口句柄。
     * @param [in] proc 要替换的窗口过程。
     * @return 窗口原有的窗口过程。
     */
    WNDPROC Attach(__in HWND hWnd, __in WNDPROC proc);

    /**
     * 获取 LMsgWnd 所属窗口的句柄。
     * @return LMsgWnd 所属窗口的句柄。
     * \note 这个函数会被 LMsgWnd 的内部调用，用以进行必要的操作。任何继承了 LMsgWnd 的窗口类都必须实现这个函数。PDL_DECLARE_WINCLASS/PDL_DEFINE_WINCLASS 宏可以简化这一操作。
     * \sa PDL_DECLARE_WINCLASS
     * \sa PDL_DEFINE_WINCLASS
     */
    virtual HWND GetHandle(void) = 0;

protected:
    virtual void OnActivate(UINT nState, HWND hWndOther, BOOL bMinimized,
        BOOL& bHandled);
    virtual void OnClose(BOOL& bHandled);
    virtual void OnCommand(WORD wNotifyCode, WORD wID, HWND hWndCtrl,
        BOOL& bHandled);
    virtual void OnContextMenu(HWND hWnd, int x, int y, BOOL& bHandled);
    virtual int OnCreate(LPCREATESTRUCT lpCs, BOOL& bHandled);
#ifdef UNICODE
    virtual int OnCreate(LPCREATESTRUCTA lpCs, BOOL& bHandled);
#else
    virtual int OnCreate(LPCREATESTRUCTW lpCs, BOOL& bHandled);
#endif // UNICODE
    virtual void OnDestroy(BOOL& bHandled);
#ifndef _WIN32_WCE
    virtual void OnDropFiles(HDROP hDropInfo, BOOL& bHandled);
#endif // _WIN32_WCE
    virtual BOOL OnEraseBkgnd(HDC hdc, BOOL& bHandled);

    /**
     * 用于响应 WM_PDL_GETNOTIFY
     * \sa WM_PDL_GETNOTIFY
     */
    virtual LRESULT OnGetPDLNotify(UINT nType);

    /**
     * 用于响应 WM_PDL_GETOBJECTA
     * \sa WM_PDL_GETOBJECTA
     */
    virtual PVOID OnGetPDLObject(PSTR lpClassName, DWORD dwSize);

    /**
     * 用于响应 WM_PDL_GETOBJECTW
     * \sa WM_PDL_GETOBJECTW
     */
    virtual PVOID OnGetPDLObject(PWSTR lpClassName, DWORD dwSize);

    virtual void OnHScroll(UINT nCode, UINT nPos, HWND hScrollBar,
        BOOL& bHandled);
    virtual BOOL OnInitDialog(HWND hCtrlFocus, LPARAM lParam, BOOL& bHandled);
    virtual void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags,
        BOOL& bHandled);
    virtual void OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags,
        BOOL& bHandled);
    virtual void OnLButtonDblClk(UINT uFlags, int x, int y, BOOL& bHandled);
    virtual void OnLButtonDown(UINT uFlags, int x, int y, BOOL& bHandled);
    virtual void OnLButtonUp(UINT uFlags, int x, int y, BOOL& bHandled);
    virtual LRESULT OnMessage(UINT uMsg, WPARAM wParam, LPARAM lParam,
        BOOL& bHandled);
    virtual void OnMouseLeave(BOOL& bHandled);
    virtual void OnMouseMove(UINT uFlags, int x, int y, BOOL& bHandled);
    virtual void OnNcCalcSize(BOOL bCalcValidRects,
        LPNCCALCSIZE_PARAMS lpncsp, BOOL& bHandled);
    virtual LRESULT OnNcHitTest(int x, int y, BOOL& bHandled);
    virtual LRESULT OnNotify(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);
    virtual void OnPaint(BOOL& bHandled);
    virtual void OnRButtonDblClk(UINT uFlags, int x, int y, BOOL& bHandled);
    virtual void OnRButtonDown(UINT uFlags, int x, int y, BOOL& bHandled);
    virtual void OnRButtonUp(UINT uFlags, int x, int y, BOOL& bHandled);
    virtual BOOL OnSetCursor(HWND hWnd, UINT nHitTest, UINT message,
        BOOL& bHandled);
    virtual void OnSetFocus(HWND hOldFocus, BOOL& bHandled);
    virtual void OnShowWindow(BOOL bShow, UINT nStatus, BOOL& bHandled);
    virtual void OnSize(UINT nType, int cx, int cy, BOOL& bHandled);
    virtual void OnSizing(UINT nSize, LPRECT lpRect, BOOL& bHandled);
    virtual void OnTimer(UINT_PTR nIDEvent, BOOL& bHandled);
    virtual void OnVScroll(UINT nCode, UINT nPos, HWND hScrollBar,
        BOOL& bHandled);
protected:
    LRESULT HandleNotify(UINT uMsg, WPARAM wParam, LPARAM lParam,
        BOOL& bHandled);
    LRESULT HandlePDLMessage(UINT uMsg, WPARAM wParam, LPARAM lParam);
    LRESULT HandleWndMessage(UINT uMsg, WPARAM wParam, LPARAM lParam,
        BOOL& bHandled);

    /**
     * 消息处理完毕后的通知函数。
     * @param [in] uMsg 处理过的消息。
     * @param [in] wParam 消息的附加参数。
     * @param [in] lParam 消息的附加参数。
     * @param [in] lRet 消息处理的返回值。
     */
    virtual void OnMsgProcceded(UINT uMsg, WPARAM wParam, LPARAM lParam,
        LRESULT lRet);
protected:
    /**
     * 用于子类化的 Thunk 对象指针
     */
    LThunk* m_thunk;
};

/**
 * \class LSubclassWnd
 * \brief PDL 子类化类
 * \details LSubclassWnd 是 PDL 中用于子类化窗口的类。
 */

class PDL_NO_VTABLE LSubclassWnd : public LMsgWnd
{
protected:
    /**
     * 构造函数
     */
    LSubclassWnd(void);
public:

    /**
     * 子类化指定的窗口，并将 LSubclassWnd 对象附着在目标窗口上。
     * @param [in] hWnd 要子类化的窗口。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL SubclassWindow(__in HWND hWnd);

protected:

    /**
     * 内部回调函数。
     */
    static LRESULT CALLBACK WindowProc(LSubclassWnd* This, UINT uMsg,
        WPARAM wParam, LPARAM lParam);

protected:
    /**
     * 子类化前的窗口过程
     */
    WNDPROC m_pfnWndProc;
};

/**
 * \class LWindow
 * \brief PDL 基本窗口封装类
 * \details LWindow 是 PDL 中最基本的窗口封装类，它封装了大部分的窗口操作和响应。
 */

class LWindow : public LWnd, public LMsgWnd
{
    PDL_DECLARE_WINCLASS(LWindow)
public:
    LWindow(void);

    /**
     * 构造函数
     * @param [in] hWnd 一个有效的窗口句柄。
     */
    LWindow(__in HWND hWnd);

    /**
     * 构造函数
     * @param [in] wc 要注册的窗口类信息。
     * \note wc 之中的 lpfnWndProc 会被自动替换为 StartWndProc。
     */
    LWindow(__in LPWNDCLASSA wc);

    /**
     * 构造函数
     * @param [in] wc 要注册的窗口类信息。
     * \note wc 之中的 lpfnWndProc 会被自动替换为 StartWndProc。
     */
    LWindow(__in LPWNDCLASSW wc);

public:

    /**
     * 将 LWindow 对象附着在目标窗口上。
     * @param [in] hWnd 一个有效的窗口句柄。
     */
    void Attach(__in HWND hWnd);

    /**
     * 创建一个窗口。
     * @param [in] lpClassName 要创建的窗口类。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] lpRect 窗口所占矩形。此参数为空时，将以 CW_USEDEFAULT 作为窗口的坐标参数。
     * @param [in] hWndParent 父窗口。
     * @param [in] nID 窗口的 ID。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in PCSTR lpClassName, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] lpClassName 要创建的窗口类，如果为 NULL 则使用 m_atom。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] lpRect 窗口所占矩形。此参数为空时，将以 CW_USEDEFAULT 作为窗口的坐标参数。
     * @param [in] hWndParent 父窗口。
     * @param [in] nID 窗口的 ID。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in PCWSTR lpClassName, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] lpClassName 要创建的窗口类，如果为 NULL 则使用 m_atom。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] x 窗口的横坐标。
     * @param [in] y 窗口的纵坐标。
     * @param [in] nWidth 窗口的宽度。
     * @param [in] nHeight 窗口的高度。
     * @param [in] hWndParent 父窗口。
     * @param [in] hMenu 窗口的菜单。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in PCSTR lpClassName, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in int x, __in int y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in HMENU hMenu, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] lpClassName 要创建的窗口类，如果为 NULL 则使用 m_atom。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] x 窗口的横坐标。
     * @param [in] y 窗口的纵坐标。
     * @param [in] nWidth 窗口的宽度。
     * @param [in] nHeight 窗口的高度。
     * @param [in] hWndParent 父窗口。
     * @param [in] hMenu 窗口的菜单。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in PCWSTR lpClassName, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in int x, __in int y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in HMENU hMenu, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] dwExStyle 窗口的附加样式。
     * @param [in] lpClassName 要创建的窗口类，如果为 NULL 则使用 m_atom。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] lpRect 窗口所占矩形。此参数为空时，将以 CW_USEDEFAULT 作为窗口的坐标参数。
     * @param [in] hWndParent 父窗口。
     * @param [in] nID 窗口的 ID。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpClassName,
        __in PCSTR lpWindowName, __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] dwExStyle 窗口的附加样式。
     * @param [in] lpClassName 要创建的窗口类，如果为 NULL 则使用 m_atom。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] lpRect 窗口所占矩形。此参数为空时，将以 CW_USEDEFAULT 作为窗口的坐标参数。
     * @param [in] hWndParent 父窗口。
     * @param [in] nID 窗口的 ID。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpClassName,
        __in PCWSTR lpWindowName, __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] dwExStyle 窗口的附加样式。
     * @param [in] lpClassName 要创建的窗口类，如果为 NULL 则使用 m_atom。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] X 窗口的横坐标。
     * @param [in] Y 窗口的纵坐标。
     * @param [in] nWidth 窗口的宽度。
     * @param [in] nHeight 窗口的高度。
     * @param [in] hWndParent 父窗口。
     * @param [in] hMenu 窗口的菜单。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpClassName,
        __in PCSTR lpWindowName, __in DWORD dwStyle, __in int X, __in int Y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in HMENU hMenu, __in PVOID lpParam);

    /**
     * 创建一个窗口。
     * @param [in] dwExStyle 窗口的附加样式。
     * @param [in] lpClassName 要创建的窗口类，如果为 NULL 则使用 m_atom。
     * @param [in] lpWindowName 要创建的窗口标题。
     * @param [in] dwStyle 要创建的窗口样式。
     * @param [in] X 窗口的横坐标。
     * @param [in] Y 窗口的纵坐标。
     * @param [in] nWidth 窗口的宽度。
     * @param [in] nHeight 窗口的高度。
     * @param [in] hWndParent 父窗口。
     * @param [in] hMenu 窗口的菜单。
     * @param [in] lpParam 窗口的附加参数。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpClassName,
        __in PCWSTR lpWindowName, __in DWORD dwStyle, __in int X, __in int Y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in HMENU hMenu, __in PVOID lpParam);

    /**
     * 注册一个窗口类。
     * @param [in] wc 要注册的窗口类信息。
     * @return 如果注册成功则返回 TRUE，否则返回 FALSE。
     * \note wc 之中的 lpfnWndProc 会被自动替换为 StartWndProc。
     */
    BOOL Register(__in LPWNDCLASSA wc);

    /**
     * 注册一个窗口类。
     * @param [in] wc 要注册的窗口类信息。
     * @return 如果注册成功则返回 TRUE，否则返回 FALSE。
     * \note wc 之中的 lpfnWndProc 会被自动替换为 StartWndProc。
     */
    BOOL Register(__in LPWNDCLASSW wc);

protected:
    /**
     * 初始的窗口过程。
     */
    static LRESULT CALLBACK StartWndProc(HWND hWnd, UINT uMsg,
        WPARAM wParam, LPARAM lParam);
    /**
     * 子类化后的窗口过程。
     */
    static LRESULT CALLBACK WindowProc(LWindow* This, UINT uMsg,
        WPARAM wParam, LPARAM lParam);
protected:
    /**
     * 窗口类的 ATOM
     */
    ATOM m_atom;
    /**
     * 子类化前的窗口过程
     */
    WNDPROC m_pfnWndProc;
};

/**
 * \class LDialog
 * \brief PDL 基本对话框封装类
 * \details LDialog 是 PDL 中最基本的对话框封装类，它封装了对话框的基本操作和响应。
 */

class LIniParser;
class LDialog : public LWnd, public LMsgWnd
{
    PDL_DECLARE_WINCLASS(LDialog)
public:

    /**
     * 构造函数
     * @param [in] uIDDialog 对话框的资源 ID。
     * @param [in] lang ini 格式的语言文件。
     */
    LDialog(__in UINT uIDDialog, __in LIniParser* lang = NULL);

public:
    /**
     * 创建非模态对话框。
     * @param [in] hParent 父窗口句柄。
     * @param [in] lParam 附加参数。
     * @return 如果创建成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in HWND hParent = ::GetActiveWindow(),
        __in LPARAM lParam = 0);

    /**
     * 创建模态对话框。
     * @param [in] hParent 父窗口句柄。
     * @param [in] lParam 附加参数。
     * @return 对话框结束的返回值。
     */
    int DoModal(__in HWND hParent = ::GetActiveWindow(),
        __in LPARAM lParam = 0);

    BOOL EndDialog(__in INT_PTR nResult);

    /**
     * 设置拥有焦点的控件。
     * @param [in] hCtrl 将要拥有焦点控件的句柄。
     */
    void SetFocusCtrl(__in HWND hCtrl);

    /**
     * 设置对话框的字体。
     * @param [in] hFont 要设置的字体。
     * @param [in] bAllCtrls 是否设置所有控件的字体。
     * @param [in] bRedraw 是否重绘窗口。
     */
    void SetFont(__in HFONT hFont, __in BOOL bAllCtrls = TRUE,
        __in BOOL bRedraw = TRUE);

protected:
    /**
     * 初始的窗口过程。
     */
    static INT_PTR CALLBACK StartDlgProc(HWND hDlg, UINT uMsg,
        WPARAM wParam, LPARAM lParam);
    /**
     * 子类化后的窗口过程。
     */
    static INT_PTR CALLBACK DialogProc(LDialog* This, UINT uMsg,
        WPARAM wParam, LPARAM lParam);
protected:
    /**
     * 加载语言文件。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL LoadLanguageRes(void);
protected:
    /**
     * 对话框的资源 ID
     */
    UINT m_uId;
    /**
     * 语言文件
     */
    LIniParser* m_lang;
};

/**
 * \class LDrawItem
 * \brief PDL 自绘辅助类
 * \details LDrawItem 是 PDL 中对 WM_DRAWITEM 的封装类，它封装了对 WM_DRAWITEM 各种处理的响应。
 * \sa PDL_ENABLE_NOTIFY
 */

class LDrawItem
{
    friend class LMsgWnd;
protected:
    virtual int OnCompareItem(PCOMPAREITEMSTRUCT cis);
    virtual BOOL OnDeleteItem(PDELETEITEMSTRUCT dis);
    virtual BOOL OnDrawItem(PDRAWITEMSTRUCT dis);
    virtual BOOL OnMeasureItem(PMEASUREITEMSTRUCT mis);
};


/**
 * \class LCustomDraw
 * \brief PDL 自绘辅助类
 * \details LCustomDraw 是 PDL 中对 NM_CUSTOMDRAW 的封装类，它封装了对 NM_CUSTOMDRAW 各种处理的响应。
 * \sa PDL_ENABLE_NOTIFY
 */

class LCustomDraw
{
    friend class LMsgWnd;
protected:
    virtual DWORD OnPrePaint(int idCtl, LPNMCUSTOMDRAW cd);
    virtual DWORD OnPostPaint(int idCtl, LPNMCUSTOMDRAW cd);
    virtual DWORD OnPreErase(int idCtl, LPNMCUSTOMDRAW cd);
    virtual DWORD OnPostErase(int idCtl, LPNMCUSTOMDRAW cd);

    virtual DWORD OnItemPrePaint(int idCtl, LPNMCUSTOMDRAW cd);
    virtual DWORD OnItemPostPaint(int idCtl, LPNMCUSTOMDRAW cd);
    virtual DWORD OnItemPreErase(int idCtl, LPNMCUSTOMDRAW cd);
    virtual DWORD OnItemPostErase(int idCtl, LPNMCUSTOMDRAW cd);

    virtual DWORD OnSubItemPrePaint(int idCtl, LPNMCUSTOMDRAW cd);
};

/**
 * \class LNotify
 * \brief PDL 通知处理类
 * \details LNotify 封装了对 WM_COMMAND 和 WM_NOTIFY 的通知处理。
 * \sa PDL_ENABLE_NOTIFY
 */

class LNotify
{
    friend class LMsgWnd;
protected:
    virtual void OnCmdNotify(WORD id, WORD wCode, HWND hCtrl, BOOL& bHandled);
    virtual LRESULT OnMsgNotify(int id, LPNMHDR nmh, BOOL& bHandled);
};
