/**
 * \file pdl_window.h
 * \brief PDL �����ඨ��
 * \details ����ļ��ж����� PDL �����л����Ĵ����ࣺ
 *   \li \c LWnd PDL ����������
 *   \li \c LMsgWnd PDL ������Ϣ��
 *   \li \c LSubclassWnd PDL ���໯��
 *   \li \c LWindow PDL �������ڷ�װ��
 *   \li \c LDialog PDL �����Ի����װ��
 *   \li \c LDrawItem PDL �Ի渨����
 *   \li \c LCustomDraw PDL �Ի渨����
 *   \li \c LNotify PDL ֪ͨ��Ϣ�Դ�����
 */

#pragma once

#include "pdl_base.h"
#include "pdl_string.h"
#include <ShellAPI.h>

/**
 * \def PDL_DECLARE_WINCLASS
 * \brief �������� PDL ������� OnGetPDLObject �� GetHandle ��Ա������
 * \note ���� PDL ������Ķ�����ʹ�á�
 */
#define PDL_DECLARE_WINCLASS(x)                             \
    protected:                                              \
    PVOID OnGetPDLObject(PSTR lpClassName, DWORD dwSize);   \
    PVOID OnGetPDLObject(PWSTR lpClassName, DWORD dwSize);  \
    private:                                                \
    HWND GetHandle(void);

/**
 * \def PDL_DEFINE_WINCLASS
 * \brief ����ʵ�� PDL ������� OnGetPDLObject �� GetHandle ��Ա������
 * \note ���� PDL �������ʵ���ļ���ʹ�á�
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
 * \brief �������� PDL �������֪ͨ���ƣ����� LDrawItem��LCustomDraw �� LNotify��
 * \note ���� PDL ������Ķ�����ʹ�á�
 * \sa LDrawItem
 * \sa LCustomDraw
 * \sa LNotify
 */

#define PDL_ENABLE_NOTIFY()                                 \
    private: LRESULT OnGetPDLNotify(UINT nType);

/**
 * \def BEGIN_NOTIFY_MAP
 * \brief ���ڶ��� PDL �������֪֧ͨ�֡�
 * \note ���� PDL �������ʵ���ļ���ʹ�á�
 */
#define BEGIN_NOTIFY_MAP(Class)                             \
    LRESULT Class::OnGetPDLNotify(UINT nType)               \
    {

/**
 * \def NOTIFY_ITEM
 * \brief ���ڶ��� PDL �������֪֧ͨ�֡�
 * \note ���� PDL �������ʵ���ļ���ʹ�á�
 */
#define NOTIFY_ITEM(x, t)                                      \
    if (x == nType)                                         \
        return (LRESULT)(t*)this;

/**
 * \def END_NOTIFY_MAP
 * \brief ���ڶ��� PDL �������֪֧ͨ�֡�
 * \note ���� PDL �������ʵ���ļ���ʹ�á�
 */
#define END_NOTIFY_MAP(Class)                               \
        return 0;                                           \
    }

/**
 * \def WNDPOS_HCENTER
 * \brief ʹ���ں�����С�
 * \sa LWnd::CenterWindow
 */
#define WNDPOS_HCENTER      0x00000001
/**
 * \def WNDPOS_VCENTER
 * \brief ʹ����������С�
 * \sa LWnd::CenterWindow
 */
#define WNDPOS_VCENTER      0x00000002

/**
 * \class LWnd
 * \brief PDL ����������
 * \details LWnd �� PDL ������Ĵ������װ������װ�˴󲿷ֳ��õĴ��� API ������
 */
class LWnd
{
public:
    /**
     * ���캯��
     * @param [in] hWnd һ�����ھ����
     */
    LWnd(__in HWND hWnd = NULL);
public:
    BOOL AnimateWindow(__in DWORD dwTime, __in DWORD dwFlags);

    /**
     * �� LWnd ���󸽼ӵ�һ�������ϡ�
     * @param [in] hWnd һ�����ھ����
     * \sa Detach
     */
    void Attach(__in HWND hWnd);

    BOOL BringWindowToTop(void);

    /**
     * �����ھ��С�
     * @param [in] lprc ���е���Ծ��Ρ����˲���Ϊ��ʱ����Ծ���Ϊ�����ڵ����ھ��Ρ�
     * @param [in] dwPos ������ʽ������ȡ����ֵ����ϣ�
     * @param [in] bRedraw �Ƿ��ػ洰�ڡ�
     * \li \c WNDPOS_HCENTER ʹ���ں�����С�
     * \li \c WNDPOS_VCENTER ʹ����������С�
     */
    void CenterWindow(__in LPCRECT lprc, __in DWORD dwPos,
        __in BOOL bRedraw = TRUE);

    BOOL CheckDlgButton(__in int nIDButton, __in UINT uCheck);
    BOOL CheckRadioButton(__in int nIDFirstButton, __in int nIDLastButton,
        __in int nIDCheckButton);
    BOOL ClientToScreen(__inout LPPOINT lpPoint);

    /**
     * ���������ݸ��Ƶ������塣
     * \note �ó�Ա�����򴰿ڷ��� WM_COPY ��Ϣ������ͨ��ֻ�� Edit �ؼ���Ч����ʹ����Ӧ�����Զ��崰�ڣ�����Ӧ WM_COPY ��Ϣ��
     */
    void Copy(void);

    /**
     * ����һ�����ڡ�
     * @param [in] lpClassName Ҫ�����Ĵ���������
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] lpRect ������ռ���Ρ��˲���Ϊ��ʱ������ CW_USEDEFAULT ��Ϊ���ڵ����������
     * @param [in] hWndParent �����ڡ�
     * @param [in] nID ���ڵ� ID��
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in PCSTR lpClassName, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] lpClassName Ҫ�����Ĵ���������
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] lpRect ������ռ���Ρ��˲���Ϊ��ʱ������ CW_USEDEFAULT ��Ϊ���ڵ����������
     * @param [in] hWndParent �����ڡ�
     * @param [in] nID ���ڵ� ID��
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in PCWSTR lpClassName, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] lpClassName Ҫ�����Ĵ���������
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] x ���ڵĺ����ꡣ
     * @param [in] y ���ڵ������ꡣ
     * @param [in] nWidth ���ڵĿ�ȡ�
     * @param [in] nHeight ���ڵĸ߶ȡ�
     * @param [in] hWndParent �����ڡ�
     * @param [in] hMenu ���ڵĲ˵���
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in PCSTR lpClassName, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in int x, __in int y, __in int nWidth,
        __in int nHeight, __in HWND hWndParent, __in HMENU hMenu,
        __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] lpClassName Ҫ�����Ĵ���������
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] x ���ڵĺ����ꡣ
     * @param [in] y ���ڵ������ꡣ
     * @param [in] nWidth ���ڵĿ�ȡ�
     * @param [in] nHeight ���ڵĸ߶ȡ�
     * @param [in] hWndParent �����ڡ�
     * @param [in] hMenu ���ڵĲ˵���
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in PCWSTR lpClassName, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in int x, __in int y, __in int nWidth,
        __in int nHeight, __in HWND hWndParent, __in HMENU hMenu,
        __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] dwExStyle ���ڵĸ�����ʽ��
     * @param [in] lpClassName Ҫ�����Ĵ���������
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] lpRect ������ռ���Ρ��˲���Ϊ��ʱ������ CW_USEDEFAULT ��Ϊ���ڵ����������
     * @param [in] hWndParent �����ڡ�
     * @param [in] nID ���ڵ� ID��
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpClassName,
        __in PCSTR lpWindowName, __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] dwExStyle ���ڵĸ�����ʽ��
     * @param [in] lpClassName Ҫ�����Ĵ���������
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] lpRect ������ռ���Ρ��˲���Ϊ��ʱ������ CW_USEDEFAULT ��Ϊ���ڵ����������
     * @param [in] hWndParent �����ڡ�
     * @param [in] nID ���ڵ� ID��
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpClassName,
        __in PCWSTR lpWindowName, __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] dwExStyle ���ڵĸ�����ʽ��
     * @param [in] lpClassName Ҫ�����Ĵ���������
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] X ���ڵĺ����ꡣ
     * @param [in] Y ���ڵ������ꡣ
     * @param [in] nWidth ���ڵĿ�ȡ�
     * @param [in] nHeight ���ڵĸ߶ȡ�
     * @param [in] hWndParent �����ڡ�
     * @param [in] hMenu ���ڵĲ˵���
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpClassName,
        __in PCSTR lpWindowName, __in DWORD dwStyle,
        __in int X, __in int Y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in HMENU hMenu, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] dwExStyle ���ڵĸ�����ʽ��
     * @param [in] lpClassName Ҫ�����Ĵ���������
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] X ���ڵĺ����ꡣ
     * @param [in] Y ���ڵ������ꡣ
     * @param [in] nWidth ���ڵĿ�ȡ�
     * @param [in] nHeight ���ڵĸ߶ȡ�
     * @param [in] hWndParent �����ڡ�
     * @param [in] hMenu ���ڵĲ˵���
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpClassName,
        __in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in int X, __in int Y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in HMENU hMenu, __in PVOID lpParam);

    BOOL DestroyWindow(void);

    /**
     * ��� LWnd ����Դ��ڵĸ���״̬��
     * @return ԭ���ӵĴ��ھ����
     * \sa Attach
     */
    HWND Detach(void);

    /**
     * �ı�һ���Ӵ��ڵĿ���״̬��
     * @param [in] nIDDlgItem �Ӵ��ڵ� ID��
     * @param [in] bEnable ���ø��Ӵ����Ƿ���Ч��
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL EnableDlgItem(__in int nIDDlgItem, __in BOOL bEnable = TRUE);

    BOOL EnableWindow(__in BOOL bEnable = TRUE);

    /**
     * ��������������뼰��������Ϣ�Ի���
     * @param [in] dwErrCode һ���� GetLastError ���ص���Ч������롣
     * @param [in] hWnd ��Ϣ�򸸴��ڡ�
     * @param [in] lpText ��Ϣ���ı���
     * @param [in] lpCaption ��Ϣ����⡣
     * @param [in] uType ��Ϣ����ʽ��ȡֵ�ɲο� MSDN �е� MessageBox ������
     * @return ����ֵ�ɲο� MSDN �е� MessageBox ������
     */
    static int ErrorBox(__in DWORD dwErrCode, __in_opt HWND hWnd,
        __in_opt PCSTR lpText, __in_opt PCSTR lpCaption, __in UINT uType);

    /**
     * ��������������뼰��������Ϣ�Ի���
     * @param [in] dwErrCode һ���� GetLastError ���ص���Ч������롣
     * @param [in] hWnd ��Ϣ�򸸴��ڡ�
     * @param [in] lpText ��Ϣ���ı���
     * @param [in] lpCaption ��Ϣ����⡣
     * @param [in] uType ��Ϣ����ʽ��ȡֵ�ɲο� MSDN �е� MessageBox ������
     * @return ����ֵ�ɲο� MSDN �е� MessageBox ������
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
     * ��ȡ���ڵ���չ��ʽ��
     * @return ���ڵ���չ��ʽ��
     */
    DWORD GetExStyle(void);

    HMENU GetMenu(void);
    HWND GetParent(void);

    /**
     * ��ȡ�����ڸ������еľ��Ρ�
     * @param [out] lpRect ���ڽ��մ��ڵľ�����Ϣ��
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetRectInParent(__out LPRECT lpRect);

    /**
     * ��ȡ���ڵľ����
     * @return ��� LWnd ���������ӵĴ��ھ������Ч�����򷵻������������򷵻� NULL��
     */
    HWND GetSafeHWND(void) const;

    BOOL GetScrollInfo(__in int nBar, __inout LPSCROLLINFO lpsi);

    /**
     * ��ȡ���ڵĳߴ磬�����ǿͻ����������
     * @param [out] size ���ڽ��մ��ڵĳߴ���Ϣ��
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetSize(__out LPSIZE size);

    /**
     * ��ȡ���ڵ���ʽ��
     * @return ���ڵ���ʽ��
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
     * ��ȡ���ڵ��ı���
     * @param [out] pStr ���ڽ��մ����ı��� LString ָ�롣
     * @return ����ɹ��򷵻� pStr �ĳ��ȣ����򷵻� 0��
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
     * ˢ�´��ڡ�
     * @param [in] bErase �Ƿ����������
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
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
     * ���ô��ڵ����塣
     * @param [in] hFont Ҫ���õ���������
     * @param [in] bRedraw �Ƿ��ػ洰�ڡ�
     */
    void SetFont(__in HFONT hFont, __in BOOL bRedraw = TRUE);

    BOOL SetForegroundWindow(void);

    /**
     * ���ô��ڵ�ͼ�ꡣ
     * @param [in] hIcon Ҫ���õ�ͼ������
     * @param [in] bBigIcon �Ƿ��ͼ�ꡣ
     * @return ����ͼ��ǰ��ͼ������
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
     * ���մ��ڵ��ı����ݸı䴰�ڵĴ�С��
     * @param [in] bRedraw �Ƿ��ػ洰�ڡ�
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL SizeToContent(__in BOOL bRedraw = TRUE);

    BOOL ShowWindow(__in int nCmdShow);
    BOOL UpdateWindow(void);
protected:
    /**
     * ���ڵľ��
     */
    HWND m_hWnd;
};


/**
 * \class LMsgWnd
 * \brief PDL ������Ϣ��
 * \details LMsgWnd �� PDL �����ڴ�������Ϣ���࣬����װ�˴󲿷ֳ��õĴ�����Ϣ��������
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
     * ʹ��һ�� LMsgWnd ���໯һ�����ڡ�
     * @param [in] hWnd Ҫ���໯�Ĵ��ھ����
     * @param [in] proc Ҫ�滻�Ĵ��ڹ��̡�
     * @return ����ԭ�еĴ��ڹ��̡�
     */
    WNDPROC Attach(__in HWND hWnd, __in WNDPROC proc);

    /**
     * ��ȡ LMsgWnd �������ڵľ����
     * @return LMsgWnd �������ڵľ����
     * \note ��������ᱻ LMsgWnd ���ڲ����ã����Խ��б�Ҫ�Ĳ������κμ̳��� LMsgWnd �Ĵ����඼����ʵ�����������PDL_DECLARE_WINCLASS/PDL_DEFINE_WINCLASS ����Լ���һ������
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
     * ������Ӧ WM_PDL_GETNOTIFY
     * \sa WM_PDL_GETNOTIFY
     */
    virtual LRESULT OnGetPDLNotify(UINT nType);

    /**
     * ������Ӧ WM_PDL_GETOBJECTA
     * \sa WM_PDL_GETOBJECTA
     */
    virtual PVOID OnGetPDLObject(PSTR lpClassName, DWORD dwSize);

    /**
     * ������Ӧ WM_PDL_GETOBJECTW
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
     * ��Ϣ������Ϻ��֪ͨ������
     * @param [in] uMsg ���������Ϣ��
     * @param [in] wParam ��Ϣ�ĸ��Ӳ�����
     * @param [in] lParam ��Ϣ�ĸ��Ӳ�����
     * @param [in] lRet ��Ϣ����ķ���ֵ��
     */
    virtual void OnMsgProcceded(UINT uMsg, WPARAM wParam, LPARAM lParam,
        LRESULT lRet);
protected:
    /**
     * �������໯�� Thunk ����ָ��
     */
    LThunk* m_thunk;
};

/**
 * \class LSubclassWnd
 * \brief PDL ���໯��
 * \details LSubclassWnd �� PDL ���������໯���ڵ��ࡣ
 */

class PDL_NO_VTABLE LSubclassWnd : public LMsgWnd
{
protected:
    /**
     * ���캯��
     */
    LSubclassWnd(void);
public:

    /**
     * ���໯ָ���Ĵ��ڣ����� LSubclassWnd ��������Ŀ�괰���ϡ�
     * @param [in] hWnd Ҫ���໯�Ĵ��ڡ�
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL SubclassWindow(__in HWND hWnd);

protected:

    /**
     * �ڲ��ص�������
     */
    static LRESULT CALLBACK WindowProc(LSubclassWnd* This, UINT uMsg,
        WPARAM wParam, LPARAM lParam);

protected:
    /**
     * ���໯ǰ�Ĵ��ڹ���
     */
    WNDPROC m_pfnWndProc;
};

/**
 * \class LWindow
 * \brief PDL �������ڷ�װ��
 * \details LWindow �� PDL ��������Ĵ��ڷ�װ�࣬����װ�˴󲿷ֵĴ��ڲ�������Ӧ��
 */

class LWindow : public LWnd, public LMsgWnd
{
    PDL_DECLARE_WINCLASS(LWindow)
public:
    LWindow(void);

    /**
     * ���캯��
     * @param [in] hWnd һ����Ч�Ĵ��ھ����
     */
    LWindow(__in HWND hWnd);

    /**
     * ���캯��
     * @param [in] wc Ҫע��Ĵ�������Ϣ��
     * \note wc ֮�е� lpfnWndProc �ᱻ�Զ��滻Ϊ StartWndProc��
     */
    LWindow(__in LPWNDCLASSA wc);

    /**
     * ���캯��
     * @param [in] wc Ҫע��Ĵ�������Ϣ��
     * \note wc ֮�е� lpfnWndProc �ᱻ�Զ��滻Ϊ StartWndProc��
     */
    LWindow(__in LPWNDCLASSW wc);

public:

    /**
     * �� LWindow ��������Ŀ�괰���ϡ�
     * @param [in] hWnd һ����Ч�Ĵ��ھ����
     */
    void Attach(__in HWND hWnd);

    /**
     * ����һ�����ڡ�
     * @param [in] lpClassName Ҫ�����Ĵ����ࡣ
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] lpRect ������ռ���Ρ��˲���Ϊ��ʱ������ CW_USEDEFAULT ��Ϊ���ڵ����������
     * @param [in] hWndParent �����ڡ�
     * @param [in] nID ���ڵ� ID��
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in PCSTR lpClassName, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] lpClassName Ҫ�����Ĵ����࣬���Ϊ NULL ��ʹ�� m_atom��
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] lpRect ������ռ���Ρ��˲���Ϊ��ʱ������ CW_USEDEFAULT ��Ϊ���ڵ����������
     * @param [in] hWndParent �����ڡ�
     * @param [in] nID ���ڵ� ID��
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in PCWSTR lpClassName, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] lpClassName Ҫ�����Ĵ����࣬���Ϊ NULL ��ʹ�� m_atom��
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] x ���ڵĺ����ꡣ
     * @param [in] y ���ڵ������ꡣ
     * @param [in] nWidth ���ڵĿ�ȡ�
     * @param [in] nHeight ���ڵĸ߶ȡ�
     * @param [in] hWndParent �����ڡ�
     * @param [in] hMenu ���ڵĲ˵���
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in PCSTR lpClassName, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in int x, __in int y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in HMENU hMenu, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] lpClassName Ҫ�����Ĵ����࣬���Ϊ NULL ��ʹ�� m_atom��
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] x ���ڵĺ����ꡣ
     * @param [in] y ���ڵ������ꡣ
     * @param [in] nWidth ���ڵĿ�ȡ�
     * @param [in] nHeight ���ڵĸ߶ȡ�
     * @param [in] hWndParent �����ڡ�
     * @param [in] hMenu ���ڵĲ˵���
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in PCWSTR lpClassName, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in int x, __in int y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in HMENU hMenu, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] dwExStyle ���ڵĸ�����ʽ��
     * @param [in] lpClassName Ҫ�����Ĵ����࣬���Ϊ NULL ��ʹ�� m_atom��
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] lpRect ������ռ���Ρ��˲���Ϊ��ʱ������ CW_USEDEFAULT ��Ϊ���ڵ����������
     * @param [in] hWndParent �����ڡ�
     * @param [in] nID ���ڵ� ID��
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpClassName,
        __in PCSTR lpWindowName, __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] dwExStyle ���ڵĸ�����ʽ��
     * @param [in] lpClassName Ҫ�����Ĵ����࣬���Ϊ NULL ��ʹ�� m_atom��
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] lpRect ������ռ���Ρ��˲���Ϊ��ʱ������ CW_USEDEFAULT ��Ϊ���ڵ����������
     * @param [in] hWndParent �����ڡ�
     * @param [in] nID ���ڵ� ID��
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpClassName,
        __in PCWSTR lpWindowName, __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] dwExStyle ���ڵĸ�����ʽ��
     * @param [in] lpClassName Ҫ�����Ĵ����࣬���Ϊ NULL ��ʹ�� m_atom��
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] X ���ڵĺ����ꡣ
     * @param [in] Y ���ڵ������ꡣ
     * @param [in] nWidth ���ڵĿ�ȡ�
     * @param [in] nHeight ���ڵĸ߶ȡ�
     * @param [in] hWndParent �����ڡ�
     * @param [in] hMenu ���ڵĲ˵���
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpClassName,
        __in PCSTR lpWindowName, __in DWORD dwStyle, __in int X, __in int Y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in HMENU hMenu, __in PVOID lpParam);

    /**
     * ����һ�����ڡ�
     * @param [in] dwExStyle ���ڵĸ�����ʽ��
     * @param [in] lpClassName Ҫ�����Ĵ����࣬���Ϊ NULL ��ʹ�� m_atom��
     * @param [in] lpWindowName Ҫ�����Ĵ��ڱ��⡣
     * @param [in] dwStyle Ҫ�����Ĵ�����ʽ��
     * @param [in] X ���ڵĺ����ꡣ
     * @param [in] Y ���ڵ������ꡣ
     * @param [in] nWidth ���ڵĿ�ȡ�
     * @param [in] nHeight ���ڵĸ߶ȡ�
     * @param [in] hWndParent �����ڡ�
     * @param [in] hMenu ���ڵĲ˵���
     * @param [in] lpParam ���ڵĸ��Ӳ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpClassName,
        __in PCWSTR lpWindowName, __in DWORD dwStyle, __in int X, __in int Y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in HMENU hMenu, __in PVOID lpParam);

    /**
     * ע��һ�������ࡣ
     * @param [in] wc Ҫע��Ĵ�������Ϣ��
     * @return ���ע��ɹ��򷵻� TRUE�����򷵻� FALSE��
     * \note wc ֮�е� lpfnWndProc �ᱻ�Զ��滻Ϊ StartWndProc��
     */
    BOOL Register(__in LPWNDCLASSA wc);

    /**
     * ע��һ�������ࡣ
     * @param [in] wc Ҫע��Ĵ�������Ϣ��
     * @return ���ע��ɹ��򷵻� TRUE�����򷵻� FALSE��
     * \note wc ֮�е� lpfnWndProc �ᱻ�Զ��滻Ϊ StartWndProc��
     */
    BOOL Register(__in LPWNDCLASSW wc);

protected:
    /**
     * ��ʼ�Ĵ��ڹ��̡�
     */
    static LRESULT CALLBACK StartWndProc(HWND hWnd, UINT uMsg,
        WPARAM wParam, LPARAM lParam);
    /**
     * ���໯��Ĵ��ڹ��̡�
     */
    static LRESULT CALLBACK WindowProc(LWindow* This, UINT uMsg,
        WPARAM wParam, LPARAM lParam);
protected:
    /**
     * ������� ATOM
     */
    ATOM m_atom;
    /**
     * ���໯ǰ�Ĵ��ڹ���
     */
    WNDPROC m_pfnWndProc;
};

/**
 * \class LDialog
 * \brief PDL �����Ի����װ��
 * \details LDialog �� PDL ��������ĶԻ����װ�࣬����װ�˶Ի���Ļ�����������Ӧ��
 */

class LIniParser;
class LDialog : public LWnd, public LMsgWnd
{
    PDL_DECLARE_WINCLASS(LDialog)
public:

    /**
     * ���캯��
     * @param [in] uIDDialog �Ի������Դ ID��
     * @param [in] lang ini ��ʽ�������ļ���
     */
    LDialog(__in UINT uIDDialog, __in LIniParser* lang = NULL);

public:
    /**
     * ������ģ̬�Ի���
     * @param [in] hParent �����ھ����
     * @param [in] lParam ���Ӳ�����
     * @return ��������ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Create(__in HWND hParent = ::GetActiveWindow(),
        __in LPARAM lParam = 0);

    /**
     * ����ģ̬�Ի���
     * @param [in] hParent �����ھ����
     * @param [in] lParam ���Ӳ�����
     * @return �Ի�������ķ���ֵ��
     */
    int DoModal(__in HWND hParent = ::GetActiveWindow(),
        __in LPARAM lParam = 0);

    BOOL EndDialog(__in INT_PTR nResult);

    /**
     * ����ӵ�н���Ŀؼ���
     * @param [in] hCtrl ��Ҫӵ�н���ؼ��ľ����
     */
    void SetFocusCtrl(__in HWND hCtrl);

    /**
     * ���öԻ�������塣
     * @param [in] hFont Ҫ���õ����塣
     * @param [in] bAllCtrls �Ƿ��������пؼ������塣
     * @param [in] bRedraw �Ƿ��ػ洰�ڡ�
     */
    void SetFont(__in HFONT hFont, __in BOOL bAllCtrls = TRUE,
        __in BOOL bRedraw = TRUE);

protected:
    /**
     * ��ʼ�Ĵ��ڹ��̡�
     */
    static INT_PTR CALLBACK StartDlgProc(HWND hDlg, UINT uMsg,
        WPARAM wParam, LPARAM lParam);
    /**
     * ���໯��Ĵ��ڹ��̡�
     */
    static INT_PTR CALLBACK DialogProc(LDialog* This, UINT uMsg,
        WPARAM wParam, LPARAM lParam);
protected:
    /**
     * ���������ļ���
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL LoadLanguageRes(void);
protected:
    /**
     * �Ի������Դ ID
     */
    UINT m_uId;
    /**
     * �����ļ�
     */
    LIniParser* m_lang;
};

/**
 * \class LDrawItem
 * \brief PDL �Ի渨����
 * \details LDrawItem �� PDL �ж� WM_DRAWITEM �ķ�װ�࣬����װ�˶� WM_DRAWITEM ���ִ������Ӧ��
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
 * \brief PDL �Ի渨����
 * \details LCustomDraw �� PDL �ж� NM_CUSTOMDRAW �ķ�װ�࣬����װ�˶� NM_CUSTOMDRAW ���ִ������Ӧ��
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
 * \brief PDL ֪ͨ������
 * \details LNotify ��װ�˶� WM_COMMAND �� WM_NOTIFY ��֪ͨ����
 * \sa PDL_ENABLE_NOTIFY
 */

class LNotify
{
    friend class LMsgWnd;
protected:
    virtual void OnCmdNotify(WORD id, WORD wCode, HWND hCtrl, BOOL& bHandled);
    virtual LRESULT OnMsgNotify(int id, LPNMHDR nmh, BOOL& bHandled);
};
