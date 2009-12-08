/**
 * \file pdl_commctrl.h
 * \brief PDL 公共控件封装
 * \details 对公共控件的功能封装：
 *   \li \c LComCtlInit 公共控件初始化类
 *   \li \c LComCtl 公共控件基类
 *   \li \c LDateTime 日期/时间控件类
 *   \li \c LHeader Header 控件类
 *   \li \c LHotKey 热键控件类
 *   \li \c LImageList 图像列表类
 *   \li \c LListView 列表视图类
 *   \li \c LMonthCal 日历控件类
 *   \li \c LProgressBar 进度条控件类
 *   \li \c LPropSheet 属性表控件类
 *   \li \c LPropSheetPage 属性页控件类
 *   \li \c LReBar ReBar 控件类
 *   \li \c LStatusBar 状态栏控件类
 *   \li \c LTabCtrl 标签控件类
 *   \li \c LToolBar 工具栏控件类
 *   \li \c LToolTip 工具提示控件类
 *   \li \c LTrackBar 滑动条控件类
 *   \li \c LTreeView 树形视图类
 *   \li \c LUpDown 微调控件类
 */

#pragma once

#include "pdl_window.h"

/**
 * \class LComCtlInit
 * \brief 公共控件初始化类
 */

class LComCtlInit
{
public:
    LComCtlInit(__in DWORD dwICC)
    {
        INITCOMMONCONTROLSEX init;
        init.dwSize = sizeof(INITCOMMONCONTROLSEX);
        init.dwICC = dwICC;

        ::InitCommonControlsEx(&init);
    }
};

/**
 * \class LComCtl
 * \brief 公共控件基类
 */

class LComCtl : public LWnd
{
public:
    LComCtl(__in HWND hWnd = NULL);
    LComCtl& operator=(__in HWND hWnd);
public:
    BOOL GetUnicodeFormat(void);
    DWORD GetVersion(void);
    BOOL SetUnicodeFormat(__in BOOL bUnicode);
    DWORD SetVersion(__in DWORD dwVersion);
};

/**
 * \class LDateTime
 * \brief 日期/时间控件类
 */

class LDateTime : public LComCtl
{
public:
    LDateTime(__in HWND hWnd = NULL);
    LDateTime& operator=(__in HWND hWnd);
public:
    DWORD GetTime(__in LPSYSTEMTIME lpSysTime);
    BOOL SetTime(__in DWORD dwFlag, __in LPSYSTEMTIME lpSysTime);
};

/**
 * \class LHeader
 * \brief Header 控件类
 */

class LHeader : public LComCtl
{
public:
    BOOL Create(__in HWND hParent, __in UINT uId, __in DWORD dwStyle);
    int InsertItem(__in int index, __in const LPHDITEMA phdi);
    int InsertItem(__in int index, __in const LPHDITEMW phdi);
    int InsertItem(__in int index, __in PCSTR pszText, __in int cxy,
        __in int fmt = HDF_LEFT);
    int InsertItem(__in int index, __in PCWSTR pszText, __in int cxy,
        __in int fmt = HDF_LEFT);
    BOOL Layout(__in LPRECT prc, __out LPWINDOWPOS pwpos);
};

/**
 * \class LHotKey
 * \brief 热键控件类
 */

class LHotKey : public LComCtl
{
public:
    LHotKey(__in HWND hWnd = NULL);
    LHotKey& operator=(__in HWND hWnd);
public:
    UINT GetHotKey(void);
    void GetHotKey(__out PUINT fsModifiers, __out PUINT vk);
    static UINT MakeHotKey(__in UINT fsModifiers, __in UINT vk);
    static void ParseHotKey(__in UINT uHotKey, __out PUINT fsModifiers,
        __out PUINT vk);
    void SetHotKey(__in UINT uHotKey);
    void SetHotKey(__in UINT fsModifiers, __in UINT vk);
};

/**
 * \class LImageList
 * \brief 图像列表类
 */

class LImageList
{
public:
    LImageList(__in HIMAGELIST himl = NULL);
    ~LImageList(void);
    LImageList& operator=(__in HIMAGELIST himl);
    operator HIMAGELIST(void) const;
public:
    int Add(__in HBITMAP hbmImage, __in HBITMAP hbmMask);
    int AddIcon(__in HICON hIcon);
    int AddMasked(__in HBITMAP hbmImage, __in COLORREF crMask);
    BOOL BeginDrag(__in int iTrack, __in int dxHotspot, __in int dyHotspot);
    BOOL Copy(__in HIMAGELIST himlSrc, __in int iDst, __in int iSrc,
        __in UINT uFlags);
    BOOL Create(__in int cx, __in int cy, __in UINT flags,
        __in int cInitial, __in int cGrow);
    BOOL Destroy(void);
    BOOL DragEnter(__in HWND hwndLock, __in int x, __in int y);
    BOOL DragLeave(__in HWND hwndLock);
    BOOL DragMove(__in int x, __in int y);
    BOOL DragShowNolock(__in BOOL fShow);
    BOOL Draw(__in int i, __in HDC hdcDst, __in int x,
        __in int y, __in UINT fStyle);
    BOOL DrawEx(int i, HDC hdcDst, int x, int y, int dx, int dy,
        COLORREF rgbBk, COLORREF rgbFg, UINT fStyle);
    BOOL DrawIndirect(IMAGELISTDRAWPARAMS *pimldp);
    HIMAGELIST Duplicate(void);
    void EndDrag();
    HICON ExtractIcon(int i);
    COLORREF GetBkColor() const;
    HIMAGELIST GetHandle(void) { return m_hImageList; }
    HICON GetIcon(int i, UINT flags);
    BOOL GetIconSize(int *cx, int *cy);
    int GetImageCount(void) const;
    BOOL GetImageInfo(int i, LPIMAGEINFO pImageInfo);
    static HIMAGELIST GetShellImageList(__in BOOL bLarge);
    static HIMAGELIST LoadFromFile(__in PCSTR lpszFileName, __in int cx,
        __in COLORREF crMask);
    static HIMAGELIST LoadFromFile(__in PCWSTR lpszFileName, __in int cx,
        __in COLORREF crMask);
    BOOL LoadImage(__in PCSTR lpbmp, __in int cx, __in COLORREF crMask,
        __in UINT flags, __in int cGrow);
    BOOL LoadImage(__in PCWSTR lpbmp, __in int cx, __in COLORREF crMask,
        __in UINT flags, __in int cGrow);
    BOOL Merge(HIMAGELIST himl1, int i1, HIMAGELIST himl2, int i2, int dx,
        int dy);
    BOOL Remove(__in int i);
    BOOL RemoveAll(void);
    int ReplaceIcon(__in int i, __in HICON hicon);
    COLORREF SetBkColor(COLORREF clrBk);
    BOOL SetDragCursorImage(int iDrag, int dxHotspot, int dyHotspot);
    BOOL SetIconSize(int cx, int cy);
    BOOL SetImageCount(UINT uNewCount);
    BOOL SetOverlayImage(int iImage, int iOverlay);
protected:
    /**
     * 图像列表句柄
     */
    HIMAGELIST m_hImageList;
};

/**
 * \class LListView
 * \brief 列表视图类
 */

class LListView : public LComCtl
{
public:
    LListView& operator=(__in HWND hWnd);
public:
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
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
        __in DWORD dwStyle, __in int X, __in int Y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in int X, __in int Y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in UINT nID);
    BOOL DeleteAllItems(void);
    BOOL DeleteColumn(__in int iCol);
    BOOL DeleteItem(__in int nItem);
    HWND EditLabel(__in int iItem);
    BOOL GetCheckState(__in UINT iIndex);
    BOOL GetColumn(__in int iCol, __out LPLVCOLUMNA pcol);
    BOOL GetColumn(__in int iCol, __out LPLVCOLUMNW pcol);
    int GetColumnWidth(__in int iCol);
    HWND GetEditControl(void);
    HIMAGELIST GetImageList(__in int iImageList);
    BOOL GetItem(__inout LPLVITEMA pitem);
    BOOL GetItem(__inout LPLVITEMW pitem);
    int GetItemCount(void) const;
    BOOL GetItemRect(__in int iItem, __out LPRECT rc, __in int code);
    UINT GetItemState(__in int iItem, __in UINT mask);
    void GetItemText(__in int iItem, __in int iSubItem, __out PSTR pszText,
        __in int cchTextMax);
    void GetItemText(__in int iItem, __in int iSubItem, __out PWSTR pszText,
        __in int cchTextMax);
    int GetNextItem(__in int iStart, __in UINT flags);
    UINT GetSelectedCount(void);
    int GetSelectionMark(void);
    int HitTest(__inout LPLVHITTESTINFO hi);
    int InsertColumn(__in int iCol, __in PCSTR pszText, __in int cx,
        __in int fmt = LVCFMT_LEFT);
    int InsertColumn(__in int iCol, __in PCWSTR pszText, __in int cx,
        __in int fmt = LVCFMT_LEFT);
    int InsertColumn(__in int iCol, __in const LPLVCOLUMNA pcol);
    int InsertColumn(__in int iCol, __in const LPLVCOLUMNW pcol);
    int InsertItem(__in int iItem, __in PCSTR pszText, __in int iImage,
        __in LPARAM lParam);
    int InsertItem(__in int iItem, __in PCWSTR pszText, __in int iImage,
        __in LPARAM lParam);
    int InsertItem(__in const LPLVITEMA pitem);
    int InsertItem(__in const LPLVITEMW pitem);
    BOOL SetCallbackMask(__in UINT mask);
    void SetCheckState(__in UINT iIndex, __in BOOL fCheck);
    BOOL SetColumnWidth(__in int iCol, __in int cx);
    void SetExtendedListViewStyle(__in DWORD dwExMask, __in DWORD dwExStyle);
    HIMAGELIST SetImageList(__in HIMAGELIST hImageList, __in int iImageList);
    BOOL SetItem(const LPLVITEMA pitem);
    BOOL SetItem(const LPLVITEMW pitem);
    void SetItemState(__in int i, __in UINT state, __in UINT mask);
    void SetItemText(__in int i, __in int iSubItem, __in PCSTR pszText);
    void SetItemText(__in int i, __in int iSubItem, __in PCWSTR pszText);
    int SetSelectionMark(__in int iIndex);
    BOOL SortItems(__in PFNLVCOMPARE pfnCompare, __in LPARAM lParamSort);
    BOOL Update(__in int iItem);
};

/**
 * \class LMonthCal
 * \brief 日历控件类
 */

class LMonthCal : public LComCtl
{
public:
    LMonthCal(__in HWND hWnd = NULL);
    LMonthCal& operator=(__in HWND hWnd);
public:
    BOOL GetCurSel(__in LPSYSTEMTIME lpSysTime);
};

/**
 * \class LProgressBar
 * \brief 进度条控件类
 */

class LProgressBar : public LComCtl
{
public:
    LProgressBar& operator=(__in HWND hWnd);
public:
    int GetStep(void);
    int SetPos(__in int nPos);
    void SetRange32(__in int nLower, __in int nUpper);
    int SetStep(__in int nStepInc);
    int StepIt(void);
};

/**
 * \class LPropSheet
 * \brief 属性表控件类
 */

class LPropSheet;
class LPropSheetPage : protected LDialog
{
    friend class LPropSheet;
public:
    LPropSheetPage(__in UINT idPage);
    ~LPropSheetPage(void);
public:
    BOOL Create(__in DWORD dwFlags, __in PCSTR pszTitle, __in LPARAM lParam,
        __in LPFNPSPCALLBACKA pfnCallback);
    BOOL Create(__in DWORD dwFlags, __in PCWSTR pszTitle, __in LPARAM lParam,
        __in LPFNPSPCALLBACKW pfnCallback);
protected:
    LPropSheet* GetParentSheet(void);
private:
    HPROPSHEETPAGE m_hPropPage;
    LPropSheet* m_pParent;
};

/**
 * \class LPropSheetPage
 * \brief 属性页控件类
 */

typedef struct ThunkPS *PTHUNKPS;

#define PROPSHEET_CENTERWINDOW  0x00000001

class LPropSheet : public LComCtl
{
public:
    LPropSheet(__in int nMaxCnt = 1);
    ~LPropSheet(void);
public:
    BOOL AddPage(__in LPropSheetPage* page);
    BOOL AddPage(__in HPROPSHEETPAGE hPage);
    int DoModal(__in HWND hParent, __in PCSTR pszCaption);
    int DoModal(__in HWND hParent, __in PCWSTR pszCaption);
    DWORD SetFlags(__in DWORD dwNewFlags);
    void SetWizButtons(__in DWORD dwFlags);
    void ShowWizButtons(__in DWORD dwFlag, __in DWORD dwButton);
protected:
    virtual void OnMessage(UINT uMsg, LPARAM lParam);
private:
    void InitThunk(void);
    static int CALLBACK StartProc(LPropSheet* pThis, UINT uMsg, LPARAM lParam);
protected:
    DWORD m_dwFlags;
    HPROPSHEETPAGE* m_hPages;
    int m_nPageCnt;
    int m_nMaxCnt;
private:
    PTHUNKPS m_thunk;
};

/**
 * \class LReBar
 * \brief ReBar 控件类
 */

class LReBar : public LComCtl
{
public:
    BOOL Create(__in DWORD dwStyle, __in HWND hWndParent, __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in DWORD dwStyle,
        __in HWND hWndParent, __in UINT nID);
    static int SizeOfREBARBANDINFO(void);
    BOOL InsertBand(__in UINT ulIndex, __in LPREBARBANDINFOA lpRbbi);
    BOOL InsertBand(__in UINT ulIndex, __in LPREBARBANDINFOW lpRbbi);
    BOOL SetBarInfo(__in LPREBARINFO lpRbi);
};

/**
 * \class LStatusBar
 * \brief 状态栏控件类
 */

class LStatusBar : public LComCtl
{
public:
    BOOL Create(__in HWND hParent, __in PCSTR lpszWindowName,
        __in DWORD dwStyle, __in UINT nID);
    BOOL Create(__in HWND hParent, __in PCWSTR lpszWindowName,
        __in DWORD dwStyle, __in UINT nID);
    BOOL GetRect(__in int nPart, __out LPRECT lprc);
    BOOL SetParts(__in int nParts, __in LPINT aWidths);
    BOOL SetText(__in int nPart, __in PCSTR lpszText);
    BOOL SetText(__in int nPart, __in PCWSTR lpszText);
};

/**
 * \class LTabCtrl
 * \brief 标签控件类
 */

class LTabCtrl : public LComCtl
{
public:
    LTabCtrl(__in HWND hWnd = NULL);
    LTabCtrl& operator=(__in HWND hWnd);
    operator HWND(void) const;
public:
    void AdjustRect(__in BOOL fLarger, __inout LPRECT prc);
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
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
        __in DWORD dwStyle, __in int X, __in int Y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in int X, __in int Y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in UINT nID);
    int GetCurSel(void);
    int InsertItem(__in int iItem, __in const LPTCITEMA pitem);
    int InsertItem(__in int iItem, __in const LPTCITEMW pitem);
    int InsertItem(__in int iItem, __in PCSTR lpszItem,
        __in int nImage = -1, __in LPARAM lParam = 0);
    int InsertItem(__in int iItem, __in PCWSTR lpszItem,
        __in int nImage = -1, __in LPARAM lParam = 0);
    HIMAGELIST SetImageList(__in HIMAGELIST himl);
};

/**
 * \class LToolBar
 * \brief 工具栏控件类
 */

#ifdef UNICODE
#define TbUnicode   TRUE
#else // !UNICODE
#define TbUnicode   FALSE
#endif // UNICODE

class LToolBar : public LComCtl
{
public:
    LToolBar(__in HWND hWnd = NULL);
public:
    BOOL Create(__in DWORD dwStyle, __in HWND hWndParent, __in UINT nID);
    BOOL AddButtons(__in UINT uNumButtons, __in LPCTBBUTTON lpButtons,
        __in BOOL bUnicode = TbUnicode);
    int AddString(__in PCSTR lpString);
    int AddString(__in PCWSTR lpString);
    void AutoSize(void);
    void ButtonStructSize(__in int cb);
    BOOL EnableButton(__in int idButton, __in BOOL fEnable);
    HIMAGELIST SetImageList(__in HIMAGELIST himl);
};

/**
 * \class LToolTip
 * \brief 工具提示控件类
 */

class LToolTip : public LComCtl
{
public:
    LToolTip(__in HWND hWnd = NULL);
public:
    BOOL Create(__in DWORD dwStyle, __in HWND hWndParent);
    void Activate(__in BOOL fActivate = TRUE);
    BOOL AddTool(__in LPTOOLINFOA pti);
    BOOL AddTool(__in LPTOOLINFOW pti);
    void DelTool(__in HWND hwnd, __in UINT uId);
    void Pop(void);
    void Popup(void);
    void SetToolInfo(__in LPTOOLINFOA pti);
    void SetToolInfo(__in LPTOOLINFOW pti);
};

/**
 * \class LTrackBar
 * \brief 滑动条控件类
 */

class LTrackBar : public LComCtl
{
public:
    LTrackBar& operator=(__in HWND hWnd);
public:
    void SetRange(__in int nMin, __in int nMax,
        __in BOOL bRedraw = FALSE);
};

/**
 * \class LTreeView
 * \brief 树形视图类
 */

class LTreeView : public LComCtl
{
public:
    LTreeView(__in HWND hWnd = NULL);
    LTreeView& operator=(__in HWND hWnd);
    operator HWND(void) const;
public:
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in int x, __in int y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in UINT id);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in int x, __in int y, __in int nWidth, __in int nHeight,
        __in HWND hWndParent, __in UINT id);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect, __in HWND hWndParent,
        __in UINT nID);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in int X, __in int Y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in UINT id);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in int X, __in int Y,
        __in int nWidth, __in int nHeight, __in HWND hWndParent,
        __in UINT id);
    BOOL DeleteAllItems(void);
    BOOL DeleteItem(__in HTREEITEM hItem);
    HWND EditLabel(__in HTREEITEM hItem);
    BOOL Expand(__in HTREEITEM hItem, __in UINT flag);
    void ExpandAll(__in HTREEITEM hItem);
    HTREEITEM GetDropHilight(void);
    HWND GetEditControl(void);
    BOOL GetItem(__inout LPTVITEMA pitem);
    BOOL GetItem(__inout LPTVITEMW pitem);
    BOOL GetItemRect(__in HTREEITEM hitem, __out LPRECT prc,
        __in BOOL fItemRect);
    UINT GetItemState(__in HTREEITEM hItem, __in UINT stateMask);
    HTREEITEM GetNextItem(__in HTREEITEM hItem, __in UINT uFlag);
    HTREEITEM GetSelection(void);
    HTREEITEM HitTest(__inout LPTVHITTESTINFO lpht);
    HTREEITEM InsertItem(__in HTREEITEM hParent, __in HTREEITEM hInsertAfter,
        __in PCSTR lpszItem, __in int nImage = -1, __in int nSelImage = -1,
        __in LPARAM lParam = 0);
    HTREEITEM InsertItem(__in HTREEITEM hParent, __in HTREEITEM hInsertAfter,
        __in PCWSTR lpszItem, __in int nImage = -1, __in int nSelImage = -1,
        __in LPARAM lParam = 0);
    HTREEITEM InsertItem(__in LPTVINSERTSTRUCTA lpis);
    HTREEITEM InsertItem(__in LPTVINSERTSTRUCTW lpis);
    BOOL SelectItem(__in HTREEITEM hItem);
    HIMAGELIST SetImageList(__in HIMAGELIST himl, __in int iImage);
    BOOL SetItem(__in LPTVITEMA pItem);
    BOOL SetItem(__in LPTVITEMW pItem);
    BOOL SortChildrenCB(__in LPTVSORTCB psort, BOOL fRecurse);
protected:
    void SetChildState(__in HTREEITEM hParent, __in BOOL bHasChild);
};

/**
 * \class LUpDown
 * \brief 微调控件类
 */

class LUpDown : public LComCtl
{
public:
    LUpDown(__in HWND hWnd = NULL);
    LUpDown& operator=(__in HWND hWnd);
public:
    HWND SetBuddy(__in HWND hwndBuddy);
    short SetPos(__in short nPos);
    int SetPos32(__in int nPos);
    void SetRange(__in short nLower, __in short nUpper );
    void SetRange32(__in int iLow, __in int iHigh);
};
