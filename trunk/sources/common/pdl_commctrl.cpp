#include "..\..\include\pdl_commctrl.h"
#include "..\..\include\pdl_gdi.h"
#include "..\..\include\pdl_module.h"

///////////////////////////////////////////////////////////////////////////////
// LDateTime

LDateTime::LDateTime(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
}

LDateTime& LDateTime::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

DWORD LDateTime::GetTime(__in LPSYSTEMTIME lpSysTime)
{
    return SendMessage(DTM_GETSYSTEMTIME, 0, (LPARAM)lpSysTime);
}

BOOL LDateTime::SetTime(__in DWORD dwFlag, __in LPSYSTEMTIME lpSysTime)
{
    return (BOOL)SendMessage(DTM_SETSYSTEMTIME, dwFlag, (LPARAM)lpSysTime);
}

///////////////////////////////////////////////////////////////////////////////
// LHeader

BOOL LHeader::Create(__in HWND hParent, __in UINT uId, __in DWORD dwStyle)
{
    return LWnd::Create(WC_HEADER, (PCTSTR)NULL, dwStyle | WS_CHILD, NULL,
        hParent, uId, NULL);
}

int LHeader::InsertItem(__in int index, __in const LPHDITEMA phdi)
{
    return (int)SendMessage(HDM_INSERTITEMA, index, (LPARAM)phdi);
}

int LHeader::InsertItem(__in int index, __in const LPHDITEMW phdi)
{
    return (int)SendMessage(HDM_INSERTITEMW, index, (LPARAM)phdi);
}

int LHeader::InsertItem(
    __in int index,
    __in PCSTR pszText,
    __in int cxy,
    __in int fmt /* = HDF_LEFT */)
{
    HDITEMA hdi;
    RtlZeroMemory(&hdi, sizeof(HDITEMA));
    hdi.mask = HDI_FORMAT | HDI_LPARAM | HDI_TEXT | HDI_WIDTH;
    hdi.pszText = (PSTR)pszText;
    hdi.cxy = cxy;
    hdi.fmt = fmt;

    return InsertItem(index, &hdi);
}

int LHeader::InsertItem(
    __in int index,
    __in PCWSTR pszText,
    __in int cxy,
    __in int fmt /* = HDF_LEFT */)
{
    HDITEMW hdi;
    RtlZeroMemory(&hdi, sizeof(HDITEMW));
    hdi.mask = HDI_FORMAT | HDI_LPARAM | HDI_TEXT | HDI_WIDTH;
    hdi.pszText = (PWSTR)pszText;
    hdi.cxy = cxy;
    hdi.fmt = fmt;

    return InsertItem(index, &hdi);
}

BOOL LHeader::Layout(__in LPRECT prc, __out LPWINDOWPOS pwpos)
{
    HDLAYOUT layout;
    layout.prc   = prc;
    layout.pwpos = pwpos;

    return Header_Layout(m_hWnd, &layout);
}

///////////////////////////////////////////////////////////////////////////////
// LHotKey

LHotKey::LHotKey(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
}

LHotKey& LHotKey::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

UINT LHotKey::GetHotKey(void)
{
#ifdef _WIN32_WCE
    return 0;
#else
    return static_cast<UINT>(SendMessage(HKM_GETHOTKEY));
#endif // _WIN32_WCE
}

void LHotKey::GetHotKey(__out PUINT fsModifiers, __out PUINT vk)
{
    UINT uHotKey = GetHotKey();
    *fsModifiers = HIBYTE(uHotKey);
    *vk = LOBYTE(uHotKey);
}

UINT LHotKey::MakeHotKey(__in UINT fsModifiers, __in UINT vk)
{
    return (UINT)MAKEWORD(vk, fsModifiers);
}

void LHotKey::ParseHotKey(
     __in UINT uHotKey,
     __out PUINT fsModifiers,
     __out PUINT vk)
{
    *fsModifiers = HIBYTE(uHotKey);
    *vk = LOBYTE(uHotKey);
}

void LHotKey::SetHotKey(__in UINT uHotKey)
{
#ifndef _WIN32_WCE
    SendMessage(HKM_SETHOTKEY, uHotKey);
#endif // _WIN32_WCE
}

void LHotKey::SetHotKey(__in UINT fsModifiers, __in UINT vk)
{
    SetHotKey(MakeHotKey(vk, fsModifiers));
}

//////////////////////////////////////////////////////////////////////////
// LImageList

LImageList::LImageList(__in HIMAGELIST himl /* = NULL */)
    : m_hImageList(himl)
{
}

LImageList::~LImageList(void)
{
    if (NULL != m_hImageList)
        Destroy();
}

LImageList& LImageList::operator=(__in HIMAGELIST himl)
{
    PDLASSERT(NULL != m_hImageList);

    m_hImageList = himl;
    return *this;
}

LImageList::operator HIMAGELIST(void) const
{
    return m_hImageList;
}

int LImageList::Add(__in HBITMAP hbmImage, __in HBITMAP hbmMask)
{
    return ImageList_Add(m_hImageList, hbmImage, hbmMask);
}

int LImageList::AddIcon(__in HICON hIcon)
{
    return ImageList_AddIcon(m_hImageList, hIcon);
}

int LImageList::AddMasked(__in HBITMAP hbmImage, __in COLORREF crMask)
{
    return ImageList_AddMasked(m_hImageList, hbmImage, crMask);
}

BOOL LImageList::BeginDrag(
    __in int iTrack,
    __in int dxHotspot,
    __in int dyHotspot)
{
    return ImageList_BeginDrag(m_hImageList, iTrack, dxHotspot, dyHotspot);
}

BOOL LImageList::Copy(
    __in HIMAGELIST himlSrc,
    __in int iDst, __in int iSrc,
    __in UINT uFlags)
{
    return ImageList_Copy(m_hImageList, iDst, himlSrc, iSrc, uFlags);
}

BOOL LImageList::Create(
    __in int cx, __in int cy,
    __in UINT flags,
    __in int cInitial,
    __in int cGrow)
{
    m_hImageList = ImageList_Create(cx, cy, flags, cInitial, cGrow);
    return NULL != m_hImageList;
}

BOOL LImageList::Destroy(void)
{
    BOOL bRet    = ImageList_Destroy(m_hImageList);
    m_hImageList = NULL;
    return bRet;
}

BOOL LImageList::DragEnter(__in HWND hwndLock, __in int x, __in int y)
{
    return ImageList_DragEnter(hwndLock, x, y);
}

BOOL LImageList::DragLeave(__in HWND hwndLock)
{
    return ImageList_DragLeave(hwndLock);
}

BOOL LImageList::DragMove(__in int x, __in int y)
{
    return ImageList_DragMove(x, y);
}

BOOL LImageList::Draw(
    __in int i,
    __in HDC hdcDst,
    __in int x,
    __in int y,
    __in UINT fStyle)
{
    return ImageList_Draw(m_hImageList, i, hdcDst, x, y, fStyle);
}

int LImageList::GetImageCount(void) const
{
    return ImageList_GetImageCount(m_hImageList);
}

HIMAGELIST LImageList::GetShellImageList(__in BOOL bLarge)
{
#ifdef _WIN32_WCE
    return NULL;
#else // !_WIN32_WCE
    typedef BOOL (WINAPI * GILPtr)(HIMAGELIST*, HIMAGELIST*);
    typedef BOOL (WINAPI * FIIPtr)(BOOL);
    HMODULE hShell32 = GetModuleHandle(_T("shell32.dll"));
    if (NULL == hShell32)
        return NULL;

    GILPtr Shell_GetImageLists = (GILPtr)GetProcAddress(hShell32,
        MAKEINTRESOURCEA(71));
    FIIPtr FileIconInit = (FIIPtr)GetProcAddress(hShell32,
        MAKEINTRESOURCEA(660));
    if (NULL == Shell_GetImageLists || NULL == FileIconInit)
        return NULL;

    FileIconInit(TRUE);

    HIMAGELIST hRet = NULL;
    if (bLarge)
        Shell_GetImageLists(&hRet, NULL);
    else
        Shell_GetImageLists(NULL, &hRet);

    return hRet;
#endif // _WIN32_WCE
}

HIMAGELIST LImageList::LoadFromFile(
    __in PCSTR lpszFileName,
    __in int cx,
    __in COLORREF crMask)
{
    HIMAGELIST himlRet = NULL;
    HBITMAP hBitmap = LBitmap::LoadFromFile(lpszFileName);
    if (NULL == hBitmap)
        return NULL;

    BITMAP bmp;
    GetObject(hBitmap, sizeof(BITMAP), &bmp);
#ifdef _WIN32_WCE
    himlRet = ::ImageList_Create(cx, bmp.bmHeight, ILC_MASK | ILC_COLOR, 0, 2);
#else
    himlRet = ::ImageList_Create(cx, bmp.bmHeight, ILC_MASK | ILC_COLOR24, 0, 2);
#endif
    if (NULL != himlRet)
        ImageList_AddMasked(himlRet, hBitmap, crMask);

    DeleteObject(hBitmap);
    return himlRet;
}

HIMAGELIST LImageList::LoadFromFile(
    __in PCWSTR lpszFileName,
    __in int cx,
    __in COLORREF crMask)
{
    HIMAGELIST himlRet = NULL;
    HBITMAP hBitmap = LBitmap::LoadFromFile(lpszFileName);
    if (NULL == hBitmap)
        return NULL;

    BITMAP bmp;
    GetObject(hBitmap, sizeof(BITMAP), &bmp);
#ifdef _WIN32_WCE
    himlRet = ::ImageList_Create(cx, bmp.bmHeight, ILC_MASK | ILC_COLOR, 0, 2);
#else
    himlRet = ::ImageList_Create(cx, bmp.bmHeight, ILC_MASK | ILC_COLOR24, 0, 2);
#endif
    if (NULL != himlRet)
        ImageList_AddMasked(himlRet, hBitmap, crMask);

    DeleteObject(hBitmap);
    return himlRet;
}

BOOL LImageList::LoadImage(
    __in PCSTR lpbmp,
    __in int cx,
    __in COLORREF crMask,
    __in UINT flags,
    __in int cGrow)
{
    LAppModule* theApp = LAppModule::GetApp();
    HBITMAP hBmp = theApp->LoadBitmapA(lpbmp);
    if (NULL == hBmp)
        return FALSE;

    BITMAP bmp;
    GetObject(hBmp, sizeof(BITMAP), &bmp);
    if (!Create(cx, bmp.bmHeight, flags, bmp.bmWidth / cx, cGrow))
        return FALSE;

    AddMasked(hBmp, crMask);
    ::DeleteObject(hBmp);
    return TRUE;
}

BOOL LImageList::LoadImage(
    __in PCWSTR lpbmp,
    __in int cx,
    __in COLORREF crMask,
    __in UINT flags,
    __in int cGrow)
{
    LAppModule* theApp = LAppModule::GetApp();
    HBITMAP hBmp = theApp->LoadBitmapW(lpbmp);
    if (NULL == hBmp)
        return FALSE;

    BITMAP bmp;
    GetObject(hBmp, sizeof(BITMAP), &bmp);
    if (!Create(cx, bmp.bmHeight, flags, bmp.bmWidth / cx, cGrow))
        return FALSE;

    AddMasked(hBmp, crMask);
    ::DeleteObject(hBmp);
    return TRUE;
}

BOOL LImageList::Remove(__in int i)
{
    return ImageList_Remove(m_hImageList, i);
}

BOOL LImageList::RemoveAll(void)
{
    return ::ImageList_RemoveAll(m_hImageList);
}

int LImageList::ReplaceIcon(__in int i, __in HICON hicon)
{
    return ::ImageList_ReplaceIcon(m_hImageList, i, hicon);
}

BOOL LImageList::SetOverlayImage(__in int iImage, __in int iOverlay)
{
    return ::ImageList_SetOverlayImage(m_hImageList, iImage, iOverlay);
}

//////////////////////////////////////////////////////////////////////////
// LListView

LListView& LListView::operator=(__in HWND hWnd)
{
    PDLASSERT(NULL == m_hWnd);

    m_hWnd = hWnd;
    return *this;
}

BOOL LListView::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_LISTVIEWA, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LListView::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_LISTVIEWW, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LListView::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_LISTVIEWA, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
}

BOOL LListView::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_LISTVIEWW, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
}

BOOL LListView::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_LISTVIEWA, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, lpParam);
}

BOOL LListView::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_LISTVIEWW, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, lpParam);
}

BOOL LListView::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID, __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_LISTVIEWA, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
}

BOOL LListView::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_LISTVIEWW, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, (HMENU)(UINT_PTR)nID, lpParam);
}

BOOL LListView::DeleteAllItems(void)
{
    return ListView_DeleteAllItems(m_hWnd);
}

BOOL LListView::DeleteItem(__in int nItem)
{
    return ListView_DeleteItem(m_hWnd, nItem);
}

BOOL LListView::GetCheckState(__in UINT iIndex)
{
    return ListView_GetCheckState(m_hWnd, iIndex);
}

BOOL LListView::GetColumn(__in int iCol, __out LPLVCOLUMNA pcol)
{
    return (BOOL)SendMessage(LVM_GETCOLUMNA, iCol, (LPARAM)pcol);
}

BOOL LListView::GetColumn(__in int iCol, __out LPLVCOLUMNW pcol)
{
    return (BOOL)SendMessage(LVM_GETCOLUMNW, iCol, (LPARAM)pcol);
}

int LListView::GetColumnWidth(__in int iCol)
{
    return ListView_GetColumnWidth(m_hWnd, iCol);
}

HIMAGELIST LListView::GetImageList(__in int iImageList)
{
    return ListView_GetImageList(m_hWnd, iImageList);
}

BOOL LListView::GetItem(__inout LPLVITEMA pitem)
{
    return (BOOL)SendMessage(LVM_GETITEMA, 0, (LPARAM)pitem);
}

BOOL LListView::GetItem(__inout LPLVITEMW pitem)
{
    return (BOOL)SendMessageW(LVM_GETITEMW, 0, (LPARAM)pitem);
}

int LListView::GetItemCount(void) const
{
    return ListView_GetItemCount(m_hWnd);
}

BOOL LListView::GetItemRect(__in int iItem, __out LPRECT rc, __in int code)
{
    return ListView_GetItemRect(m_hWnd, iItem, rc, code);
}

void LListView::GetItemText(
    __in int iItem,
    __in int iSubItem,
    __out PSTR pszText,
    __in int cchTextMax)
{
    LVITEMA item = { 0 };
    item.iItem = iItem;
    item.iSubItem = iSubItem;
    item.cchTextMax = cchTextMax;
    item.mask = LVIF_TEXT;
    item.pszText = pszText;

    SendMessage(LVM_GETITEMA, iItem, (LPARAM)&item);
}

void LListView::GetItemText(
    __in int iItem,
    __in int iSubItem,
    __out PWSTR pszText,
    __in int cchTextMax)
{
    LVITEMW item = { 0 };
    item.iItem = iItem;
    item.iSubItem = iSubItem;
    item.cchTextMax = cchTextMax;
    item.mask = LVIF_TEXT;
    item.pszText = pszText;

    SendMessage(LVM_GETITEMW, iItem, (LPARAM)&item);
}

int LListView::GetSelectionMark(void)
{
    return ListView_GetSelectionMark(m_hWnd);
}

int LListView::InsertColumn(
    __in int iCol,
    __in PCSTR pszText,
    __in int cx,
    __in int fmt /* = LVCFMT_LEFT */)
{
    LVCOLUMNA lvc = { 0 };

    lvc.cx = cx;
    lvc.fmt = fmt;
    lvc.iSubItem = iCol;
    lvc.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
    lvc.pszText = (PSTR)pszText;

    return InsertColumn(iCol, &lvc);
}

int LListView::InsertColumn(
    __in int iCol,
    __in PCWSTR pszText,
    __in int cx,
    __in int fmt /* = LVCFMT_LEFT */)
{
    LVCOLUMNW lvc = { 0 };

    lvc.cx = cx;
    lvc.fmt = fmt;
    lvc.iSubItem = iCol;
    lvc.mask = LVCF_FMT | LVCF_SUBITEM | LVCF_TEXT | LVCF_WIDTH;
    lvc.pszText = (PWSTR)pszText;

    return InsertColumn(iCol, &lvc);
}

int LListView::InsertColumn(__in int iCol, __in const LPLVCOLUMNA pcol)
{
    return (int)SendMessage(LVM_INSERTCOLUMNA, (WPARAM)iCol, (LPARAM)pcol);
}

int LListView::InsertColumn(__in int iCol, __in const LPLVCOLUMNW pcol)
{
    return (int)SendMessage(LVM_INSERTCOLUMNW, (WPARAM)iCol, (LPARAM)pcol);
}

int LListView::InsertItem(
    __in int iItem,
    __in PCSTR pszText,
    __in int iImage,
    __in LPARAM lParam)
{
    LVITEMA item = { 0 };

    item.iImage = iImage;
    item.iItem = iItem;
    item.lParam = lParam;
    item.pszText = (PSTR)pszText;
    item.mask = LVIF_IMAGE | LVIF_PARAM | LVIF_TEXT;

    return InsertItem(&item);
}

int LListView::InsertItem(
    __in int iItem,
    __in PCWSTR pszText,
    __in int iImage,
    __in LPARAM lParam)
{
    LVITEMW item = { 0 };

    item.iImage = iImage;
    item.iItem = iItem;
    item.lParam = lParam;
    item.pszText = (PWSTR)pszText;
    item.mask = LVIF_IMAGE | LVIF_PARAM | LVIF_TEXT;

    return InsertItem(&item);
}

int LListView::InsertItem(__in const LPLVITEMA pitem)
{
    return (int)SendMessage(LVM_INSERTITEMA, 0, (LPARAM)pitem);
}

int LListView::InsertItem(__in const LPLVITEMW pitem)
{
    return (int)SendMessage(LVM_INSERTITEMW, 0, (LPARAM)pitem);
}

void LListView::SetCheckState(__in UINT iIndex, __in BOOL fCheck)
{
    SetItemState(iIndex, INDEXTOSTATEIMAGEMASK(fCheck ? 2 : 1),
        LVIS_STATEIMAGEMASK);
}

BOOL LListView::SetColumnWidth(__in int iCol, __in int cx)
{
    return ListView_SetColumnWidth(m_hWnd, iCol, cx);
}

void LListView::SetExtendedListViewStyle(__in DWORD dwExStyle)
{
    ListView_SetExtendedListViewStyle(m_hWnd, dwExStyle);
}

HIMAGELIST LListView::SetImageList(__in HIMAGELIST hImageList,
                                   __in int iImageList)
{
    return ListView_SetImageList(m_hWnd, hImageList, iImageList);
}

BOOL LListView::SetItem(const LPLVITEMA pitem)
{
    return (BOOL)SendMessage(LVM_SETITEMA, 0, (LPARAM)pitem);
}

BOOL LListView::SetItem(const LPLVITEMW pitem)
{
    return (BOOL)SendMessage(LVM_SETITEMW, 0, (LPARAM)pitem);
}

void LListView::SetItemState(__in int i, __in UINT state, __in UINT mask)
{
    ListView_SetItemState(m_hWnd, i, state, mask);
}

void LListView::SetItemText(__in int i, __in int iSubItem, __in PCSTR pszText)
{
    LVITEMA lvi = { 0 };

    lvi.iSubItem = iSubItem;
    lvi.pszText = (PSTR)pszText;

    SendMessage(LVM_SETITEMTEXTA, (WPARAM)i, (LPARAM)&lvi);
}

void LListView::SetItemText(__in int i, __in int iSubItem, __in PCWSTR pszText)
{
    LVITEMW lvi = { 0 };

    lvi.iSubItem = iSubItem;
    lvi.pszText = (PWSTR)pszText;

    SendMessage(LVM_SETITEMTEXTW, (WPARAM)i, (LPARAM)&lvi);
}

int LListView::SetSelectionMark(__in int iIndex)
{
    return ListView_SetSelectionMark(m_hWnd, iIndex);
}

///////////////////////////////////////////////////////////////////////////////
// LMonthCal

LMonthCal::LMonthCal(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
}

LMonthCal& LMonthCal::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

BOOL LMonthCal::GetCurSel(__in LPSYSTEMTIME lpSysTime)
{
    return (BOOL)SendMessage(MCM_GETCURSEL, 0, (LPARAM)lpSysTime);
}

//////////////////////////////////////////////////////////////////////////
// LProgressBar

LProgressBar& LProgressBar::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

int LProgressBar::GetStep(void)
{
    return (int)SendMessage(PBM_GETSTEP);
}

int LProgressBar::SetPos(__in int nPos)
{
    return (int)SendMessage(PBM_SETPOS, nPos);
}

void LProgressBar::SetRange32(__in int nLower, __in int nUpper)
{
    SendMessage(PBM_SETRANGE32, nLower, nUpper);
}

int LProgressBar::SetStep(__in int nStepInc)
{
    return (int)SendMessage(PBM_SETSTEP, (WPARAM)nStepInc);
}

int LProgressBar::StepIt(void)
{
    return (int)SendMessage(PBM_STEPIT);
}

///////////////////////////////////////////////////////////////////////////////
// LPropSheetPage & LPropSheet

LPropSheetPage::LPropSheetPage(__in UINT idPage) : LDialog(idPage)
{
    m_hPropPage = NULL;
    m_pParent = NULL;
}

LPropSheetPage::~LPropSheetPage(void)
{
    if (NULL != m_hPropPage)
        DestroyPropertySheetPage(m_hPropPage);
}

BOOL LPropSheetPage::Create(
    __in DWORD dwFlags,
    __in PCSTR pszTitle,
    __in LPARAM lParam,
    __in LPFNPSPCALLBACKA pfnCallback)
{
    LAppModule* theApp = LAppModule::GetApp();

    PROPSHEETPAGEA prop;
    ZeroMemory(&prop, sizeof(PROPSHEETPAGEA));
    prop.dwSize = sizeof(PROPSHEETPAGEA);
    prop.dwFlags = dwFlags;
    prop.hInstance = theApp->GetInstance();
    prop.pszTemplate = MAKEINTRESOURCEA(m_uId);

    if (NULL != pszTitle)
    {
        prop.pszTitle = pszTitle;
        prop.dwFlags |= PSP_USETITLE;
    }

    prop.pfnDlgProc = StartDlgProc;
    prop.lParam = lParam;
    m_hPropPage = CreatePropertySheetPageA(&prop);

    return NULL != m_hPropPage;
}

BOOL LPropSheetPage::Create(
    __in DWORD dwFlags,
    __in PCWSTR pszTitle,
    __in LPARAM lParam,
    __in LPFNPSPCALLBACKW pfnCallback)
{
    LAppModule* theApp = LAppModule::GetApp();

    PROPSHEETPAGEW prop;
    ZeroMemory(&prop, sizeof(PROPSHEETPAGEW));
    prop.dwSize = sizeof(PROPSHEETPAGEW);
    prop.dwFlags = dwFlags;
    prop.hInstance = theApp->GetInstance();
    prop.pszTemplate = MAKEINTRESOURCEW(m_uId);

    if (NULL != pszTitle)
    {
        prop.pszTitle = pszTitle;
        prop.dwFlags |= PSP_USETITLE;
    }

    prop.pfnDlgProc = StartDlgProc;
    prop.lParam = lParam;
    m_hPropPage = CreatePropertySheetPageW(&prop);

    return NULL != m_hPropPage;
}

LPropSheet* LPropSheetPage::GetParentSheet(void)
{
    return m_pParent;
}

#pragma pack(push, 1)
typedef struct ThunkPS {
    DWORD mov1;
    BYTE mov2;
    DWORD_PTR hWnd;
    DWORD mov3;
    DWORD_PTR pThis;
    BYTE jmp;
    DWORD_PTR proc;
} THUNKPS;
#pragma pack(pop)

LPropSheet::LPropSheet(__in int nMaxCnt /* = 1 */)
{
    m_thunk = new THUNKPS;

    m_dwFlags = PSH_USECALLBACK;
    m_nPageCnt = 0;

    if (nMaxCnt > 0)
        m_nMaxCnt = nMaxCnt;
    else
        m_nMaxCnt = 1;

    m_hPages = new HPROPSHEETPAGE[m_nMaxCnt];
}

LPropSheet::~LPropSheet(void)
{
    if (NULL != m_hPages)
        delete [] m_hPages;
    delete m_thunk;
}

BOOL LPropSheet::AddPage(__in LPropSheetPage* page)
{
    LAppModule* theApp = LAppModule::GetApp();
    theApp->AddWndData(page);

    HPROPSHEETPAGE hPage = page->m_hPropPage;
    page->m_hPropPage = NULL;
    page->m_pParent = this;
    if (NULL == m_hPages)
        return AddPage(hPage);

    if (m_nPageCnt == m_nMaxCnt)
    {
        // 扩大数组
        m_nMaxCnt *= 2;
        HPROPSHEETPAGE *pages = new HPROPSHEETPAGE[m_nMaxCnt];
        CopyMemory(pages, m_hPages, sizeof(HPROPSHEETPAGE) * m_nPageCnt);
        delete [] m_hPages;
        m_hPages = pages;
    }

    m_hPages[m_nPageCnt] = hPage;
    ++m_nPageCnt;
    return TRUE;
}

BOOL LPropSheet::AddPage(__in HPROPSHEETPAGE hPage)
{
    return (BOOL)PropSheet_AddPage(m_hWnd, hPage);
}

int LPropSheet::DoModal(
    __in HWND hParent,
    __in PCSTR pszCaption)
{
    LAppModule* theApp = LAppModule::GetApp();

    PROPSHEETHEADERA psh;
    ZeroMemory(&psh, sizeof(PROPSHEETHEADERA));
    psh.dwSize = sizeof(PROPSHEETHEADERA);
    psh.dwFlags = m_dwFlags;
    psh.hwndParent = hParent;
    psh.hInstance = theApp->GetInstance();
    psh.pszCaption = pszCaption;
    psh.nPages = m_nPageCnt;
    psh.phpage = m_hPages;

    InitThunk();
    psh.pfnCallback = (PFNPROPSHEETCALLBACK)m_thunk;

    psh.dwFlags &= ~PSH_MODELESS;
    int ret = (int)PropertySheetA(&psh);

    delete [] m_hPages;
    m_hPages = NULL;
    m_nPageCnt = 0;
    return ret;
}

int LPropSheet::DoModal(
    __in HWND hParent,
    __in PCWSTR pszCaption)
{
    LAppModule* theApp = LAppModule::GetApp();

    PROPSHEETHEADERW psh;
    ZeroMemory(&psh, sizeof(PROPSHEETHEADERW));
    psh.dwSize = sizeof(PROPSHEETHEADERW);
    psh.dwFlags = m_dwFlags;
    psh.hwndParent = hParent;
    psh.hInstance = theApp->GetInstance();
    psh.pszCaption = pszCaption;
    psh.nPages = m_nPageCnt;
    psh.phpage = m_hPages;

    InitThunk();
    psh.pfnCallback = (PFNPROPSHEETCALLBACK)m_thunk;
    psh.dwFlags |= PSH_USECALLBACK;

    psh.dwFlags &= ~PSH_MODELESS;
    int ret = (int)PropertySheetW(&psh);

    delete [] m_hPages;
    m_hPages = NULL;
    m_nPageCnt = 0;
    return ret;
}

void LPropSheet::InitThunk(void)
{
    // mov eax, dword ptr[esp + 4]
    m_thunk->mov1 = 0x0424448b;
    // mov dword ptr[m_hWnd], eax
    m_thunk->mov2 = 0xa3;
    m_thunk->hWnd = (DWORD_PTR)&m_hWnd;
    // mov dword ptr [esp + 4], pThis
    m_thunk->mov3 = 0x042444c7;
    m_thunk->pThis = (DWORD_PTR)this;
    // jmp proc
    m_thunk->jmp = 0xe9;
    m_thunk->proc = (INT_PTR)StartProc - ((INT_PTR)m_thunk + sizeof(THUNKPS));

    ::FlushInstructionCache(GetCurrentProcess(), m_thunk, sizeof(THUNKPS));
}

void LPropSheet::OnMessage(UINT uMsg, LPARAM lParam)
{
    // Dummy
}

DWORD LPropSheet::SetFlags(__in DWORD dwNewFlags)
{
    DWORD ret = m_dwFlags;
    m_dwFlags = dwNewFlags | PSH_USECALLBACK;
    return ret;
}

void LPropSheet::SetWizButtons(__in DWORD dwFlags)
{
    ::PropSheet_SetWizButtons(m_hWnd, dwFlags);
}

void LPropSheet::ShowWizButtons(__in DWORD dwFlag, __in DWORD dwButton)
{
    PropSheet_ShowWizButtons(m_hWnd, dwFlag, dwButton);
}

int CALLBACK LPropSheet::StartProc(LPropSheet* pThis, UINT uMsg, LPARAM lParam)
{
    pThis->OnMessage(uMsg, lParam);
    return 0;
}

///////////////////////////////////////////////////////////////////////////////
// LReBar

BOOL LReBar::Create(
    __in DWORD dwStyle,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    RECT rc = { 0 };
    rc.bottom = 20;
    return LWnd::Create(REBARCLASSNAME, _T(""),
        dwStyle | WS_CHILD | WS_VISIBLE, &rc, hWndParent, nID, lpParam);
}

BOOL LReBar::CreateEx(
    __in DWORD dwExStyle,
    __in DWORD dwStyle,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    RECT rc = { 0 };
    return LWnd::CreateEx(dwExStyle, REBARCLASSNAME, _T(""),
        dwStyle | WS_CHILD | WS_VISIBLE, &rc, hWndParent, nID, lpParam);
}

BOOL LReBar::InsertBand(__in UINT ulIndex, __in LPREBARBANDINFOA lpRbbi)
{
    return (BOOL)SendMessage(RB_INSERTBANDA, ulIndex, (LPARAM)lpRbbi);
}

BOOL LReBar::InsertBand(__in UINT ulIndex, __in LPREBARBANDINFOW lpRbbi)
{
    return (BOOL)SendMessage(RB_INSERTBANDW, ulIndex, (LPARAM)lpRbbi);
}

BOOL LReBar::SetBarInfo(__in LPREBARINFO lpRbi)
{
    return (BOOL)SendMessage(RB_SETBARINFO, 0, (LPARAM)lpRbi);
}

///////////////////////////////////////////////////////////////////////////////
// LStatusBar

BOOL LStatusBar::Create(
    __in HWND hParent,
    __in PCSTR lpszWindowName,
    __in DWORD dwStyle,
    __in UINT nID)
{
#ifdef _WIN32_WCE
    return FALSE;
#else
    m_hWnd = CreateStatusWindowA(dwStyle, lpszWindowName, hParent, nID);
    return NULL != m_hWnd;
#endif // _WIN32_WCE
}

BOOL LStatusBar::Create(
    __in HWND hParent,
    __in PCWSTR lpszWindowName,
    __in DWORD dwStyle,
    __in UINT nID)
{
    m_hWnd = CreateStatusWindowW(dwStyle, lpszWindowName, hParent, nID);
    return NULL != m_hWnd;
}

BOOL LStatusBar::GetRect(__in int nPart, __out LPRECT lprc)
{
    PDLASSERT(NULL != lprc);
    return (BOOL)SendMessage(SB_GETRECT, (WPARAM)nPart, (LPARAM)lprc);
}

BOOL LStatusBar::SetParts(__in int nParts, __in LPINT aWidths)
{
    return (BOOL)SendMessage(SB_SETPARTS, (WPARAM)nParts, (LPARAM)aWidths);
}

BOOL LStatusBar::SetText(__in int nPart, __in PCSTR lpszText)
{
    return (BOOL)SendMessage(SB_SETTEXTA, (WPARAM)nPart, (LPARAM)lpszText);
}

BOOL LStatusBar::SetText(__in int nPart, __in PCWSTR lpszText)
{
    return (BOOL)SendMessage(SB_SETTEXTW, (WPARAM)nPart, (LPARAM)lpszText);
}

//////////////////////////////////////////////////////////////////////////
// LTabCtrl

LTabCtrl::LTabCtrl(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
    // Dummy
}

LTabCtrl& LTabCtrl::operator=(__in HWND hWnd)
{
    PDLASSERT(NULL == m_hWnd);
    m_hWnd = hWnd;
    return *this;
}

LTabCtrl::operator HWND(void) const
{
    return m_hWnd;
}

void LTabCtrl::AdjustRect(__in BOOL fLarger, __inout LPRECT prc)
{
    SendMessage(TCM_ADJUSTRECT, (WPARAM)fLarger, (LPARAM)prc);
}

BOOL LTabCtrl::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_TABCONTROLA, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LTabCtrl::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_TABCONTROLW, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LTabCtrl::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_TABCONTROLA, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, (HMENU)nID, lpParam);
}

BOOL LTabCtrl::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_TABCONTROLW, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, (HMENU)nID, lpParam);
}

BOOL LTabCtrl::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_TABCONTROLA, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, lpParam);
}

BOOL LTabCtrl::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_TABCONTROLW, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, lpParam);
}

BOOL LTabCtrl::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_TABCONTROLA, lpWindowName, dwStyle,
        x, y, nWidth, nHeight, hWndParent, (HMENU)nID, lpParam);
}

BOOL LTabCtrl::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_TABCONTROLW, lpWindowName, dwStyle,
        x, y, nWidth, nHeight, hWndParent, (HMENU)nID, lpParam);
}

int LTabCtrl::GetCurSel(void)
{
    return (int)SendMessage(TCM_GETCURSEL);
}

int LTabCtrl::InsertItem(__in int iItem, __in const LPTCITEMA pitem)
{
    return (int)SendMessage(TCM_INSERTITEMA, (WPARAM)iItem, (LPARAM)pitem);
}

int LTabCtrl::InsertItem(__in int iItem, __in const LPTCITEMW pitem)
{
    return (int)SendMessage(TCM_INSERTITEMW, (WPARAM)iItem, (LPARAM)pitem);
}

int LTabCtrl::InsertItem(
    __in int iItem,
    __in PCSTR lpszItem,
    __in int nImage /* = -1 */,
    __in LPARAM lParam /* = 0 */)
{
    TCITEMA item = { 0 };

    item.mask = TCIF_IMAGE | TCIF_PARAM | TCIF_TEXT;
    item.iImage = nImage;
    item.lParam = lParam;
    item.pszText = (PSTR)lpszItem;

    return InsertItem(iItem, &item);
}

int LTabCtrl::InsertItem(__in int iItem, __in PCWSTR lpszItem,
                         __in int nImage /* = -1 */,
                         __in LPARAM lParam /* = 0 */)
{
    TCITEMW item = { 0 };

    item.mask = TCIF_IMAGE | TCIF_PARAM | TCIF_TEXT;
    item.iImage = nImage;
    item.lParam = lParam;
    item.pszText = (PWSTR)lpszItem;

    return InsertItem(iItem, &item);
}

HIMAGELIST LTabCtrl::SetImageList(__in HIMAGELIST himl)
{
    return (HIMAGELIST)SendMessage(TCM_SETIMAGELIST, 0, (LPARAM)himl);
}

///////////////////////////////////////////////////////////////////////////////
// LToolBar

LToolBar::LToolBar(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
}

BOOL LToolBar::Create(__in DWORD dwStyle, __in HWND hWndParent, __in UINT nID)
{
    RECT rc = { 0 };
    return LWnd::Create(TOOLBARCLASSNAME, NULL,
        dwStyle | WS_CHILD | WS_VISIBLE, &rc, hWndParent,
        nID, NULL);
}

BOOL LToolBar::AddButtons(
    __in UINT uNumButtons,
    __in LPCTBBUTTON lpButtons,
    __in BOOL bUnicode /* = TbUnicode */)
{
    UINT msg = bUnicode ? TB_ADDBUTTONSW : TB_ADDBUTTONSA;
    return (BOOL)SendMessage(msg, uNumButtons, (LPARAM)lpButtons);
}

int LToolBar::AddString(__in PCSTR lpString)
{
    return (int)SendMessage(TB_ADDSTRINGA, 0, (LPARAM)lpString);
}

int LToolBar::AddString(__in PCWSTR lpString)
{
    return (int)SendMessage(TB_ADDSTRINGW, 0, (LPARAM)lpString);
}

void LToolBar::AutoSize(void)
{
    SendMessage(TB_AUTOSIZE);
}

void LToolBar::ButtonStructSize(__in int cb)
{
    SendMessage(TB_BUTTONSTRUCTSIZE, cb);
}

BOOL LToolBar::EnableButton(__in int idButton, __in BOOL fEnable)
{
    return (BOOL)SendMessage(TB_ENABLEBUTTON, idButton, MAKELONG(fEnable, 0));
}

HIMAGELIST LToolBar::SetImageList(__in HIMAGELIST himl)
{
    return (HIMAGELIST)SendMessage(TB_SETIMAGELIST, 0, (LPARAM)himl);
}

///////////////////////////////////////////////////////////////////////////////
// LToolTip

LToolTip::LToolTip(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
    // Dummy
}

BOOL LToolTip::Create(__in DWORD dwStyle, __in HWND hWndParent)
{
    return LWnd::Create(TOOLTIPS_CLASS, static_cast<LPTSTR>(NULL),
        dwStyle | WS_POPUP, CW_USEDEFAULT, CW_USEDEFAULT,
        CW_USEDEFAULT, CW_USEDEFAULT, hWndParent, NULL, NULL);
}

void LToolTip::Activate(__in BOOL fActivate /* = TRUE */)
{
    SendMessage(TTM_ACTIVATE, fActivate);
}

BOOL LToolTip::AddTool(__in LPTOOLINFOA pti)
{
    LAppModule *theApp = LAppModule::GetApp();

    PDLASSERT(NULL != pti);
    pti->cbSize = sizeof(TOOLINFOA);
    pti->hinst = theApp->GetInstance();
    return (BOOL)SendMessage(TTM_ADDTOOLA, 0, (LPARAM)pti);
}

BOOL LToolTip::AddTool(__in LPTOOLINFOW pti)
{
    LAppModule *theApp = LAppModule::GetApp();

    PDLASSERT(NULL != pti);
    pti->cbSize = sizeof(TOOLINFOW);
    pti->hinst = theApp->GetInstance();
    return (BOOL)SendMessage(TTM_ADDTOOLW, 0, (LPARAM)pti);
}

BOOL LToolTip::AddTool(__in HWND hWnd, PCSTR lpszText)
{
    TOOLINFOA ti = { 0 };
    ti.uFlags = TTF_IDISHWND | TTF_SUBCLASS;
    ti.lpszText = (PSTR)lpszText;
    ti.uId = (UINT_PTR)hWnd;
    return AddTool(&ti);
}

BOOL LToolTip::AddTool(__in HWND hWnd, PCWSTR lpszText)
{
    TOOLINFOW ti = { 0 };
    ti.uFlags = TTF_IDISHWND | TTF_SUBCLASS;
    ti.lpszText = (PWSTR)lpszText;
    ti.uId = (UINT_PTR)hWnd;
    return AddTool(&ti);
}

///////////////////////////////////////////////////////////////////////////////
// LTrackBar

LTrackBar& LTrackBar::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

void LTrackBar::SetRange(
    __in int nMin, __in int nMax,
    __in BOOL bRedraw /* = FALSE */)
{
    SendMessage(TBM_SETRANGE, (WPARAM)bRedraw, MAKELPARAM(nMin, nMax));
}

//////////////////////////////////////////////////////////////////////////
// LTreeView

LTreeView::LTreeView(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
    // Dummy
}

LTreeView& LTreeView::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

LTreeView::operator HWND(void) const
{
    return m_hWnd;
}

BOOL LTreeView::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_TREEVIEWA, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LTreeView::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_TREEVIEWW, lpWindowName, dwStyle, lpRect,
        hWndParent, nID, lpParam);
}

BOOL LTreeView::Create(
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT id,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_TREEVIEWA, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, (HMENU)id, lpParam);
}

BOOL LTreeView::Create(
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int x, __in int y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT id,
    __in PVOID lpParam)
{
    return LWnd::Create(WC_TREEVIEWW, lpWindowName, dwStyle, x, y,
        nWidth, nHeight, hWndParent, (HMENU)id, lpParam);
}

BOOL LTreeView::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_TREEVIEWA, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, lpParam);
}

BOOL LTreeView::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in LPCRECT lpRect,
    __in HWND hWndParent,
    __in UINT nID,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_TREEVIEWW, lpWindowName, dwStyle,
        lpRect, hWndParent, nID, lpParam);
}

BOOL LTreeView::CreateEx(
    __in DWORD dwExStyle,
    __in PCSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT id,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_TREEVIEWA, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, (HMENU)id, lpParam);
}

BOOL LTreeView::CreateEx(
    __in DWORD dwExStyle,
    __in PCWSTR lpWindowName,
    __in DWORD dwStyle,
    __in int X, __in int Y,
    __in int nWidth, __in int nHeight,
    __in HWND hWndParent,
    __in UINT id,
    __in PVOID lpParam)
{
    return LWnd::CreateEx(dwExStyle, WC_TREEVIEWW, lpWindowName, dwStyle,
        X, Y, nWidth, nHeight, hWndParent, (HMENU)id, lpParam);
}

BOOL LTreeView::DeleteAllItems(void)
{
    return TreeView_DeleteAllItems(m_hWnd);
}

BOOL LTreeView::DeleteItem(__in HTREEITEM hItem)
{
    return TreeView_DeleteItem(m_hWnd, hItem);
}

BOOL LTreeView::Expand(__in HTREEITEM hItem, __in UINT flag)
{
    return TreeView_Expand(m_hWnd, hItem, flag);
}

void LTreeView::ExpandAll(__in HTREEITEM hItem)
{
    // 首先展开本级所有项目
    Expand(hItem, TVE_EXPAND);

    // 遍历，展开所有子级项目
    hItem = GetNextItem(hItem, TVGN_CHILD);
    if (NULL == hItem)
        return;

    do
    {
        ExpandAll(hItem);
        hItem = GetNextItem(hItem, TVGN_NEXT);
    } while (NULL != hItem);
}

HTREEITEM LTreeView::GetDropHilight(void)
{
    return TreeView_GetDropHilight(m_hWnd);
}

BOOL LTreeView::GetItem(__inout LPTVITEMA pitem)
{
    return (BOOL)SendMessage(TVM_GETITEMA, 0, (LPARAM)pitem);
}

BOOL LTreeView::GetItem(__inout LPTVITEMW pitem)
{
    return (BOOL)SendMessage(TVM_GETITEMW, 0, (LPARAM)pitem);
}

HTREEITEM LTreeView::GetNextItem(__in HTREEITEM hItem,
                                 __in UINT uFlag)
{
    return TreeView_GetNextItem(m_hWnd, hItem, uFlag);
}

HTREEITEM LTreeView::GetSelection(void)
{
    return TreeView_GetSelection(m_hWnd);
}

HTREEITEM LTreeView::InsertItem(
    __in HTREEITEM hParent,
    __in HTREEITEM hInsertAfter,
    __in PCSTR lpszItem,
    __in int nImage /* = -1 */,
    __in int nSelImage /* = -1 */,
    __in LPARAM lParam /* = 0 */)
{
    TVINSERTSTRUCTA tvis = { 0 };

    tvis.hParent = hParent;
    tvis.hInsertAfter = hInsertAfter;

    tvis.item.mask = TVIF_CHILDREN | TVIF_IMAGE | TVIF_PARAM \
        | TVIF_SELECTEDIMAGE | TVIF_TEXT;
    tvis.item.cChildren = 0;
    tvis.item.iImage = nImage;
    tvis.item.iSelectedImage = nSelImage;
    tvis.item.lParam = lParam;
    tvis.item.pszText = (PSTR)lpszItem;

    HTREEITEM hRet = InsertItem(&tvis);
    if (TVI_ROOT != hParent && NULL != hRet)
        SetChildState(hParent, TRUE);

    return hRet;
}

HTREEITEM LTreeView::InsertItem(
    __in HTREEITEM hParent,
    __in HTREEITEM hInsertAfter,
    __in PCWSTR lpszItem,
    __in int nImage /* = -1 */,
    __in int nSelImage /* = -1 */,
    __in LPARAM lParam /* = 0 */)
{
    TVINSERTSTRUCTW tvis = { 0 };

    tvis.hParent = hParent;
    tvis.hInsertAfter = hInsertAfter;

    tvis.item.mask = TVIF_CHILDREN | TVIF_IMAGE | TVIF_PARAM \
        | TVIF_SELECTEDIMAGE | TVIF_TEXT;
    tvis.item.cChildren = 0;
    tvis.item.iImage = nImage;
    tvis.item.iSelectedImage = nSelImage;
    tvis.item.lParam = lParam;
    tvis.item.pszText = (PWSTR)lpszItem;

    HTREEITEM hRet = InsertItem(&tvis);
    if (TVI_ROOT != hParent && NULL != hRet)
        SetChildState(hParent, TRUE);

    return hRet;
}

HTREEITEM LTreeView::InsertItem(__in LPTVINSERTSTRUCTA lpis)
{
    return (HTREEITEM)SendMessage(TVM_INSERTITEMA, 0, (LPARAM)lpis);
}

HTREEITEM LTreeView::InsertItem(__in LPTVINSERTSTRUCTW lpis)
{
    return (HTREEITEM)SendMessage(TVM_INSERTITEMW, 0, (LPARAM)lpis);
}

BOOL LTreeView::SelectItem(__in HTREEITEM hItem)
{
    return TreeView_SelectItem(m_hWnd, hItem);
}

HIMAGELIST LTreeView::SetImageList(__in HIMAGELIST himl, __in int iImage)
{
    return TreeView_SetImageList(m_hWnd, himl, iImage);
}

BOOL LTreeView::SetItem(__in LPTVITEMA pItem)
{
    return (BOOL)SendMessage(TVM_SETITEMA, 0, (LPARAM)pItem);
}

BOOL LTreeView::SetItem(__in LPTVITEMW pItem)
{
    return (BOOL)SendMessage(TVM_SETITEMW, 0, (LPARAM)pItem);
}

void LTreeView::SetChildState(__in HTREEITEM hParent, __in BOOL bHasChild)
{
    TVITEM item = { 0 };
    item.mask = TVIF_CHILDREN | TVIF_HANDLE;
    item.cChildren = bHasChild ? 1 : 0;
    item.hItem = hParent;
    SetItem(&item);
}

BOOL LTreeView::SortChildrenCB(__in LPTVSORTCB psort, BOOL fRecurse)
{
    return (BOOL)SendMessage(TVM_SORTCHILDRENCB, fRecurse, (LPARAM)psort);
}

///////////////////////////////////////////////////////////////////////////////
// LUpDown

LUpDown::LUpDown(__in HWND hWnd /* = NULL */) : LWnd(hWnd)
{
}

LUpDown& LUpDown::operator=(__in HWND hWnd)
{
    m_hWnd = hWnd;
    return *this;
}

HWND LUpDown::SetBuddy(__in HWND hwndBuddy)
{
    return (HWND)SendMessage(UDM_SETBUDDY, (WPARAM)hwndBuddy);
}

short LUpDown::SetPos(__in short nPos)
{
    return (short)SendMessage(UDM_SETPOS, 0, MAKELPARAM(nPos, 0));
}

int LUpDown::SetPos32(__in int nPos)
{
#ifdef _WIN32_WCE
    return SetPos(nPos);
#else
    return (int)SendMessage(UDM_SETPOS32, 0, nPos);
#endif // _WIN32_WCE
}

void LUpDown::SetRange(__in short nLower, __in short nUpper )
{
    SendMessage(UDM_SETRANGE, 0, MAKELPARAM(nUpper, nLower));
}

void LUpDown::SetRange32(__in int iLow, __in int iHigh)
{
    SendMessage(UDM_SETRANGE32, iLow, iHigh);
}
