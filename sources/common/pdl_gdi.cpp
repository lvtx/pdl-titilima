#include "..\..\include\pdl_gdi.h"
#include "..\..\include\pdl_file.h"
#include "..\..\include\pdl_string.h"

//////////////////////////////////////////////////////////////////////////
// LGdiObj

LGdiObj::LGdiObj(__in HGDIOBJ hGdiObj /* = NULL */) : m_hGdiObj(hGdiObj)
{
}

LGdiObj::~LGdiObj(void)
{
    if (NULL != m_hGdiObj)
        ::DeleteObject(m_hGdiObj);
}

LGdiObj& LGdiObj::operator=(__in HGDIOBJ hGdiObj)
{
    DeleteObject();
    m_hGdiObj = hGdiObj;
    return *this;
}

void LGdiObj::Attach(__in HGDIOBJ hGdiObj)
{
    m_hGdiObj = hGdiObj;
}

BOOL LGdiObj::DeleteObject(void)
{
    if (NULL == m_hGdiObj)
        return FALSE;
    return ::DeleteObject(Detach());
}

HGDIOBJ LGdiObj::Detach(void)
{
    HGDIOBJ hRet = m_hGdiObj;
    m_hGdiObj = NULL;
    return hRet;
}

//////////////////////////////////////////////////////////////////////////
// LBitmap

LBitmap::LBitmap(__in HBITMAP hBitmap /* = NULL */)
{
    m_hGdiObj = hBitmap;
}

LBitmap& LBitmap::operator=(__in HBITMAP hBitmap)
{
    DeleteObject();
    m_hGdiObj = hBitmap;
    return *this;
}

HBITMAP LBitmap::CreateFromData(__in PBYTE data)
{
    PBITMAPINFOHEADER pbih = NULL;
    if ('B' == data[0] && 'M' == data[1])
        pbih = (PBITMAPINFOHEADER)&data[sizeof(BITMAPFILEHEADER)];
    else
        pbih = (PBITMAPINFOHEADER)data;

    DWORD dwClrUsed = pbih->biClrUsed;
    DWORD dwSize = pbih->biSizeImage;
    if (0 == dwClrUsed)
    {
        if (24 != pbih->biBitCount)
            dwClrUsed = 1 << pbih->biBitCount;
    }
    if (0 == dwSize)
    {
        dwSize = ((((pbih->biWidth * pbih->biBitCount) + 31) & ~31) >> 3)
            * pbih->biHeight;
    }

    PBYTE pb = new BYTE[sizeof(BITMAPINFOHEADER) + dwClrUsed * sizeof(RGBQUAD)];

    PBITMAPINFO pbi = (PBITMAPINFO)pb;
    CopyMemory(&pbi->bmiHeader, pbih, sizeof(BITMAPINFOHEADER));
    pbi->bmiHeader.biClrUsed = dwClrUsed;
    pbi->bmiHeader.biSizeImage = dwSize;

    if (0 != dwClrUsed)
        CopyMemory(&pbi->bmiColors, pbih + 1, sizeof(RGBQUAD));

    HDC hdc = ::CreateCompatibleDC(NULL);
    PVOID dib = NULL;
    HBITMAP hBmp = ::CreateDIBSection(hdc, pbi, DIB_RGB_COLORS, &dib, NULL, 0);
    CopyMemory(dib, (PBYTE)(pbih + 1) + dwClrUsed * sizeof(RGBQUAD), dwSize);
    ::DeleteDC(hdc);

    delete [] pb;
    return hBmp;
}

HBITMAP LBitmap::LoadFromFile(__in PCSTR lpszBmpFile)
{
    LFile file;
    if (!file.Create(lpszBmpFile, GENERIC_READ, FILE_SHARE_READ,
        OPEN_EXISTING))
    {
        return NULL;
    }

    return LoadFromFile(&file);
}

HBITMAP LBitmap::LoadFromFile(__in PCWSTR lpszBmpFile)
{
    LFile file;
    if (!file.Create(lpszBmpFile, GENERIC_READ, FILE_SHARE_READ,
        OPEN_EXISTING))
    {
        return NULL;
    }

    return LoadFromFile(&file);
}

HBITMAP LBitmap::LoadFromFile(__in LFile* pFile)
{
    BITMAPFILEHEADER bfh;
    pFile->SetPointer(0, FILE_BEGIN);
    pFile->Read(&bfh, sizeof(BITMAPFILEHEADER));
    if ('MB' != bfh.bfType)
        return NULL;

    DWORD dwSize = pFile->GetSize();
    PBYTE buf = new BYTE[dwSize - sizeof(BITMAPFILEHEADER)];
    pFile->Read(buf, dwSize - sizeof(BITMAPFILEHEADER));
    HBITMAP hBmp = CreateFromData(buf);
    delete [] buf;

    return hBmp;
}

//////////////////////////////////////////////////////////////////////////
// LBrush

LBrush::LBrush(__in HBRUSH hBrush /* = NULL */) : LGdiObj(hBrush)
{
}

LBrush& LBrush::operator=(__in HBRUSH hBrush)
{
    DeleteObject();
    m_hGdiObj = hBrush;
    return *this;
}

HBRUSH LBrush::CreateHalftoneBrush(void)
{
    HBRUSH hRet = NULL;
    WORD grayPattern[8];
    for (int i = 0; i < 8; ++i)
        grayPattern[i] = (WORD)(0x5555 << (i & 1));

    HBITMAP grayBitmap = CreateBitmap(8, 8, 1, 1, &grayPattern);
    if (NULL != grayBitmap)
    {
        hRet = ::CreatePatternBrush(grayBitmap);
        ::DeleteObject(grayBitmap);
    }
    return hRet;
}

//////////////////////////////////////////////////////////////////////////
// LFont

LFont::LFont(__in HFONT hFont /* = NULL */)
{
    m_hGdiObj = hFont;
}

LFont& LFont::operator=(__in HFONT hFont)
{
    DeleteObject();
    m_hGdiObj = hFont;
    return *this;
}

BOOL LFont::CreateIndirect(__in LPLOGFONTA lf)
{
    HFONT hFont = CreateFontIndirectA(lf);
    if (NULL == hFont)
        return FALSE;

    DeleteObject();
    m_hGdiObj = hFont;
    return TRUE;
}

BOOL LFont::CreateIndirect(__in LPLOGFONTW lf)
{
    HFONT hFont = CreateFontIndirectW(lf);
    if (NULL == hFont)
        return FALSE;

    DeleteObject();
    m_hGdiObj = hFont;
    return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
// LPen

LPen::LPen(__in HPEN hPen /* = NULL */)
{
    m_hGdiObj = hPen;
}

LPen& LPen::operator=(__in HPEN hPen)
{
    DeleteObject();
    m_hGdiObj = hPen;
    return *this;
}

///////////////////////////////////////////////////////////////////////////////
// LRgn

LRgn::LRgn(__in HRGN hRgn /* = NULL */)
{
    m_hGdiObj = hRgn;
}

LRgn& LRgn::operator=(__in HRGN hRgn)
{
    DeleteObject();
    m_hGdiObj = hRgn;
    return *this;
}

///////////////////////////////////////////////////////////////////////////////
// LDC

LDC::LDC(__in HDC hDC /* = NULL */)
{
    m_hDC = hDC;
}

LDC::~LDC(void)
{
}

void LDC::Attach(__in HDC hDC)
{
    m_hDC = hDC;
}

BOOL LDC::BitBlt(__in int nXDest, __in int nYDest, __in int nWidth,
                 __in int nHeight, __in HDC hdcSrc, __in int nXSrc,
                 __in int nYSrc, __in DWORD dwRop)
{
    return ::BitBlt(m_hDC, nXDest, nYDest, nWidth, nHeight, hdcSrc, nXSrc, nYSrc,
        dwRop);
}

HBITMAP LDC::CreateCompatibleBitmap(__in int cx, __in int cy)
{
    return ::CreateCompatibleBitmap(m_hDC, cx, cy);
}

HDC LDC::CreateCompatibleDC(void)
{
    return ::CreateCompatibleDC(m_hDC);
}

void LDC::Draw3dRect(LPCRECT lpRect, COLORREF clrTopLeft, COLORREF clrBottomRight)
{
    Draw3dRect(lpRect->left, lpRect->top, lpRect->right - lpRect->left,
        lpRect->bottom - lpRect->top, clrTopLeft, clrBottomRight);
}

void LDC::Draw3dRect(int x, int y, int cx, int cy, COLORREF clrTopLeft,
                COLORREF clrBottomRight)
{
    FillSolidRect(x, y, cx - 1, 1, clrTopLeft);
    FillSolidRect(x, y, 1, cy - 1, clrTopLeft);
    FillSolidRect(x + cx, y, -1, cy, clrBottomRight);
    FillSolidRect(x, y + cy, cx, -1, clrBottomRight);
}

BOOL LDC::DrawBitmap(__in HBITMAP hBitmap, __in int x, __in int y)
{
    HDC hMemDC;
    HBITMAP hOldBmp;
    BITMAP bmp;
    BOOL bRet;

    hMemDC = ::CreateCompatibleDC(m_hDC);
    hOldBmp = (HBITMAP)::SelectObject(hMemDC, hBitmap);
    ::GetObject(hBitmap, sizeof(BITMAP), &bmp);
    bRet = ::BitBlt(m_hDC, x, y, bmp.bmWidth, bmp.bmHeight, hMemDC, 0, 0, SRCCOPY);
    ::SelectObject(hMemDC, hOldBmp);
    ::DeleteDC(hMemDC);

    return bRet;
}

BOOL LDC::DrawEdge(__in LPRECT qrc, __in UINT edge, __in UINT grfFlags)
{
    return ::DrawEdge(m_hDC, qrc, edge, grfFlags);
}

BOOL LDC::DrawFocusRect(__in LPCRECT lprc)
{
    return ::DrawFocusRect(m_hDC, lprc);
}

BOOL LDC::DrawIcon(__in int X, __in int Y, __in HICON hIcon)
{
    return ::DrawIcon(m_hDC, X, Y, hIcon);
}

int LDC::DrawTextA(
    __in PCSTR lpString,
    __in int nCount,
    __inout LPRECT lpRect,
    __in UINT uFormat)
{
#ifdef _WIN32_WCE
    LStringW strW = lpString;
    return ::DrawTextW(m_hDC, strW, nCount, lpRect, uFormat);
#else
    return ::DrawTextA(m_hDC, lpString, nCount, lpRect, uFormat);
#endif // _WIN32_WCE
}

int LDC::DrawTextW(
    __in PCWSTR lpString,
    __in int nCount,
    __inout LPRECT lpRect,
    __in UINT uFormat)
{
    return ::DrawTextW(m_hDC, lpString, nCount, lpRect, uFormat);
}

BOOL LDC::ExtTextOutA(
    __in int X, __in int Y,
    __in UINT fuOptions,
    __in CONST RECT* lprc,
    __in PCSTR lpString,
    __in UINT cbCount,
    __in CONST INT* lpDx)
{
    return ::ExtTextOutA(m_hDC, X, Y, fuOptions, lprc, lpString, cbCount,
        lpDx);
}

BOOL LDC::ExtTextOutW(
    __in int X, __in int Y,
    __in UINT fuOptions,
    __in CONST RECT* lprc,
    __in PCWSTR lpString,
    __in UINT cbCount,
    __in CONST INT* lpDx)
{
    return ::ExtTextOutW(m_hDC, X, Y, fuOptions, lprc, lpString, cbCount,
        lpDx);
}

int LDC::FillRect(const RECT *lprc, HBRUSH hbr)
{
    return ::FillRect(m_hDC, lprc, hbr);
}

void LDC::FillSolidRect(LPCRECT lpRect, COLORREF clr)
{
    SetBkColor(clr);
    ExtTextOut(0, 0, ETO_OPAQUE, lpRect, NULL, 0, NULL);
}

void LDC::FillSolidRect(int x, int y, int cx, int cy, COLORREF clr)
{
    SetBkColor(clr);

    RECT rc;
    ::SetRect(&rc, x, y, x + cx, y + cy);
    ExtTextOut(0, 0, ETO_OPAQUE, &rc, NULL, 0, NULL);
}

HDC LDC::GetSafeHDC(void) const
{
    return m_hDC;
}

BOOL LDC::GetTextMetricsA(LPTEXTMETRICA lptm)
{
    return ::GetTextMetricsA(m_hDC, lptm);
}

BOOL LDC::GetTextMetricsW(LPTEXTMETRICW lptm)
{
    return ::GetTextMetricsW(m_hDC, lptm);
}

BOOL LDC::LineTo(__in int nXEnd, __in int nYEnd)
{
    return ::LineTo(m_hDC, nXEnd, nYEnd);
}

BOOL LDC::MoveToEx(__in int X, __in int Y, __out LPPOINT lpPoint /* = NULL */)
{
    return ::MoveToEx(m_hDC, X, Y, lpPoint);
}

BOOL LDC::PatBlt(int nXLeft, int nYLeft, int nWidth, int nHeight, DWORD dwRop)
{
    return ::PatBlt(m_hDC, nXLeft, nYLeft, nWidth, nHeight, dwRop);
}

BOOL LDC::Rectangle(__in int left, __in int top,
                    __in int right, __in int bottom)
{
    return ::Rectangle(m_hDC, left, top, right, bottom);
}

BOOL LDC::Rectangle(__in LPCRECT lprc)
{
    PDLASSERT(NULL != lprc);
    if (NULL == lprc)
        return FALSE;
    return Rectangle(lprc->left, lprc->top, lprc->right, lprc->bottom);
}

HBITMAP LDC::SelectBitmap(__in HBITMAP hBitmap)
{
    return (HBITMAP)SelectObject(hBitmap);
}

HBRUSH LDC::SelectBrush(__in HBRUSH hBrush)
{
    return (HBRUSH)SelectObject(hBrush);
}

HFONT LDC::SelectFont(__in HFONT hFont)
{
    return (HFONT)SelectObject(hFont);
}

HGDIOBJ LDC::SelectObject(__in HGDIOBJ hgdiobj)
{
    return ::SelectObject(m_hDC, hgdiobj);
}

HPEN LDC::SelectPen(__in HPEN hPen)
{
    return (HPEN)SelectObject(hPen);
}

COLORREF LDC::SetBkColor(COLORREF crColor)
{
    return ::SetBkColor(m_hDC, crColor);
}

int LDC::SetBkMode(int iBkMode)
{
    return ::SetBkMode(m_hDC, iBkMode);
}

COLORREF LDC::SetTextColor(COLORREF crColor)
{
    return ::SetTextColor(m_hDC, crColor);
}

//////////////////////////////////////////////////////////////////////////
// LPaintDC

LPaintDC::LPaintDC(HWND hWnd) : m_hWnd(hWnd)
{
    m_hDC = ::BeginPaint(m_hWnd, &ps);
}

LPaintDC::~LPaintDC(void)
{
    ::EndPaint(m_hWnd, &ps);
}

//////////////////////////////////////////////////////////////////////////
// LClientDC

LClientDC::LClientDC(HWND hWnd) : m_hWnd(hWnd)
{
    m_hDC = ::GetDC(m_hWnd);
}

LClientDC::~LClientDC(void)
{
    ::ReleaseDC(m_hWnd, m_hDC);
}

//////////////////////////////////////////////////////////////////////////
// LWindowDC

LWindowDC::LWindowDC(HWND hWnd) : m_hWnd(hWnd)
{
    m_hDC = ::GetWindowDC(m_hWnd);
}

LWindowDC::~LWindowDC(void)
{
    ::ReleaseDC(m_hWnd, m_hDC);
}

//////////////////////////////////////////////////////////////////////////
// LBufferDC

LBufferDC::~LBufferDC(void)
{
    ::DeleteDC(m_hDC);
}

void LBufferDC::CommitDraw(void)
{
    BITMAP bmp;
    ::GetObject((HBITMAP)m_bmp, sizeof(BITMAP), &bmp);

    m_pDC->BitBlt(0, 0, bmp.bmWidth, bmp.bmHeight, m_hDC, 0, 0, SRCCOPY);
}

void LBufferDC::PrepareDraw(__in LDC *pDC, __in int cx, __in int cy)
{
    m_pDC = pDC;
    m_hDC = m_pDC->CreateCompatibleDC();

    m_bmp = m_pDC->CreateCompatibleBitmap(cx, cy);
    SelectBitmap(m_bmp);
}

void LBufferDC::PrepareDraw(__in LDC* dc, __in const RECT& rc)
{
    PrepareDraw(dc, rc.right - rc.left, rc.bottom - rc.top);
}
