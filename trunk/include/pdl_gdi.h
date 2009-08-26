/**
 * \file pdl_gdi.h
 * \brief PDL GDI 辅助类
 * \details 这个文件中包括了 PDL 对 GDI 各种功能的封装，如下：
 *   \li \c LGdiObj GDI 对象基类
 *   \li \c LBitmap 位图对象类
 *   \li \c LBrush 画刷对象类
 *   \li \c LFont 字体对象类
 *   \li \c LPen 画笔对象类
 *   \li \c LDC HDC 封装类
 *   \li \c LPaintDC BeginPaint/EndPaint 封装类
 *   \li \c LClientDC GetDC/ReleaseDC 封装类
 *   \li \c LWindowDC GetWindowDC/ReleaseDC 封装类
 *   \li \c LBufferDC 双缓冲 HDC 封装类
 */

#pragma once

#include <pdl_base.h>

/**
 * \class LGdiObj
 * \brief GDI 对象基类
 */

class LGdiObj
{
public:
    LGdiObj(__in HGDIOBJ hGdiObj = NULL);
    virtual ~LGdiObj(void);
    operator HGDIOBJ(void);
    LGdiObj& operator=(__in HGDIOBJ hGdiObj);
public:

    /**
     * 将一个 LGdiObj 对象附着到一个 HGDIOBJ 句柄上。
     * @param [in] hGdiObj 一个有效的 HGDIOBJ 句柄。
     */
    void Attach(__in HGDIOBJ hGdiObj);

    /**
     * 解除 LGdiObj 对 HGDIOBJ 句柄的附着。
     * @return 先前所附着的 HGDIOBJ 句柄。
     */
    HGDIOBJ Detach(void);

protected:
    /**
     * HGDIOBJ 句柄
     */
    HGDIOBJ m_hGdiObj;
};

/**
 * \class LBitmap
 * \brief 位图对象类
 */

class LFile;
class LBitmap : public LGdiObj
{
public:
    LBitmap(__in HBITMAP hBitmap = NULL);
    operator HBITMAP(void);
    LBitmap& operator=(__in HBITMAP hBitmap);
public:

    /**
     * 从一段位图数据创建一个位图。
     * @param [in] data 以 BITMAPINFOHEADER 开头的位图数据。
     * @return 如果创建成功则返回位图的句柄，否则返回 NULL。
     */
    static HBITMAP CreateFromData(__in PBYTE data);

    /**
     * 从文件加载一个位图。
     * @param [in] lpszBmpFile 一个有效的 BMP 文件名。
     * @return 如果创建成功则返回位图的句柄，否则返回 NULL。
     */
    static HBITMAP LoadFromFile(__in PCSTR lpszBmpFile);

    /**
     * 从文件加载一个位图。
     * @param [in] lpszBmpFile 一个有效的 BMP 文件名。
     * @return 如果创建成功则返回位图的句柄，否则返回 NULL。
     */
    static HBITMAP LoadFromFile(__in PCWSTR lpszBmpFile);

private:
    /**
     * 从一个 LFile 对象创建一个位图。
     */
    static HBITMAP LoadFromFile(__in LFile* pFile);
};

/**
 * \class LBrush
 * \brief 画刷对象类
 */

class LBrush : public LGdiObj
{
public:
    LBrush(__in HBRUSH hBrush = NULL);
    operator HBRUSH(void);
    LBrush& operator=(__in HBRUSH hBrush);
public:

    /**
     * 创建一个 Halftone 画刷。
     * @return 画刷句柄。
     */
    static HBRUSH CreateHalftoneBrush(void);

};

/**
 * \class LFont
 * \brief 字体对象类
 */

class LFont : public LGdiObj
{
public:
    LFont(__in HFONT hFont = NULL);
    operator HFONT(void);
    LFont& operator=(__in HFONT hFont);
};

/**
 * \class LPen
 * \brief 画笔对象类
 */

class LPen : public LGdiObj
{
public:
    LPen(__in HPEN hPen = NULL);
    operator HPEN(void);
    LPen& operator=(__in HPEN hPen);
};

/**
 * \class LDC
 * \brief HDC 封装类
 */

class LDC
{
public:
    LDC(__in HDC hDC = NULL);
    virtual ~LDC(void);
public:

    /**
     * 将一个 LDC 对象附着到一个 HDC 句柄上。
     * @param [in] hDC 一个有效的 HDC 句柄。
     */
    void Attach(__in HDC hDC);

    BOOL BitBlt(__in int nXDest, __in int nYDest, __in int nWidth,
        __in int nHeight, __in HDC hdcSrc, __in int nXSrc, __in int nYSrc,
        __in DWORD dwRop);
    HBITMAP CreateCompatibleBitmap(__in int cx, __in int cy);
    HDC CreateCompatibleDC(void);

    /**
     * 绘制一个 3D 效果的矩形。
     * @param [in] lpRect 要绘制的矩形范围。
     * @param [in] clrTopLeft 左上边框的颜色。
     * @param [in] clrBottomRight 右下边框的颜色。
     */
    void Draw3dRect(__in LPCRECT lpRect, __in COLORREF clrTopLeft,
        __in COLORREF clrBottomRight);

    /**
     * 绘制一个 3D 效果的矩形。
     * @param [in] x 要绘制的矩形横坐标。
     * @param [in] y 要绘制的矩形纵坐标。
     * @param [in] cx 矩形的宽度。
     * @param [in] cy 矩形的高度。
     * @param [in] clrTopLeft 左上边框的颜色。
     * @param [in] clrBottomRight 右下边框的颜色。
     */
    void Draw3dRect(__in int x, __in int y, __in int cx, __in int cy,
        __in COLORREF clrTopLeft, __in COLORREF clrBottomRight);

    /**
     * 在 DC 上绘制一个位图。
     * @param [in] hBitmap 要绘制的位图句柄。
     * @param [in] x 位图的横坐标。
     * @param [in] y 位图的纵坐标。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL DrawBitmap(__in HBITMAP hBitmap, __in int x, __in int y);

    BOOL DrawEdge(__in LPRECT qrc, __in UINT edge, __in UINT grfFlags);
    BOOL DrawFocusRect(__in LPCRECT lprc);
    int DrawTextA(__in PCSTR lpString, __in int nCount,
        __inout LPRECT lpRect, __in UINT uFormat);
    int DrawTextW(__in PCWSTR lpString, __in int nCount,
        __inout LPRECT lpRect, __in UINT uFormat);

    /**
     * 在给定的矩形范围内绘制文字，如果超出范围，则使用省略号来剪切文字。
     */
    int DrawTextClipped(__in PCSTR lpString, __in int nCount,
        __inout LPRECT lpRect, __in UINT uFormat);

    /**
     * 在给定的矩形范围内绘制文字，如果超出范围，则使用省略号来剪切文字。
     */
    int DrawTextClipped(__in PCWSTR lpString, __in int nCount,
        __inout LPRECT lpRect, __in UINT uFormat);

    BOOL ExtTextOutA(__in int X, __in int Y, __in UINT fuOptions,
        __in CONST RECT* lprc, __in PCSTR lpString, __in UINT cbCount,
        __in CONST INT* lpDx);
    BOOL ExtTextOutW(__in int X, __in int Y, __in UINT fuOptions,
        __in CONST RECT* lprc, __in PCWSTR lpString, __in UINT cbCount,
        __in CONST INT* lpDx);
    int FillRect(__in const RECT *lprc, __in HBRUSH hbr);

    /**
     * 使用指定的颜色来填充一个矩形。
     * @param [in] lpRect 要填充的矩形。
     * @param [in] clr 要填充的颜色。
     */
    void FillSolidRect(__in LPCRECT lpRect, __in COLORREF clr);

    /**
     * 使用指定的颜色来填充一个矩形。
     * @param [in] x 要填充矩形的横坐标。
     * @param [in] y 要填充矩形的纵坐标。
     * @param [in] cx 要填充矩形的宽度。
     * @param [in] cy 要填充矩形的高度。
     * @param [in] clr 要填充的颜色。
     */
    void FillSolidRect(__in int x, __in int y, __in int cx, __in int cy,
        __in COLORREF clr);

    /**
     * 获取 LDC 对象对应的 HDC 句柄。
     * @return LDC 对象对应的 HDC 句柄。
     */
    HDC GetSafeHDC(void) const;

    BOOL GetTextMetricsA(__in LPTEXTMETRICA lptm);
    BOOL GetTextMetricsW(__in LPTEXTMETRICW lptm);
    BOOL LineTo(__in int nXEnd, __in int nYEnd);
    BOOL MoveToEx(__in int X, __in int Y, __out LPPOINT lpPoint = NULL);
    BOOL PatBlt(__in int nXLeft, __in int nYLeft,
        __in int nWidth, __in int nHeight, __in DWORD dwRop);
    BOOL Rectangle(__in int left, __in int top, __in int right, __in int bottom);
    BOOL Rectangle(__in LPCRECT lprc);
    HBITMAP SelectBitmap(__in HBITMAP hBitmap);
    HBRUSH SelectBrush(__in HBRUSH hBrush);
    HFONT SelectFont(__in HFONT hFont);
    HGDIOBJ SelectObject(__in HGDIOBJ hgdiobj);
    HPEN SelectPen(__in HPEN hPen);
    COLORREF SetBkColor(__in COLORREF crColor);
    int SetBkMode(__in int iBkMode);
    COLORREF SetTextColor(__in COLORREF crColor);
protected:
    /**
     * HDC 句柄
     */
    HDC m_hDC;
};

/**
 * \class LPaintDC
 * \brief BeginPaint/EndPaint 封装类
 */

class LPaintDC : public LDC
{
public:

    /**
     * 构造函数。
     * @param [in] hWnd 要处理 WM_PAINT 的窗口句柄。
     * \note 在这个函数之中，会调用 BeginPaint。
     */
    LPaintDC(__in HWND hWnd = NULL);

    /**
     * 析构函数。
     * \note 在这个函数之中，会调用 EndPaint。
     */
    ~LPaintDC(void);

public:
    /**
     * WM_PAINT 绘制结构
     */
    PAINTSTRUCT ps;
protected:
    /**
     * 要处理 WM_PAINT 的窗口句柄
     */
    HWND m_hWnd;
};

/**
 * \class LClientDC
 * \brief GetDC/ReleaseDC 封装类
 */

class LClientDC : public LDC
{
public:

    /**
     * 构造函数。
     * @param [in] hWnd 要进行绘制的窗口句柄。
     * \note 在这个函数之中，会调用 GetDC。
     */
    LClientDC(__in HWND hWnd);

    /**
     * 析构函数。
     * \note 在这个函数之中，会调用 ReleaseDC。
     */
    ~LClientDC(void);

protected:
    /**
     * 要进行绘制的窗口句柄
     */
    HWND m_hWnd;
};

/**
 * \class LWindowDC
 * \brief GetWindowDC/ReleaseDC 封装类
 */

class LWindowDC : public LDC
{
public:

    /**
     * 构造函数。
     * @param [in] hWnd 要进行绘制的窗口句柄。
     * \note 在这个函数之中，会调用 GetWindowDC。
     */
    LWindowDC(__in HWND hWnd);

    /**
     * 析构函数。
     * \note 在这个函数之中，会调用 ReleaseDC。
     */
    ~LWindowDC(void);

protected:
    /**
     * 要进行绘制的窗口句柄
     */
    HWND m_hWnd;
};

/**
 * \class LBufferDC
 * \brief 双缓冲 HDC 封装类
 */

class LBufferDC : public LDC
{
public:
    ~LBufferDC(void);
public:

    /**
     * 将绘制的图形提交到目标 DC 上。
     */
    void CommitDraw(void);

    /**
     * 开始准备绘制。
     * @param [in] dc 要绘制的目标 DC。
     * @param [in] cx DC 的宽度。
     * @param [in] cy DC 的高度。
     */
    void PrepareDraw(__in LDC* dc, __in int cx, __in int cy);

    /**
     * 开始准备绘制。
     * @param [in] dc 要绘制的目标 DC。
     * @param [in] rc 要绘制的矩形范围。
     */
    void PrepareDraw(__in LDC* dc, __in const RECT& rc);

protected:
    /**
     * 要绘制的目标 DC
     */
    LDC *m_pDC;
    /**
     * 缓冲位图
     */
    LBitmap m_bmp;
};
