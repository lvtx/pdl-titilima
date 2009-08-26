/**
 * \file pdl_gdi.h
 * \brief PDL GDI ������
 * \details ����ļ��а����� PDL �� GDI ���ֹ��ܵķ�װ�����£�
 *   \li \c LGdiObj GDI �������
 *   \li \c LBitmap λͼ������
 *   \li \c LBrush ��ˢ������
 *   \li \c LFont ���������
 *   \li \c LPen ���ʶ�����
 *   \li \c LDC HDC ��װ��
 *   \li \c LPaintDC BeginPaint/EndPaint ��װ��
 *   \li \c LClientDC GetDC/ReleaseDC ��װ��
 *   \li \c LWindowDC GetWindowDC/ReleaseDC ��װ��
 *   \li \c LBufferDC ˫���� HDC ��װ��
 */

#pragma once

#include <pdl_base.h>

/**
 * \class LGdiObj
 * \brief GDI �������
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
     * ��һ�� LGdiObj �����ŵ�һ�� HGDIOBJ ����ϡ�
     * @param [in] hGdiObj һ����Ч�� HGDIOBJ �����
     */
    void Attach(__in HGDIOBJ hGdiObj);

    /**
     * ��� LGdiObj �� HGDIOBJ ����ĸ��š�
     * @return ��ǰ�����ŵ� HGDIOBJ �����
     */
    HGDIOBJ Detach(void);

protected:
    /**
     * HGDIOBJ ���
     */
    HGDIOBJ m_hGdiObj;
};

/**
 * \class LBitmap
 * \brief λͼ������
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
     * ��һ��λͼ���ݴ���һ��λͼ��
     * @param [in] data �� BITMAPINFOHEADER ��ͷ��λͼ���ݡ�
     * @return ��������ɹ��򷵻�λͼ�ľ�������򷵻� NULL��
     */
    static HBITMAP CreateFromData(__in PBYTE data);

    /**
     * ���ļ�����һ��λͼ��
     * @param [in] lpszBmpFile һ����Ч�� BMP �ļ�����
     * @return ��������ɹ��򷵻�λͼ�ľ�������򷵻� NULL��
     */
    static HBITMAP LoadFromFile(__in PCSTR lpszBmpFile);

    /**
     * ���ļ�����һ��λͼ��
     * @param [in] lpszBmpFile һ����Ч�� BMP �ļ�����
     * @return ��������ɹ��򷵻�λͼ�ľ�������򷵻� NULL��
     */
    static HBITMAP LoadFromFile(__in PCWSTR lpszBmpFile);

private:
    /**
     * ��һ�� LFile ���󴴽�һ��λͼ��
     */
    static HBITMAP LoadFromFile(__in LFile* pFile);
};

/**
 * \class LBrush
 * \brief ��ˢ������
 */

class LBrush : public LGdiObj
{
public:
    LBrush(__in HBRUSH hBrush = NULL);
    operator HBRUSH(void);
    LBrush& operator=(__in HBRUSH hBrush);
public:

    /**
     * ����һ�� Halftone ��ˢ��
     * @return ��ˢ�����
     */
    static HBRUSH CreateHalftoneBrush(void);

};

/**
 * \class LFont
 * \brief ���������
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
 * \brief ���ʶ�����
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
 * \brief HDC ��װ��
 */

class LDC
{
public:
    LDC(__in HDC hDC = NULL);
    virtual ~LDC(void);
public:

    /**
     * ��һ�� LDC �����ŵ�һ�� HDC ����ϡ�
     * @param [in] hDC һ����Ч�� HDC �����
     */
    void Attach(__in HDC hDC);

    BOOL BitBlt(__in int nXDest, __in int nYDest, __in int nWidth,
        __in int nHeight, __in HDC hdcSrc, __in int nXSrc, __in int nYSrc,
        __in DWORD dwRop);
    HBITMAP CreateCompatibleBitmap(__in int cx, __in int cy);
    HDC CreateCompatibleDC(void);

    /**
     * ����һ�� 3D Ч���ľ��Ρ�
     * @param [in] lpRect Ҫ���Ƶľ��η�Χ��
     * @param [in] clrTopLeft ���ϱ߿����ɫ��
     * @param [in] clrBottomRight ���±߿����ɫ��
     */
    void Draw3dRect(__in LPCRECT lpRect, __in COLORREF clrTopLeft,
        __in COLORREF clrBottomRight);

    /**
     * ����һ�� 3D Ч���ľ��Ρ�
     * @param [in] x Ҫ���Ƶľ��κ����ꡣ
     * @param [in] y Ҫ���Ƶľ��������ꡣ
     * @param [in] cx ���εĿ�ȡ�
     * @param [in] cy ���εĸ߶ȡ�
     * @param [in] clrTopLeft ���ϱ߿����ɫ��
     * @param [in] clrBottomRight ���±߿����ɫ��
     */
    void Draw3dRect(__in int x, __in int y, __in int cx, __in int cy,
        __in COLORREF clrTopLeft, __in COLORREF clrBottomRight);

    /**
     * �� DC �ϻ���һ��λͼ��
     * @param [in] hBitmap Ҫ���Ƶ�λͼ�����
     * @param [in] x λͼ�ĺ����ꡣ
     * @param [in] y λͼ�������ꡣ
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL DrawBitmap(__in HBITMAP hBitmap, __in int x, __in int y);

    BOOL DrawEdge(__in LPRECT qrc, __in UINT edge, __in UINT grfFlags);
    BOOL DrawFocusRect(__in LPCRECT lprc);
    int DrawTextA(__in PCSTR lpString, __in int nCount,
        __inout LPRECT lpRect, __in UINT uFormat);
    int DrawTextW(__in PCWSTR lpString, __in int nCount,
        __inout LPRECT lpRect, __in UINT uFormat);

    /**
     * �ڸ����ľ��η�Χ�ڻ������֣����������Χ����ʹ��ʡ�Ժ����������֡�
     */
    int DrawTextClipped(__in PCSTR lpString, __in int nCount,
        __inout LPRECT lpRect, __in UINT uFormat);

    /**
     * �ڸ����ľ��η�Χ�ڻ������֣����������Χ����ʹ��ʡ�Ժ����������֡�
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
     * ʹ��ָ������ɫ�����һ�����Ρ�
     * @param [in] lpRect Ҫ���ľ��Ρ�
     * @param [in] clr Ҫ������ɫ��
     */
    void FillSolidRect(__in LPCRECT lpRect, __in COLORREF clr);

    /**
     * ʹ��ָ������ɫ�����һ�����Ρ�
     * @param [in] x Ҫ�����εĺ����ꡣ
     * @param [in] y Ҫ�����ε������ꡣ
     * @param [in] cx Ҫ�����εĿ�ȡ�
     * @param [in] cy Ҫ�����εĸ߶ȡ�
     * @param [in] clr Ҫ������ɫ��
     */
    void FillSolidRect(__in int x, __in int y, __in int cx, __in int cy,
        __in COLORREF clr);

    /**
     * ��ȡ LDC �����Ӧ�� HDC �����
     * @return LDC �����Ӧ�� HDC �����
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
     * HDC ���
     */
    HDC m_hDC;
};

/**
 * \class LPaintDC
 * \brief BeginPaint/EndPaint ��װ��
 */

class LPaintDC : public LDC
{
public:

    /**
     * ���캯����
     * @param [in] hWnd Ҫ���� WM_PAINT �Ĵ��ھ����
     * \note ���������֮�У������ BeginPaint��
     */
    LPaintDC(__in HWND hWnd = NULL);

    /**
     * ����������
     * \note ���������֮�У������ EndPaint��
     */
    ~LPaintDC(void);

public:
    /**
     * WM_PAINT ���ƽṹ
     */
    PAINTSTRUCT ps;
protected:
    /**
     * Ҫ���� WM_PAINT �Ĵ��ھ��
     */
    HWND m_hWnd;
};

/**
 * \class LClientDC
 * \brief GetDC/ReleaseDC ��װ��
 */

class LClientDC : public LDC
{
public:

    /**
     * ���캯����
     * @param [in] hWnd Ҫ���л��ƵĴ��ھ����
     * \note ���������֮�У������ GetDC��
     */
    LClientDC(__in HWND hWnd);

    /**
     * ����������
     * \note ���������֮�У������ ReleaseDC��
     */
    ~LClientDC(void);

protected:
    /**
     * Ҫ���л��ƵĴ��ھ��
     */
    HWND m_hWnd;
};

/**
 * \class LWindowDC
 * \brief GetWindowDC/ReleaseDC ��װ��
 */

class LWindowDC : public LDC
{
public:

    /**
     * ���캯����
     * @param [in] hWnd Ҫ���л��ƵĴ��ھ����
     * \note ���������֮�У������ GetWindowDC��
     */
    LWindowDC(__in HWND hWnd);

    /**
     * ����������
     * \note ���������֮�У������ ReleaseDC��
     */
    ~LWindowDC(void);

protected:
    /**
     * Ҫ���л��ƵĴ��ھ��
     */
    HWND m_hWnd;
};

/**
 * \class LBufferDC
 * \brief ˫���� HDC ��װ��
 */

class LBufferDC : public LDC
{
public:
    ~LBufferDC(void);
public:

    /**
     * �����Ƶ�ͼ���ύ��Ŀ�� DC �ϡ�
     */
    void CommitDraw(void);

    /**
     * ��ʼ׼�����ơ�
     * @param [in] dc Ҫ���Ƶ�Ŀ�� DC��
     * @param [in] cx DC �Ŀ�ȡ�
     * @param [in] cy DC �ĸ߶ȡ�
     */
    void PrepareDraw(__in LDC* dc, __in int cx, __in int cy);

    /**
     * ��ʼ׼�����ơ�
     * @param [in] dc Ҫ���Ƶ�Ŀ�� DC��
     * @param [in] rc Ҫ���Ƶľ��η�Χ��
     */
    void PrepareDraw(__in LDC* dc, __in const RECT& rc);

protected:
    /**
     * Ҫ���Ƶ�Ŀ�� DC
     */
    LDC *m_pDC;
    /**
     * ����λͼ
     */
    LBitmap m_bmp;
};
