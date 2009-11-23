/**
 * \file pdl_ctrlext.h
 * \brief PDL 扩展控件
 * \details 这个文件中定义了常用的非标准控件，如下：
 *   \li \c LHyperLink PDL 超级链接类
 *   \li \c LSplitter PDL 分隔窗口类
 */

#pragma once

#include "pdl_base.h"
#include "pdl_window.h"
#include "pdl_message.h"
#include "pdl_ctrl.h"
#include "pdl_commctrl.h"
#include "pdl_gdi.h"

/**
 * \class LHyperLink
 * \brief PDL 超级链接类
 */

class LHyperLink : public LStatic, protected LSubclassWnd
{
    PDL_DECLARE_WINCLASS(LHyperLink)
public:
    LHyperLink(void);
    LHyperLink& operator=(__in HWND hWnd);
public:

    /**
     * 将 LHyperLink 对象附着在一个窗口上。
     * @param [in] hWnd 一个有效的窗口句柄。
     */
    BOOL Attach(__in HWND hWnd);

    BOOL Create(__in PCSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
        __in PVOID lpParam);
    BOOL Create(__in PCWSTR lpWindowName, __in DWORD dwStyle,
        __in LPCRECT lpRect, __in HWND hWndParent, __in UINT nID,
        __in PVOID lpParam);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);
    BOOL CreateEx(__in DWORD dwExStyle, __in PCWSTR lpWindowName,
        __in DWORD dwStyle, __in LPCRECT lpRect,
        __in HWND hWndParent, __in UINT nID, __in PVOID lpParam);

    /**
     * 设置鼠标悬停的文本色。
     * @param [in] clrHover 新的鼠标悬停文本色。
     * @param [in] bRedraw 是否重绘窗口。
     * @return 原有的鼠标悬停文本色。
     */
    COLORREF SetHoverColor(__in COLORREF clrHover, __in BOOL bRedraw = TRUE);

    /**
     * 设置普通状态的文本色。
     * @param [in] clrNormal 新的普通状态文本色。
     * @param [in] bRedraw 是否重绘窗口。
     * @return 原有的普通状态文本色。
     */
    COLORREF SetNormalColor(__in COLORREF clrNormal, __in BOOL bRedraw = TRUE);

protected:
    PDL_DECLARE_MSGMAP();
    DECLARE_ERASEBKGND_HANDLER(OnEraseBkgnd);
    DECLARE_MOUSELEAVE_HANDLER(OnMouseLeave);
    DECLARE_MOUSEMOVE_HANDLER(OnMouseMove);
    DECLARE_PAINT_HANDLER(OnPaint);
    DECLARE_SETCURSOR_HANDLER(OnSetCursor);
private:
    /**
     * 鼠标是否悬停
     */
    BOOL m_bHover;
    /**
     * 光标指针
     */
    HCURSOR m_hCursor;
    /**
     * 普通状态文本色
     */
    COLORREF m_clrNormal;
    /**
     * 鼠标悬停文本色
     */
    COLORREF m_clrHover;
};

/**
 * \def LSPLIT_LEFTPANE
 * 分隔条左面板
 * \sa LSplitter::SetPane
 */
#define LSPLIT_LEFTPANE     0
/**
 * \def LSPLIT_TOPPANE
 * 分隔条上面板
 * \sa LSplitter::SetPane
 */
#define LSPLIT_TOPPANE      0
/**
 * \def LSPLIT_RIGHTPANE
 * 分隔条右面板
 * \sa LSplitter::SetPane
 */
#define LSPLIT_RIGHTPANE    1
/**
 * \def LSPLIT_BOTTOMPANE
 * 分隔条下面板
 * \sa LSplitter::SetPane
 */
#define LSPLIT_BOTTOMPANE   1

/**
 * \class LSplitter
 * \brief PDL 分隔窗口类
 */

class LSplitter : public LWindow
{
    PDL_DECLARE_WINCLASS(LSplitter)
public:
    /**
     * 构造函数
     * @param [in] bVertical 是否垂直分隔条。
     * @param [in] nSplitSize 分隔条尺寸。
     * @param [in] bFullDrag 拖动时是否实时改变面板大小。
     */
    LSplitter(__in BOOL bVertical = TRUE, __in int nSplitSize = 3,
        __in BOOL bFullDrag = FALSE);
public:

    /**
     * 创建分隔条。
     * @param [in] hParent 父窗口句柄。
     * @param [in] nID 窗口控件 ID。
     * @param [in] lpRect 分隔条所占矩形，如为 NULL 则填满父窗口。
     * @return 如果创建成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in HWND hParent, __in UINT nID,
        __in LPCRECT lpRect = NULL);

    /**
     * 创建分隔条。
     * @param [in] hParent 父窗口句柄。
     * @param [in] nID 窗口控件 ID。
     * @param [in] x 分隔条位置横坐标。
     * @param [in] y 分隔条位置纵坐标。
     * @param [in] cx 分隔条宽度。
     * @param [in] cy 分隔条高度。
     * @return 如果创建成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Create(__in HWND hParent, __in UINT nID, __in int x,
        __in int y, __in int cx, __in int cy);

    /**
     * 获得分隔条的分隔位置。
     * @return 分隔条的分隔位置。
     */
    int GetSplitPos(void);

    /**
     * 设置全部拖动属性。
     * @param [in] bFullDrag 新的全部拖动属性。
     * @return 原有的全部拖动属性。
     */
    BOOL SetFullDrag(__in BOOL bFullDrag);

    /**
     * 设置分隔条面板。
     * @param [in] i 要设置的面板号，可取如下值之一：
     *   \li \c LSPLIT_LEFTPANE
     *   \li \c LSPLIT_RIGHTPANE
     *   \li \c LSPLIT_TOPPANE
     *   \li \c LSPLIT_BOTTOMPANE
     * @param [in] hWnd 要设置为面板的窗口。
     * @param [in] nSize 此面板的大小。如果取 -1，则占满剩余空间（只对第二个面板有效）。
     * @param [in] nMinSize 面板的最小尺寸。
     * @return 如果设置成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL SetPane(__in int i, __in HWND hWnd, __in int nSize = -1,
        __in int nMinSize = 5);

    /**
     * 设置分隔条的分隔位置。
     * @param [in] nPos 分隔条的分隔位置。
     * @return 如果设置成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL SetSplitPos(__in int nPos);

    /**
     * 设置分隔条大小。
     * @param [in] nSplitSize 分隔条的新大小。
     * @return 分隔条的原有大小。
     */
    int SetSplitSize(__in int nSplitSize = 3);

    /**
     * 更新面板布局。
     * @param [in] rcSplitter 分隔条的所在矩形。
     */
    virtual void UpdateLayout(__in const RECT& rcSplitter);

protected:
    /**
     * 绘制分隔条。
     */
    virtual void DrawSplitter(__in LDC *pDC, __in const RECT &rcSplitter);
protected:
    PDL_DECLARE_MSGMAP();
    DECLARE_COMMAND_HANDLER(OnCommand);
    DECLARE_LBUTTONDOWN_HANDLER(OnLButtonDown);
    DECLARE_LBUTTONUP_HANDLER(OnLButtonUp);
    DECLARE_MOUSEMOVE_HANDLER(OnMouseMove);
    DECLARE_SETCURSOR_HANDLER(OnSetCursor);
    DECLARE_SIZE_HANDLER(OnSize);
protected:
    /**
     * 面板
     */
    LWnd m_wndPane[2];
    /**
     * 分隔位置
     */
    int m_nSplitPos;
    /**
     * 分隔条尺寸
     */
    int m_nSplitSize;
    /**
     * 第一个面板的最小尺寸
     */
    int m_nMinSize1;
    /**
     * 第二个面板的最小尺寸
     */
    int m_nMinSize2;
    /**
     * 是否垂直分隔条
     */
    BOOL m_bVertical;
    /**
     * 是否全部拖动
     */
    BOOL m_bFullDrag;
    /**
     * 是否绘制了分隔线
     */
    BOOL m_bBarDrawed;
};
