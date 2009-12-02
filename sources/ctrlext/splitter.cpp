///////////////////////////////////////////////////////////////////////////////
// FileName:    splitter.cpp
// Created:     2009/04/21
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 分隔条控件实现
///////////////////////////////////////////////////////////////////////////////

#include "..\..\include\pdl_ctrlext.h"
#include "..\..\include\pdl_module.h"

#define HSPLITTER_CLASS _T("LSplitterH")
#define VSPLITTER_CLASS _T("LSplitterV")

PDL_DEFINE_WINCLASS(LSplitter)

LSplitter::LSplitter(
    __in BOOL bVertical /* = TRUE */,
    __in int nSplitSize /* = 3 */,
    __in BOOL bFullDrag /* = FALSE */)
    : m_bVertical(bVertical)
    , m_nSplitSize(nSplitSize)
    , m_bFullDrag(bFullDrag)
    , m_bBarDrawed(FALSE)
{
    PDLASSERT(nSplitSize > 0);

    WNDCLASS wc = { 0 };
    wc.hbrBackground = PDL_SYSBRUSH(COLOR_BTNFACE);
    wc.hCursor = ::LoadCursor(NULL, IDC_ARROW);
    wc.hInstance = LAppModule::GetApp()->GetInstance();
    wc.lpszClassName = bVertical ? VSPLITTER_CLASS : HSPLITTER_CLASS;
    Register(&wc);
}

BOOL LSplitter::Create(
    __in HWND hParent,
    __in UINT nID,
    __in LPCRECT lpRect /* = NULL */)
{
    RECT rcPos;
    if (NULL != lpRect)
    {
        rcPos = *lpRect;
    }
    else
    {
        // 如创建矩形为空，则占满父窗口客户区
        ::GetClientRect(hParent, &rcPos);
    }

    return LWindow::Create(m_bVertical ? VSPLITTER_CLASS : HSPLITTER_CLASS,
        _T(""), WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS,
        &rcPos, hParent, nID, NULL);
}

BOOL LSplitter::Create(
    __in HWND hParent,
    __in UINT nID,
    __in int x, __in int y,
    __in int cx, __in int cy)
{
    RECT rc;
    ::GetClientRect(hParent, &rc);
    if (CW_USEDEFAULT != x)
        rc.left = x;
    if (CW_USEDEFAULT != y)
        rc.top = y;
    if (CW_USEDEFAULT != cx)
        rc.right = rc.left + cx;
    if (CW_USEDEFAULT != cy)
        rc.bottom = rc.top + cy;
    return Create(hParent, nID, &rc);
}

void LSplitter::DrawSplitter(__in LDC *pDC, __in const RECT &rcSplitter)
{
    LBrush brush = LBrush::CreateHalftoneBrush();
    HBRUSH hBrushOld = pDC->SelectBrush(brush);
    if (m_bVertical)
    {
        pDC->PatBlt(m_nSplitPos - 1, rcSplitter.top, m_nSplitSize + 1,
            rcSplitter.bottom, PATINVERT);
    }
    else
    {
        pDC->PatBlt(rcSplitter.left, m_nSplitPos - 1, rcSplitter.right,
            m_nSplitSize + 1, PATINVERT);
    }
    pDC->SelectBrush(hBrushOld);
}

int LSplitter::GetSplitPos(void)
{
    return m_nSplitPos;
}

BOOL LSplitter::SetFullDrag(__in BOOL bFullDrag)
{
    BOOL ret = m_bFullDrag;
    m_bFullDrag = bFullDrag;
    return ret;
}

BOOL LSplitter::SetPane(
    __in int i,
    __in HWND hWnd,
    __in int nSize /* = -1 */,
    __in int nMinSize /* = 5 */)
{
    PDLASSERT(nMinSize > 0);

    if ((0 != i && 1 != i) || NULL != m_wndPane[i].GetSafeHWND())
        return FALSE;

    m_wndPane[i] = hWnd;
    m_wndPane[i].SetParent(m_hWnd); // 改变面板窗口的所属关系
    if (0 == i)
    {
        PDLASSERT(nSize >= nMinSize);
        m_nMinSize1 = nMinSize;
        m_nSplitPos = nSize;
    }
    else
    {
        m_nMinSize2 = nMinSize;
    }
    // 如果两个面板都有内容了，那么就更新一下布局
    if (NULL != m_wndPane[0].GetSafeHWND()
        && NULL != m_wndPane[1].GetSafeHWND())
    {
        RECT rcSplitter;
        GetClientRect(&rcSplitter);
        UpdateLayout(rcSplitter);
    }
    return TRUE;
}

BOOL LSplitter::SetSplitPos(__in int nPos)
{
    RECT rcSplitter;
    GetClientRect(&rcSplitter);
    int nMax = m_bVertical ? rcSplitter.right : rcSplitter.bottom;
    if (nPos < m_nMinSize1 || nPos > nMax - m_nMinSize2)
        return FALSE;

    m_nSplitPos = nPos;
    UpdateLayout(rcSplitter);
    return TRUE;
}

int LSplitter::SetSplitSize(__in int nSplitSize /* = 3 */)
{
    int ret = m_nSplitSize;
    m_nSplitSize = nSplitSize;
    return ret;
}

void LSplitter::UpdateLayout(__in const RECT& rcSplitter)
{
    if (m_bVertical)
    {
        // 对分隔条位置进行修正
        if (m_nSplitPos > rcSplitter.right - m_nMinSize2)
            m_nSplitPos = rcSplitter.right - m_nMinSize2;

        // 垂直分隔条的大小布局
        if (NULL != m_wndPane[0].GetSafeHWND())
        {
            m_wndPane[0].SetWindowPos(NULL, 0, 0, m_nSplitPos, rcSplitter.bottom,
                SWP_NOZORDER);
        }
        if (NULL != m_wndPane[1].GetSafeHWND())
        {
            m_wndPane[1].SetWindowPos(NULL, m_nSplitPos + m_nSplitSize, 0,
                rcSplitter.right - m_nSplitPos - m_nSplitSize, rcSplitter.bottom,
                SWP_NOZORDER);
        }
    }
    else
    {
        // 对分隔条位置进行修正
        if (m_nSplitPos > rcSplitter.bottom - m_nMinSize2)
            m_nSplitPos = rcSplitter.bottom - m_nMinSize2;

        // 水平分隔条的大小布局
        if (NULL != m_wndPane[0].GetSafeHWND())
        {
            m_wndPane[0].SetWindowPos(NULL, 0, 0, rcSplitter.right, m_nSplitPos,
                SWP_NOZORDER);
        }
        if (NULL != m_wndPane[1].GetSafeHWND())
        {
            m_wndPane[1].SetWindowPos(NULL, 0, m_nSplitPos + m_nSplitSize,
                rcSplitter.right, rcSplitter.bottom - m_nSplitPos - m_nSplitSize,
                SWP_NOZORDER);
        }
    }
}

///////////////////////////////////////////////////////////////////////////////

PDL_BEGIN_MSGMAP(LSplitter)
    PROCESS_COMMAND(OnCommand)
    PROCESS_LBUTTONDOWN(OnLButtonDown)
    PROCESS_LBUTTONUP(OnLButtonUp)
    PROCESS_MOUSEMOVE(OnMouseMove)
    PROCESS_SETCURSOR(OnSetCursor)
    PROCESS_SIZE(OnSize)
PDL_END_MSGMAP(LWindow)

void LSplitter::OnCommand(
    WORD wNotifyCode,
    WORD wID,
    HWND hWndCtrl,
    BOOL& bHandled)
{
    // 由于分隔条改变了窗口的所属关系，所以这里要重新传递命令消息
    ::SendMessage(GetParent(), WM_COMMAND, MAKEWPARAM(wID, wNotifyCode),
        (LPARAM)hWndCtrl);
}

void LSplitter::OnLButtonDown(UINT uFlags, int x, int y, BOOL& bHandled)
{
    RECT rc;
    GetClientRect(&rc);
    rc.left += m_nSplitPos;
    rc.right = rc.left + m_nSplitSize;

    POINT pt;
    pt.x = x;
    pt.y = y;

    if (!PtInRect(&rc, pt))
        return;

    if (!m_bFullDrag)
    {
        RECT rcClient;
        GetClientRect(&rcClient);
        // 绘制分隔条
        LWindowDC dc(m_hWnd);
        DrawSplitter(&dc, rcClient);
        // 考虑到连续快速点击分隔条的情况，m_bBarDrawed用于保证分隔条绘制的互斥性
        m_bBarDrawed = TRUE;
    }
    // 捕获鼠标
    SetCapture();
}

void LSplitter::OnLButtonUp(UINT uFlags, int x, int y, BOOL& bHandled)
{
    if (m_hWnd != ::GetCapture())
        return;

    if (!m_bFullDrag)
    {
        RECT rcSplitter;
        GetClientRect(&rcSplitter);
        if (m_bBarDrawed)
        {
            // 如果已绘制了分隔条，那么这里将其擦除
            LWindowDC dc(m_hWnd);
            DrawSplitter(&dc, rcSplitter);
            m_bBarDrawed = FALSE;
        }
        UpdateLayout(rcSplitter);
    }
    // 释放鼠标捕获
    ReleaseCapture();
}

void LSplitter::OnMouseMove(UINT uFlags, int x, int y, BOOL& bHandled)
{
    if ((uFlags & MK_LBUTTON) && ::GetCapture() == m_hWnd)
    {
        RECT rcSplitter;
        GetClientRect(&rcSplitter);
        if (!m_bFullDrag && m_bBarDrawed)
        {
            LWindowDC dc(m_hWnd);
            DrawSplitter(&dc, rcSplitter);
            m_bBarDrawed = FALSE;
        }
        if (m_bVertical)
        {
            if (x < m_nMinSize1)
                m_nSplitPos = m_nMinSize1;
            else if (rcSplitter.right - x < m_nMinSize2)
                m_nSplitPos = rcSplitter.right - m_nMinSize2;
            else
                m_nSplitPos = x;
        }
        else
        {
            if (y < m_nMinSize1)
                m_nSplitPos = m_nMinSize1;
            else if (rcSplitter.bottom - y < m_nMinSize2)
                m_nSplitPos = rcSplitter.bottom - m_nMinSize2;
            else
                m_nSplitPos = y;
        }
        if (m_bFullDrag)
        {
            UpdateLayout(rcSplitter);
        }
        else
        {
            LWindowDC dc(m_hWnd);
            DrawSplitter(&dc, rcSplitter);
            m_bBarDrawed = TRUE;
        }
    }
}

BOOL LSplitter::OnSetCursor(
    HWND hWnd,
    UINT nHitTest,
    UINT message,
    BOOL& bHandled)
{
    POINT pt;
    GetCursorPos(&pt);

    RECT rc;
    GetWindowRect(&rc);
    rc.left += m_nSplitPos;
    rc.right = rc.left + m_nSplitSize;

    if (WindowFromPoint(pt) == m_hWnd)
    {
        if (PtInRect(&rc, pt))
            SetCursor(LoadCursor(NULL, m_bVertical ? IDC_SIZEWE : IDC_SIZENS));
        else
            SetCursor(LoadCursor(NULL, IDC_ARROW));
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

void LSplitter::OnSize(UINT nType, int cx, int cy, BOOL& bHandled)
{
    if (0 == cx || 0 == cy)
        return;

    RECT rcSplitter;
    ::SetRect(&rcSplitter, 0, 0, cx, cy);

    UpdateLayout(rcSplitter);
}
