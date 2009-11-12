///////////////////////////////////////////////////////////////////////////////
// FileName:    HelloWnd.cpp
// Created:     2009/06/05
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 主窗口类实现
///////////////////////////////////////////////////////////////////////////////

#include "HelloWnd.h"
#include <pdl_module.h>
#include <pdl_gdi.h>
#include "AboutDlg.h"

#include "resource.h"

CHelloWnd::CHelloWnd(LPWNDCLASS wc) : LWindow(wc)
{
    // Nothing
}

PDL_BEGIN_MSGMAP(CHelloWnd)
    PROCESS_COMMAND(OnCommand)
    PROCESS_DESTROY(OnDestroy)
    PROCESS_PAINT(OnPaint)
PDL_END_MSGMAP(LWindow)

void CHelloWnd::OnCommand(
    WORD wNotifyCode,
    WORD wID,
    HWND hWndCtrl,
    BOOL& bHandled)
{
    switch (wID)
    {
    case ID_ABOUT:
        {
            CAboutDlg dlg;
            dlg.DoModal(m_hWnd);
        }
        break;
    case ID_EXIT:
        {
            DestroyWindow();
        }
        break;
    default:
        bHandled = FALSE;
    }
}

void CHelloWnd::OnDestroy(BOOL& bHandled)
{
    ::PostQuitMessage(0);
}

void CHelloWnd::OnPaint(BOOL& bHandled)
{
    LPaintDC dc(m_hWnd);

    RECT rc;
    GetClientRect(&rc);
    dc.DrawText(_T("Hello, PDL!"), -1, &rc,
        DT_CENTER | DT_VCENTER | DT_SINGLELINE);
}

void CHelloWnd::OnMsgProcceded(UINT uMsg, WPARAM wParam, LPARAM lParam, LRESULT lRet)
{
    if (WM_NCHITTEST == uMsg && HTCAPTION == lRet)
        PDLTRACE(_T("[%08x] wp = %08x, lp = %08x, ret = %08x\n"), uMsg, wParam, lParam, lRet);
}
