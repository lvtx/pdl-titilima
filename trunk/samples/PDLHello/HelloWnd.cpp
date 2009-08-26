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
