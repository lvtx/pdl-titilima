///////////////////////////////////////////////////////////////////////////////
// FileName:    AboutDlg.cpp
// Created:     2009/07/11
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: ���ڶԻ���ʵ��
///////////////////////////////////////////////////////////////////////////////

#include "AboutDlg.h"

#include "resource.h"

CAboutDlg::CAboutDlg(void) : LDialog(IDD_DLG_ABOUT)
{
    // Nothing
}

void CAboutDlg::OnCommand(
    WORD wNotifyCode,
    WORD wID,
    HWND hWndCtrl,
    BOOL& bHandled)
{
    switch (wID)
    {
    case IDOK:
        EndDialog(0);
        break;
    default:
        bHandled = FALSE;
    }
}
