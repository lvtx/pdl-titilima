///////////////////////////////////////////////////////////////////////////////
// FileName:    AboutDlg.cpp
// Created:     2009/07/11
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 关于对话框实现
///////////////////////////////////////////////////////////////////////////////

#include "AboutDlg.h"

#include "resource.h"

CAboutDlg::CAboutDlg(void) : LDialog(IDD_DLG_ABOUT)
{
    // Nothing
}

PDL_BEGIN_MSGMAP(CAboutDlg)
    PROCESS_COMMAND(OnCommand)
PDL_END_MSGMAP(LDialog)

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
