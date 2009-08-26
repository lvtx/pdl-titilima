///////////////////////////////////////////////////////////////////////////////
// 文件名：  PageApp.cpp
// 创建时间：2009-01-18
// 作者：    李马
// 版权所有：Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// 说明：    应用程序页面实现
///////////////////////////////////////////////////////////////////////////////

#include <pdl_base.h>
#include "PageApp.h"
#include "config.h"

#include "resource.h"

CPageApp::CPageApp(void) : LPropSheetPage(IDD_PAGE_APP)
{
    //
}

BOOL CPageApp::OnInitDialog(HWND hCtrlFocus, LPARAM lParam, BOOL& bHandled)
{
    CheckDlgButton(IDC_CHK_UNICODE, BST_CHECKED);
    CheckDlgButton(IDC_CHK_MANIFEST, BST_CHECKED);
    return TRUE;
}

void CPageApp::OnCommand(
    WORD wNotifyCode,
    WORD wID,
    HWND hWndCtrl,
    BOOL& bHandled)
{
    switch (wID)
    {
    case IDC_CHK_ANSI:
    case IDC_CHK_UNICODE:
        {
            LPropSheet* prop = GetParentSheet();
            if (BST_CHECKED == IsDlgButtonChecked(IDC_CHK_ANSI)
                || BST_CHECKED == IsDlgButtonChecked(IDC_CHK_UNICODE))
            {
                prop->SetWizButtons(PSWIZB_BACK | PSWIZB_NEXT);
            }
            else
            {
                prop->SetWizButtons(PSWIZB_BACK);
            }
        }
        break;
    }
}

LRESULT CPageApp::OnNotify(
    int idCtrl,
    LPNMHDR pnmh,
    BOOL& bHandled)
{
    LRESULT ret = 0;
    switch (pnmh->code)
    {
    case PSN_SETACTIVE:
        {
            LPropSheet* prop = GetParentSheet();
            prop->ShowWizButtons(
                PSWIZB_BACK | PSWIZB_NEXT | PSWIZB_FINISH | PSWIZB_CANCEL,
                PSWIZB_BACK | PSWIZB_NEXT | PSWIZB_CANCEL);
            prop->SetWizButtons(PSWIZB_BACK | PSWIZB_NEXT);
        }
        break;
    case PSN_WIZNEXT:
        {
            if (BST_CHECKED == IsDlgButtonChecked(IDC_CHK_ANSI))
                theConfig.Flags |= CONFIG_ANSI;
            if (BST_CHECKED == IsDlgButtonChecked(IDC_CHK_UNICODE))
                theConfig.Flags |= CONFIG_UNICODE;
            if (BST_CHECKED == IsDlgButtonChecked(IDC_CHK_MANIFEST))
                theConfig.Flags |= CONFIG_MANIFEST;
        }
        break;
    default:
        bHandled = FALSE;
    }

    return ret;
}
