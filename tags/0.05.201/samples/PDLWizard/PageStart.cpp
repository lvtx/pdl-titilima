///////////////////////////////////////////////////////////////////////////////
// 文件名：  PageStart.cpp
// 创建时间：2009-01-16
// 作者：    李马
// 版权所有：Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// 说明：    向导起始页面实现
///////////////////////////////////////////////////////////////////////////////

#include <pdl_base.h>
#include <pdl_module.h>
#include "PageStart.h"
#include "config.h"
#include <ShlObj.h>

#include "resource.h"

#define IDX_APP     0
#define IDX_DLL     1
#define IDX_CON     2
#define IDX_FIRST   IDX_APP
#define IDX_LAST    IDX_CON

CPageStart::CPageStart(void) : LPropSheetPage(IDD_PAGE_START)
{
    LAppModule* theApp = LAppModule::GetApp();
    HBITMAP hBmp = theApp->LoadBitmap(MAKEINTRESOURCE(IDB_BMP_TYPE));
    m_imlType.Create(32, 32, ILC_COLOR24 | ILC_MASK, IDX_LAST + 1, 1);
    m_imlType.AddMasked(hBmp, 0x800080);
    ::DeleteObject(hBmp);

    m_nSel = IDX_APP;
}

PDL_BEGIN_MSGMAP(CPageStart)
    PROCESS_COMMAND(OnCommand)
    PROCESS_INITDIALOG(OnInitDialog)
    PROCESS_NOTIFY(OnNotify)
PDL_END_MSGMAP(LPropSheetPage)

BOOL CPageStart::OnInitDialog(HWND hCtrlFocus, LPARAM lParam, BOOL& bHandled)
{
    LPropSheet* prop = GetParentSheet();
    prop->CenterWindow(NULL, WNDPOS_HCENTER | WNDPOS_VCENTER);
    prop->SetWizButtons(PSWIZB_CANCEL);

    m_lstType = GetDlgItem(IDC_LIST_TYPE);
    m_lstType.SetImageList(m_imlType, LVSIL_NORMAL);
    m_lstType.InsertItem(IDX_APP, _T("应用程序"), IDX_APP + 1, 0);
    m_lstType.InsertItem(IDX_DLL, _T("动态链接库"), IDX_DLL * 2, 0);
    m_lstType.InsertItem(IDX_CON, _T("控制台程序"), IDX_CON * 2, 0);
    m_lstType.SetSelectionMark(IDX_APP);

    TCHAR szPath[MAX_PATH];
    theIni.GetString("History", "LastPath", _T(""), szPath, MAX_PATH);
    SetDlgItemText(IDC_EDIT_PATH, szPath);

    return TRUE;
}

void CPageStart::OnCommand(
    WORD wNotifyCode,
    WORD wID,
    HWND hWndCtrl,
    BOOL& bHandled)
{
    if (EN_UPDATE == wNotifyCode)
    {
        LPropSheet* prop = GetParentSheet();

        TCHAR sz[2];
        GetDlgItemText(IDC_EDIT_NAME, sz, 2);
        if (_T('\0') == sz[0])
        {
            prop->SetWizButtons(PSWIZB_CANCEL);
            return;
        }

        GetDlgItemText(IDC_EDIT_PATH, sz, 2);
        if (_T('\0') == sz[0])
        {
            prop->SetWizButtons(PSWIZB_CANCEL);
            return;
        }

        prop->SetWizButtons(PSWIZB_NEXT | PSWIZB_CANCEL);
        return;
    }

    switch (wID)
    {
    case IDC_BTN_BROWSE:
        {
            BROWSEINFO bi;
            ZeroMemory(&bi, sizeof(BROWSEINFO));
            bi.hwndOwner = m_hWnd;
            bi.lpszTitle = _T("请选择工程的位置:");

            LPITEMIDLIST pidl = SHBrowseForFolder(&bi);
            if (NULL != pidl)
            {
                TCHAR szPath[MAX_PATH];
                SHGetPathFromIDList(pidl, szPath);
                SetDlgItemText(IDC_EDIT_PATH, szPath);
            }

        }
        break;
    }
}

LRESULT CPageStart::OnNotify(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
    LRESULT ret = 0;
    switch (pnmh->code)
    {
    case LVN_ITEMCHANGED:
        {
            LPNMLISTVIEW pnm = (LPNMLISTVIEW)pnmh;
            if ((m_nSel != pnm->iItem) && (LVIS_SELECTED & pnm->uNewState))
            {
                LVITEM item;
                ZeroMemory(&item, sizeof(LVITEM));
                item.mask = LVIF_IMAGE;
                // 清除前一个选定项
                item.iItem = m_nSel;
                item.iImage = m_nSel * 2;
                m_lstType.SetItem(&item);
                // 设置新的选定项
                m_nSel = pnm->iItem;
                item.iItem = m_nSel;
                item.iImage = pnm->iItem * 2 + 1;
                m_lstType.SetItem(&item);
            }
        }
        break;
    case PSN_SETACTIVE:
        {
            LPropSheet* prop = GetParentSheet();
            prop->ShowWizButtons(
                PSWIZB_NEXT | PSWIZB_FINISH | PSWIZB_CANCEL,
                PSWIZB_NEXT | PSWIZB_CANCEL);

            DWORD dwEnable = PSWIZB_CANCEL;
            if ('\0' != theConfig.szName[0] && '\0' != theConfig.szPath)
                dwEnable |= PSWIZB_NEXT;
            prop->SetWizButtons(dwEnable);
        }
        break;
    case PSN_WIZNEXT:
        {
            LONG mr = -1;
            switch (m_nSel)
            {
            case IDX_APP:
                {
                    mr = IDD_PAGE_APP;
                    theConfig.Type = Application;
                    theConfig.SubSystem = Windows;
                    GetDlgItemTextA(IDC_EDIT_NAME, theConfig.szName, MAX_PATH);
                    GetDlgItemTextA(IDC_EDIT_PATH, theConfig.szPath, MAX_PATH);
                    theIni.WriteString("History", "LastPath", theConfig.szPath);
                }
                break;
            case IDX_DLL:
                {
                    mr = IDD_PAGE_APP;
                    theConfig.Type = DynamicLinkLibrary;
                    theConfig.SubSystem = Windows;
                    GetDlgItemTextA(IDC_EDIT_NAME, theConfig.szName, MAX_PATH);
                    GetDlgItemTextA(IDC_EDIT_PATH, theConfig.szPath, MAX_PATH);
                    theIni.WriteString("History", "LastPath", theConfig.szPath);
                }
                break;
            case IDX_CON:
                {
                    mr = IDD_PAGE_APP;
                    theConfig.Type = Application;
                    theConfig.SubSystem = Console;
                    GetDlgItemTextA(IDC_EDIT_NAME, theConfig.szName, MAX_PATH);
                    GetDlgItemTextA(IDC_EDIT_PATH, theConfig.szPath, MAX_PATH);
                    theIni.WriteString("History", "LastPath", theConfig.szPath);
                }
                break;
            default:
                ret = -1;
            }
            SetWindowLong(DWL_MSGRESULT, mr);
        }
        break;
    default:
        bHandled = FALSE;
    }

    return ret;
}
