///////////////////////////////////////////////////////////////////////////////
// 文件名：  PageFinish.cpp
// 创建时间：2009-01-19
// 作者：    李马
// 版权所有：Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// 说明：    完成页面实现
///////////////////////////////////////////////////////////////////////////////

#include <pdl_base.h>
#include "PageFinish.h"
#include "config.h"

#include "resource.h"

CPageFinish::CPageFinish(void) : LPropSheetPage(IDD_PAGE_FINISH)
{
    //
}

void CPageFinish::AddText(__in PCSTR lpText)
{
    m_edtInfo.SetSelA(-1, -1);
    m_edtInfo.ReplaceSel(lpText);
}

void CPageFinish::FillConfig(void)
{
    m_edtInfo.SetWindowText(_T(""));
    switch (theConfig.Type)
    {
    case Application:
        {
            if (Windows == theConfig.SubSystem)
                AddText("程序类型: 应用程序\r\n");
            else
                AddText("程序类型: 命令行程序\r\n");
        }
        break;
    case DynamicLinkLibrary:
        AddText("程序类型: 动态链接库\r\n");
        break;
    }
    AddText("工程名称: ");
    AddText(theConfig.szName);
    AddText("\r\n工程路径: ");
    AddText(theConfig.szPath);
    AddText("\r\n工程配置: \r\n");
    if (CONFIG_ANSI & theConfig.Flags)
        AddText("* ANSI\r\n");
    if (CONFIG_UNICODE & theConfig.Flags)
        AddText("* Unicode\r\n");
    if (CONFIG_MANIFEST & theConfig.Flags)
        AddText("* XP Manifest\r\n");
}

BOOL CPageFinish::OnInitDialog(HWND hCtrlFocus, LPARAM lParam, BOOL& bHandled)
{
    m_edtInfo = GetDlgItem(IDC_EDIT_CONFIG);
    return TRUE;
}

LRESULT CPageFinish::OnNotify(
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
                PSWIZB_BACK | PSWIZB_FINISH | PSWIZB_CANCEL);
            prop->SetWizButtons(PSWIZB_BACK | PSWIZB_FINISH);
            FillConfig();
        }
        break;
    case PSN_WIZFINISH:
        {
            ::SetCurrentDirectoryA(theConfig.szPath);
            ::CreateDirectoryA(theConfig.szName, NULL);
            ::SetCurrentDirectoryA(theConfig.szName);

            CHAR proj[MAX_PATH];
            lstrcpyA(proj, theConfig.szName);
            lstrcatA(proj, ".vcproj");
            LXmlParser xml;

            CProjectConfig cfg;
            LXmlNode node = cfg.OutputHeader(&xml);
            if (CONFIG_ANSI & theConfig.Flags)
            {
                cfg.SetCharacterSet(MultiByte);
                cfg.OutputCfgDebug(&xml, node);
                cfg.OutputCfgRelease(&xml, node);
            }
            if (CONFIG_UNICODE & theConfig.Flags)
            {
                cfg.SetCharacterSet(Unicode);
                cfg.OutputCfgDebug(&xml, node);
                cfg.OutputCfgRelease(&xml, node);
            }
            cfg.OutputFiles(&xml, node);
            cfg.CreateFiles();

            if (!xml.Save(proj))
            {
                MessageBox(_T("创建工程失败。"), _T("错误"),
                    MB_ICONINFORMATION);
                break;
            }
        }
        break;
    default:
        bHandled = FALSE;
    }

    return ret;
}
