///////////////////////////////////////////////////////////////////////////////
// �ļ�����  PageFinish.cpp
// ����ʱ�䣺2009-01-19
// ���ߣ�    ����
// ��Ȩ���У�Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// ˵����    ���ҳ��ʵ��
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
                AddText("��������: Ӧ�ó���\r\n");
            else
                AddText("��������: �����г���\r\n");
        }
        break;
    case DynamicLinkLibrary:
        AddText("��������: ��̬���ӿ�\r\n");
        break;
    }
    AddText("��������: ");
    AddText(theConfig.szName);
    AddText("\r\n����·��: ");
    AddText(theConfig.szPath);
    AddText("\r\n��������: \r\n");
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
            LFile file;
            if (!file.Create(proj, GENERIC_WRITE, 0, CREATE_ALWAYS))
            {
                MessageBox(_T("��������ʧ�ܡ�"), _T("����"),
                    MB_ICONINFORMATION);
                break;
            }

            CProjectConfig cfg;
            cfg.OutputHeader(&file);
            if (CONFIG_ANSI & theConfig.Flags)
            {
                cfg.SetCharacterSet(MultiByte);
                cfg.OutputCfgDebug(&file);
                cfg.OutputCfgRelease(&file);
            }
            if (CONFIG_UNICODE & theConfig.Flags)
            {
                cfg.SetCharacterSet(Unicode);
                cfg.OutputCfgDebug(&file);
                cfg.OutputCfgRelease(&file);
            }
            cfg.OutputFiles(&file);
            cfg.CreateFiles();
        }
        break;
    default:
        bHandled = FALSE;
    }

    return ret;
}
