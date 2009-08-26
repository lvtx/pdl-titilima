///////////////////////////////////////////////////////////////////////////////
// 文件名：  PDLWizard.cpp
// 创建时间：2009-01-16
// 作者：    李马
// 版权所有：Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// 说明：    程序入口
///////////////////////////////////////////////////////////////////////////////

#include <pdl_base.h>
#include <pdl_module.h>
#include "config.h"
#include "PageStart.h"
#include "PageApp.h"
#include "PageFinish.h"

int WINAPI _tWinMain(
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPTSTR lpCmdLine,
    int nShowCmd)
{
    LComCtlInit init(ICC_WIN95_CLASSES);
    LAppModule::Initialize(hInstance);

    ZeroMemory(&theConfig, sizeof(CONFIG));
    TCHAR szIni[MAX_PATH];
    GetModuleFileName(NULL, szIni, MAX_PATH);
    lstrcpy(_tcsrchr(szIni, _T('\\')) + 1, _T("PDLWizard.ini"));
    theIni.Open(szIni);

    CPageStart pageStart;
    CPageApp pageApp;
    CPageFinish pageFinish;
    LPropSheet prop;

    pageStart.Create(PSP_DEFAULT, _T("PDL 应用程序向导 - 起始页"), 0, NULL);
    pageApp.Create(PSP_DEFAULT, _T("PDL 应用程序向导 - 应用程序"), 0, NULL);
    pageFinish.Create(PSP_DEFAULT, _T("PDL 应用程序向导 - 完成"), 0, NULL);
    prop.AddPage(&pageStart);
    prop.AddPage(&pageApp);
    prop.AddPage(&pageFinish);

    prop.SetFlags(PSH_WIZARD);
    prop.DoModal(NULL, _T("PDL 应用程序向导"));

    LAppModule::Destroy();
    theIni.Save(szIni);
    return 0;
}
