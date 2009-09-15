///////////////////////////////////////////////////////////////////////////////
// �ļ�����  PDLWizard.cpp
// ����ʱ�䣺2009-01-16
// ���ߣ�    ����
// ��Ȩ���У�Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// ˵����    �������
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

    pageStart.Create(PSP_DEFAULT, _T("PDL Ӧ�ó����� - ��ʼҳ"), 0, NULL);
    pageApp.Create(PSP_DEFAULT, _T("PDL Ӧ�ó����� - Ӧ�ó���"), 0, NULL);
    pageFinish.Create(PSP_DEFAULT, _T("PDL Ӧ�ó����� - ���"), 0, NULL);
    prop.AddPage(&pageStart);
    prop.AddPage(&pageApp);
    prop.AddPage(&pageFinish);

    prop.SetFlags(PSH_WIZARD);
    prop.DoModal(NULL, _T("PDL Ӧ�ó�����"));

    LAppModule::Destroy();
    theIni.Save(szIni);
    return 0;
}
