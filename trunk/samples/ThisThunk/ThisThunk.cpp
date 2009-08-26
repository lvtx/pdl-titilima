#include <pdl_base.h>
#include <pdl_module.h>
#include "MainDlg.h"

int WINAPI _tWinMain(
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPTSTR lpCmdLine,
    int nShowCmd)
{
    LComCtlInit comctl(ICC_BAR_CLASSES);

    LAppModule::Initialize(hInstance);

    CMainDlg dlg;
    dlg.DoModal(NULL);

    LAppModule::Destroy();
    return 0;
}
