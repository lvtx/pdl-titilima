#include <pdl_base.h>
#include <pdl_module.h>

[!if WIN_APP]
int WINAPI _tWinMain(
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPTSTR lpCmdLine,
    int nShowCmd)
{
    LAppModule::Initialize(hInstance);

    // TODO: 在这里添加您的代码

    LAppModule::Destroy();
    return 0;
}
[!endif]
[!if CONSOLE_APP]
int _tmain(int argc, TCHAR* argv[])
{
    LAppModule::Initialize(NULL);

    // TODO: 在这里添加您的代码

    LAppModule::Destroy();
    return 0;
}
[!endif]
[!if DLL_APP]
BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, PVOID lpvReserved)
{
    if (DLL_PROCESS_ATTACH == fdwReason)
        LAppModule::Initialize(hinstDLL);
    else if (DLL_PROCESS_DETACH == fdwReason)
        LAppModule::Destroy();

    return TRUE;
}
[!endif]
