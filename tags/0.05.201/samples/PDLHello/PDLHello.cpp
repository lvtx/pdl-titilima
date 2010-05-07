///////////////////////////////////////////////////////////////////////////////
// FileName:    PDLHello.cpp
// Created:     2009/06/05
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: ³ÌÐòÈë¿Ú
///////////////////////////////////////////////////////////////////////////////

#include <pdl_base.h>
#include <pdl_module.h>
#include "HelloWnd.h"

#include "resource.h"

int WINAPI _tWinMain(
    HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
    LPTSTR lpCmdLine,
    int nShowCmd)
{
    LAppModule* theApp = LAppModule::Initialize(hInstance);


    WNDCLASS wc = { 0 };
    wc.hbrBackground = (HBRUSH)::GetStockObject(WHITE_BRUSH);
    wc.hCursor = ::LoadCursor(NULL, IDC_ARROW);
    wc.hIcon = ::LoadIcon(NULL, IDI_APPLICATION);
    wc.hInstance = theApp->GetInstance();
    wc.lpszClassName = _T("HelloPDLWnd");
    wc.lpszMenuName = MAKEINTRESOURCE(IDR_MENU);
    wc.style = CS_HREDRAW | CS_VREDRAW;

    CHelloWnd wnd(&wc);
    wnd.Create(NULL, _T("PDLHello"), WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
        NULL, NULL, NULL);
    wnd.ShowWindow(nShowCmd);
    wnd.UpdateWindow();

    MSG msg;
    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    LAppModule::Destroy();
    return 0;
}
