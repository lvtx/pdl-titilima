///////////////////////////////////////////////////////////////////////////////
// FileName:    HelloWnd.h
// Created:     2009/06/05
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: Ö÷´°¿ÚÀà
///////////////////////////////////////////////////////////////////////////////

#pragma once

#include <pdl_window.h>

class CHelloWnd : public LWindow
{
public:
    CHelloWnd(LPWNDCLASS wc);
private:
    void OnCommand(WORD wNotifyCode, WORD wID, HWND hWndCtrl, BOOL& bHandled);
    void OnDestroy(BOOL& bHandled);
    void OnPaint(BOOL& bHandled);
};
