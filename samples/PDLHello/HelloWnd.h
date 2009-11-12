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
    PDL_DECLARE_MSGMAP();
    DECLARE_COMMAND_HANDLER(OnCommand);
    DECLARE_DESTROY_HANDLER(OnDestroy);
    DECLARE_PAINT_HANDLER(OnPaint);
    void OnMsgProcceded(UINT uMsg, WPARAM wParam, LPARAM lParam, LRESULT lRet);
};
