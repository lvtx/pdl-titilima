///////////////////////////////////////////////////////////////////////////////
// FileName:    AboutDlg.h
// Created:     2009/07/11
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: ���ڶԻ���
///////////////////////////////////////////////////////////////////////////////

#pragma once

#include <pdl_window.h>

class CAboutDlg : public LDialog
{
public:
    CAboutDlg(void);
private:
    void OnCommand(WORD wNotifyCode, WORD wID, HWND hWndCtrl, BOOL& bHandled);
};
