///////////////////////////////////////////////////////////////////////////////
// FileName:    AboutDlg.h
// Created:     2009/07/11
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 关于对话框
///////////////////////////////////////////////////////////////////////////////

#pragma once

#include <pdl_window.h>
#include <pdl_message.h>

class CAboutDlg : public LDialog
{
public:
    CAboutDlg(void);
private:
    PDL_DECLARE_MSGMAP();
    DECLARE_COMMAND_HANDLER(OnCommand);
};
