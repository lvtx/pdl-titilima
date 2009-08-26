///////////////////////////////////////////////////////////////////////////////
// 文件名：  PageApp.h
// 创建时间：2009-01-18
// 作者：    李马
// 版权所有：Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// 说明：    应用程序页面
///////////////////////////////////////////////////////////////////////////////

#pragma once

#include <pdl_commctrl.h>

class CPageApp : public LPropSheetPage
{
public:
    CPageApp(void);
private:
    BOOL OnInitDialog(HWND hCtrlFocus, LPARAM lParam, BOOL& bHandled);
    void OnCommand(WORD wNotifyCode, WORD wID, HWND hWndCtrl, BOOL& bHandled);
    LRESULT OnNotify(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);
};
