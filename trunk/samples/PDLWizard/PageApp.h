///////////////////////////////////////////////////////////////////////////////
// �ļ�����  PageApp.h
// ����ʱ�䣺2009-01-18
// ���ߣ�    ����
// ��Ȩ���У�Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// ˵����    Ӧ�ó���ҳ��
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
