///////////////////////////////////////////////////////////////////////////////
// �ļ�����  PageStart.h
// ����ʱ�䣺2009-01-16
// ���ߣ�    ����
// ��Ȩ���У�Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// ˵����    ����ʼҳ��
///////////////////////////////////////////////////////////////////////////////

#pragma once

#include <pdl_commctrl.h>

class CPageStart : public LPropSheetPage
{
public:
    CPageStart(void);
private:
    BOOL OnInitDialog(HWND hCtrlFocus, LPARAM lParam, BOOL& bHandled);
    void OnCommand(WORD wNotifyCode, WORD wID, HWND hWndCtrl,
        BOOL& bHandled);
    LRESULT OnNotify(int idCtrl, LPNMHDR pnmh,
        BOOL& bHandled);
private:
    int m_nSel;
    LListView m_lstType;
    LImageList m_imlType;
};
