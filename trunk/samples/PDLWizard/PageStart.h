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
    PDL_DECLARE_MSGMAP();
    DECLARE_COMMAND_HANDLER(OnCommand);
    DECLARE_INITDIALOG_HANDLER(OnInitDialog);
    DECLARE_NOTIFY_HANDLER(OnNotify);
private:
    int m_nSel;
    LListView m_lstType;
    LImageList m_imlType;
};
