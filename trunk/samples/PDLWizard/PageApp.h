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
    PDL_DECLARE_MSGMAP();
    DECLARE_COMMAND_HANDLER(OnCommand);
    DECLARE_INITDIALOG_HANDLER(OnInitDialog);
    DECLARE_NOTIFY_HANDLER(OnNotify);
};
