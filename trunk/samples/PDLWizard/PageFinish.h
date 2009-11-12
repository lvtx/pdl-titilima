///////////////////////////////////////////////////////////////////////////////
// �ļ�����  PageFinish.h
// ����ʱ�䣺2009-01-19
// ���ߣ�    ����
// ��Ȩ���У�Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// ˵����    ���ҳ��
///////////////////////////////////////////////////////////////////////////////

#pragma once

#include <pdl_ctrl.h>
#include <pdl_commctrl.h>

class CPageFinish : public LPropSheetPage
{
public:
    CPageFinish(void);
private:
    void AddText(__in PCSTR lpText);
    void FillConfig(void);
private:
    PDL_DECLARE_MSGMAP();
    DECLARE_INITDIALOG_HANDLER(OnInitDialog);
    DECLARE_NOTIFY_HANDLER(OnNotify);
private:
    LEdit m_edtInfo;
};
