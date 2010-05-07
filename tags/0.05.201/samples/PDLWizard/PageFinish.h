///////////////////////////////////////////////////////////////////////////////
// 文件名：  PageFinish.h
// 创建时间：2009-01-19
// 作者：    李马
// 版权所有：Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// 说明：    完成页面
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
