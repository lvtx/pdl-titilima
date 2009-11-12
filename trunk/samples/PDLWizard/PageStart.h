///////////////////////////////////////////////////////////////////////////////
// 文件名：  PageStart.h
// 创建时间：2009-01-16
// 作者：    李马
// 版权所有：Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// 说明：    向导起始页面
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
