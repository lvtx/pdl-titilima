#pragma once

#include <pdl_window.h>
#include <pdl_commctrl.h>

class CMainDlg : public LDialog
{
public:
    CMainDlg(void);
    ~CMainDlg(void);
private:
    PDL_DECLARE_MSGMAP();
    DECLARE_CLOSE_HANDLER(OnClose);
    DECLARE_COMMAND_HANDLER(OnCommand);
    DECLARE_INITDIALOG_HANDLER(OnInitDialog);
    DWORD WINAPI ThreadProc(PVOID param);
private:
    LProgressBar m_pb;
    LThisThunk<LPTHREAD_START_ROUTINE> m_thunk;
};
