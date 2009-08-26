#pragma once

#include <pdl_window.h>
#include <pdl_commctrl.h>

class CMainDlg : public LDialog
{
public:
    CMainDlg(void);
    ~CMainDlg(void);
private:
    BOOL OnInitDialog(HWND hCtrlFocus, LPARAM lParam, BOOL& bHandled);
    void OnClose(BOOL& bHandled);
    void OnCommand(WORD wNotifyCode, WORD wID, HWND hWndCtrl, BOOL& bHandled);
    DWORD WINAPI ThreadProc(PVOID param);
private:
    LProgressBar m_pb;
    LThisThunk<LPTHREAD_START_ROUTINE> m_thunk;
};
