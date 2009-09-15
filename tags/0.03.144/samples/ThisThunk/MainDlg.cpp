#include "MainDlg.h"

#include "resource.h"

CMainDlg::CMainDlg(void) : LDialog(IDD_DLG_MAIN)
{
    m_thunk.CreateThunk(this, &CMainDlg::ThreadProc);
}

CMainDlg::~CMainDlg(void)
{
    m_thunk.DestroyThunk();
}

void CMainDlg::OnClose(BOOL& bHandled)
{
    EndDialog(0);
}

void CMainDlg::OnCommand(
    WORD wNotifyCode,
    WORD wID,
    HWND hWndCtrl,
    BOOL& bHandled)
{
    switch (wID)
    {
    case IDC_BTN_START:
        {
            HANDLE hThread = ::CreateThread(NULL, 0, m_thunk, NULL, 0, NULL);
            ::CloseHandle(hThread);
        }
        break;
    case IDOK:
        {
            EndDialog(0);
        }
        break;
    default:
        bHandled = FALSE;
    }
}

BOOL CMainDlg::OnInitDialog(HWND hCtrlFocus, LPARAM lParam, BOOL& bHandled)
{
    m_pb = GetDlgItem(IDC_PROGRESS);
    m_pb.SetRange32(0, 10);
    m_pb.SetStep(1);
    return FALSE;
}

DWORD WINAPI CMainDlg::ThreadProc(PVOID param)
{
    m_pb.SetPos(0);
    for (int i = 0; i < 10; ++i)
    {
        m_pb.StepIt();
        ::Sleep(1000);
    }
    return 0;
}
