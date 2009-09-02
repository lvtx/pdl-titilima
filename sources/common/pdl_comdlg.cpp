#include "..\..\include\pdl_comdlg.h"
#ifdef _WIN32_WCE
#include "..\adaptor\wince_adaptor.h"
#endif // _WIN32_WCE

///////////////////////////////////////////////////////////////////////////////
// LFileDialogA

LFileDialogA::LFileDialogA(
    __in BOOL bOpenFileDialog,
    __in PCSTR lpszFilter /* = NULL */ ,
    __in PCSTR lpszDefExt /* = NULL */) : m_bOpenFileDialog(bOpenFileDialog)
{
    m_ofn = new OPENFILENAMEA;

    ZeroMemory(m_szFileName, sizeof(m_szFileName));
    ZeroMemory(m_ofn, sizeof(OPENFILENAMEA));
    m_ofn->lStructSize = sizeof(OPENFILENAMEA);
    m_ofn->lpstrFilter = lpszFilter;
    m_ofn->lpstrFile = m_szFileName;
    m_ofn->nMaxFile = MAX_PATH;
    m_ofn->lpstrDefExt = lpszDefExt;
    if (m_bOpenFileDialog)
    {
        m_ofn->Flags = OFN_ENABLESIZING | OFN_EXPLORER | OFN_FILEMUSTEXIST
            | OFN_HIDEREADONLY | OFN_LONGNAMES | OFN_PATHMUSTEXIST;
    }
    else
    {
        m_ofn->Flags = OFN_ENABLESIZING | OFN_EXPLORER | OFN_FILEMUSTEXIST
            | OFN_HIDEREADONLY | OFN_LONGNAMES | OFN_OVERWRITEPROMPT;
    }
}

LFileDialogA::~LFileDialogA(void)
{
    delete m_ofn;
}

BOOL LFileDialogA::DoModal(__in HWND hParent, __in PCSTR lpszTitle /* = NULL */)
{
    m_ofn->hwndOwner = hParent;
    m_ofn->lpstrTitle = lpszTitle;
    if (m_bOpenFileDialog)
        return ::GetOpenFileNameA(m_ofn);
    else
        return ::GetSaveFileNameA(m_ofn);
}

///////////////////////////////////////////////////////////////////////////////
// LFileDialogW

LFileDialogW::LFileDialogW(
    __in BOOL bOpenFileDialog,
    __in PCWSTR lpszFilter /* = NULL */,
    __in PCWSTR lpszDefExt /* = NULL */) : m_bOpenFileDialog(bOpenFileDialog)
{
    m_ofn = new OPENFILENAMEW;

    ZeroMemory(m_szFileName, sizeof(m_szFileName));
    ZeroMemory(m_ofn, sizeof(OPENFILENAMEW));
    m_ofn->lStructSize = sizeof(OPENFILENAMEW);
    m_ofn->lpstrFilter = lpszFilter;
    m_ofn->lpstrFile = m_szFileName;
    m_ofn->nMaxFile = MAX_PATH;
    m_ofn->lpstrDefExt = lpszDefExt;
    if (m_bOpenFileDialog)
    {
        m_ofn->Flags = OFN_ENABLESIZING | OFN_EXPLORER | OFN_FILEMUSTEXIST
            | OFN_HIDEREADONLY | OFN_LONGNAMES | OFN_PATHMUSTEXIST;
    }
    else
    {
        m_ofn->Flags = OFN_ENABLESIZING | OFN_EXPLORER | OFN_FILEMUSTEXIST
            | OFN_HIDEREADONLY | OFN_LONGNAMES | OFN_OVERWRITEPROMPT;
    }
}

LFileDialogW::~LFileDialogW(void)
{
    delete m_ofn;
}

BOOL LFileDialogW::DoModal(__in HWND hParent, __in PCWSTR lpszTitle /* = NULL */)
{
    m_ofn->hwndOwner = hParent;
    m_ofn->lpstrTitle = lpszTitle;
    if (m_bOpenFileDialog)
        return ::GetOpenFileNameW(m_ofn);
    else
        return ::GetSaveFileNameW(m_ofn);
}

///////////////////////////////////////////////////////////////////////////////
