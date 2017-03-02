/**
 * \file pdl_comdlg.h
 * \brief PDL 公共对话框封装
 * \details 对公共对话框的功能封装：
 *   \li \c LFileDialog
 */

#pragma once

#include "pdl_base.h"
#include <CommDlg.h>

class LFileDialogA
{
public:
    LFileDialogA(__in BOOL bOpenFileDialog,
        __in_opt PCSTR lpszFilter = NULL,
        __in_opt PCSTR lpszDefExt = NULL);
    ~LFileDialogA(void);
public:
    BOOL DoModal(__in HWND hParent, __in PCSTR lpszTitle = NULL);
public:
    CHAR m_szFileName[MAX_PATH];
    LPOPENFILENAMEA m_ofn;
protected:
    BOOL m_bOpenFileDialog;
};

class LFileDialogW
{
public:
    LFileDialogW(__in BOOL bOpenFileDialog,
        __in_opt PCWSTR lpszFilter = NULL,
        __in_opt PCWSTR lpszDefExt = NULL);
    ~LFileDialogW(void);
public:
    BOOL DoModal(__in HWND hParent, __in PCWSTR lpszTitle = NULL);
public:
    WCHAR m_szFileName[MAX_PATH];
    LPOPENFILENAMEW m_ofn;
protected:
    BOOL m_bOpenFileDialog;
};

#ifdef UNICODE
typedef LFileDialogW LFileDialog;
#else
typedef LFileDialogA LFileDialog;
#endif // UNICODE
