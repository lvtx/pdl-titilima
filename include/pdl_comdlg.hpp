/**
 * \file pdl_comdlg.hpp
 * \brief PDL 公共对话框封装
 * \details 对公共对话框的功能封装：
 *   \li \c LFileDialogT
 *   \li \c LFontDialogT
 */

#pragma once

#include "pdl_base.h"
#include <CommDlg.h>

template<typename T>
class LComDlgTraits;

template<>
class LComDlgTraits<char>
{
public:
    typedef char CT, *PT;
    typedef const char *PCT;
    // OPENFILENAME
    typedef OPENFILENAMEA OFNT;
    static BOOL WINAPI GOFNT(OFNT* ofn)
    {
        return ::GetOpenFileNameA(ofn);
    }
    static BOOL WINAPI GSFNT(OFNT* ofn)
    {
        return ::GetSaveFileNameA(ofn);
    }
    // CHOOSEFONT
    typedef LOGFONTA LFT;
    typedef CHOOSEFONTA CFT;
    static BOOL WINAPI CF(CFT* cf)
    {
        return ::ChooseFontA(cf);
    }
};

template<>
class LComDlgTraits<WCHAR>
{
public:
    typedef WCHAR CT, *PT;
    typedef const WCHAR *PCT;
    // OPENFILENAME
    typedef OPENFILENAMEW OFNT;
    static BOOL WINAPI GOFNT(OFNT* ofn)
    {
        return ::GetOpenFileNameW(ofn);
    }
    static BOOL WINAPI GSFNT(OFNT* ofn)
    {
        return ::GetSaveFileNameW(ofn);
    }
    // CHOOSEFONT
    typedef LOGFONTW LFT;
    typedef CHOOSEFONTW CFT;
    static BOOL WINAPI CF(CFT* cf)
    {
        return ::ChooseFontW(cf);
    }
};

template< typename T, typename Traits = LComDlgTraits<T> >
class LFileDialogT
{
public:
    LFileDialogT(__in BOOL bOpenFileDialog,
        __in_opt typename Traits::PCT lpszFilter = NULL,
        __in_opt typename Traits::PCT lpszDefExt = NULL)
    {
        m_ofn = new Traits::OFNT;

        ZeroMemory(m_szFileName, sizeof(m_szFileName));
        ZeroMemory(m_ofn, sizeof(Traits::OFNT));
        m_ofn->lStructSize = sizeof(Traits::OFNT);
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
    ~LFileDialogT(void)
    {
        delete m_ofn;
    }
public:
    BOOL DoModal(__in HWND hParent,
        __in typename Traits::PCT lpszTitle = NULL)
    {
        m_ofn->hwndOwner = hParent;
        m_ofn->lpstrTitle = lpszTitle;
        if (m_bOpenFileDialog)
            return Traits::GOFNT(m_ofn);
        else
            return Traits::GSFNT(m_ofn);
    }
public:
    typename Traits::CT m_szFileName[MAX_PATH];
    typename Traits::OFNT* m_ofn;
protected:
    BOOL m_bOpenFileDialog;
};

template< typename T, typename Traits = LComDlgTraits<T> >
class LFontDialogT
{
public:
    LFontDialogT(__in_opt typename Traits::LFT* lfInitial = NULL,
        __in_opt DWORD dwFlags = CF_EFFECTS | CF_SCREENFONTS,
        __in_opt HDC hdc = NULL)
    {
        m_lf = new Traits::LFT;
        if (NULL != lfInitial)
            CopyMemory(m_lf, lfInitial, sizeof(Traits::LFT));
        else
            ZeroMemory(m_lf, sizeof(Traits::LFT));

        m_cf = new Traits::CFT;
        ZeroMemory(m_cf, sizeof(Traits::CFT));
        m_cf->lStructSize = sizeof(Traits::CFT);
        m_cf->hDC = hdc;
        m_cf->lpLogFont = m_lf;
        m_cf->Flags = dwFlags;
    }
    ~LFontDialogT(void)
    {
        delete m_cf;
        delete m_lf;
    }
public:
    BOOL DoModal(__in HWND hParent)
    {
        m_cf->hwndOwner = hParent;
        return Traits::CF(m_cf);
    }
public:
    typename Traits::LFT* m_lf;
    typename Traits::CFT* m_cf;
};

typedef LFileDialogT<char> LFileDialogA;
typedef LFileDialogT<WCHAR> LFileDialogW;
typedef LFontDialogT<char> LFontDialogA;
typedef LFontDialogT<WCHAR> LFontDialogW;

#ifdef UNICODE
typedef LFileDialogW LFileDialog;
typedef LFontDialogW LFontDialog;
#else
typedef LFileDialogA LFileDialog;
typedef LFontDialogA LFontDialog;
#endif // UNICODE
