/**
 * \file pdl_menu.h
 * \brief PDL 菜单封装类 
 */

#pragma once

#include "pdl_base.h"

/**
 * \class LMenu
 * \brief PDL 菜单封装类
 */

#ifdef _WIN32_WCE
#undef TrackPopupMenu
#endif // _WIN32_WCE

class LIniParser;
class LMenu
{
public:
    LMenu(__in HMENU hMenu = NULL);
    operator HMENU(void);
    LMenu& operator=(__in HMENU hMenu);
public:
    DWORD CheckItem(__in UINT uIDCheckItem, __in UINT uCheck);
#ifdef _WIN32_WCE
    static HWND PDLAPI CreateBarCE(__in HWND hParent, __in UINT uId);
#endif // _WIN32_WCE
    BOOL EnableItem(__in UINT uIDEnableItem, __in UINT uEnable);
    BOOL GetItemInfo(__in UINT uItem, __in BOOL fByPosition,
        __inout LPMENUITEMINFOA lpmii);
    BOOL GetItemInfo(__in UINT uItem, __in BOOL fByPosition,
        __inout LPMENUITEMINFOW lpmii);
    HMENU GetSub(__in int nPos);
    BOOL LoadLanguageRes(__in LIniParser* lan, __in PCSTR name);
    BOOL SetDefaultItem(__in UINT uItem, __in UINT fByPos);
    BOOL SetItemInfo(__in UINT uItem, __in BOOL fByPosition,
        __in LPMENUITEMINFOA lpmii);
    BOOL SetItemInfo(__in UINT uItem, __in BOOL fByPosition,
        __in LPMENUITEMINFOW lpmii);
    BOOL TrackPopup(__in UINT uFlags, __in int x, __in int y,
        __in HWND hWnd);
protected:
    static void LoadLanguageRes(LIniParser* lan, PCSTR name, HMENU hSubMenu,
        int& idxPopup);
protected:
    HMENU m_hMenu;
};
