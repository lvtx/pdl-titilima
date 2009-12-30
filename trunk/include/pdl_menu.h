/**
 * \file pdl_menu.h
 * \brief PDL �˵���װ
 * \details ����ļ��а����˶Բ˵��ĸ��ַ�װ��
 *   \li \c LMenu PDL �˵���װ��
 *   \li \c LPopupMenu PDL �����˵���װ��
 */

#pragma once

#include "pdl_base.h"

/**
 * \class LMenu
 * \brief PDL �˵���װ��
 */

#ifdef _WIN32_WCE
#undef TrackPopupMenu
#endif // _WIN32_WCE

class LIniParser;
class LMenu
{
public:
    LMenu(__in HMENU hMenu = NULL);
    operator HMENU(void) const;
    LMenu& operator=(__in HMENU hMenu);
public:
    DWORD CheckItem(__in UINT uIDCheckItem, __in UINT uCheck);
#ifdef _WIN32_WCE
    static HWND PDLAPI CreateBarCE(__in HWND hParent, __in UINT uId);
#endif // _WIN32_WCE
    BOOL Destroy(void);
    BOOL EnableItem(__in UINT uIDEnableItem, __in UINT uEnable);
    BOOL GetItemInfo(__in UINT uItem, __in BOOL fByPosition,
        __inout LPMENUITEMINFOA lpmii);
    BOOL GetItemInfo(__in UINT uItem, __in BOOL fByPosition,
        __inout LPMENUITEMINFOW lpmii);
    HMENU GetSub(__in int nPos);
    BOOL IsMenu(void);
    BOOL LoadLanguageRes(__in LIniParser* lan, __in PCSTR name);
    BOOL Remove(__in UINT uPosition, __in UINT uFlags);
    BOOL SetDefaultItem(__in UINT uItem, __in UINT fByPos);
    BOOL SetItemInfo(__in UINT uItem, __in BOOL fByPosition,
        __in LPMENUITEMINFOA lpmii);
    BOOL SetItemInfo(__in UINT uItem, __in BOOL fByPosition,
        __in LPMENUITEMINFOW lpmii);
protected:
    static void LoadLanguageRes(LIniParser* lan, PCSTR name, HMENU hSubMenu,
        int& idxPopup);
protected:
    HMENU m_hMenu;
};

/**
 * \class LPopupMenu
 * \brief PDL �����˵���װ��
 */

class LPopupMenu : public LMenu
{
public:
    LPopupMenu(__in HMENU hMenu = NULL);
    ~LPopupMenu(void);
    operator HMENU(void) const;
    LPopupMenu& operator=(__in HMENU hMenu);
public:
    BOOL Create(void);
    BOOL Track(__in UINT uFlags, __in int x, __in int y, __in HWND hWnd);
};
