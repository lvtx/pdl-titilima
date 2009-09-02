#include "..\..\include\pdl_menu.h"
#include "..\..\include\pdl_module.h"
#ifdef _WIN32_WCE
#include <aygshell.h>
#endif // _WIN32_WCE

//////////////////////////////////////////////////////////////////////////
// LMenu

LMenu::LMenu(__in HMENU hMenu /* = NULL */) : m_hMenu(hMenu)
{
}

LMenu::operator HMENU(void)
{
    return (HMENU)m_hMenu;
}

LMenu& LMenu::operator=(__in HMENU hMenu)
{
    m_hMenu = hMenu;
    return *this;
}

DWORD LMenu::CheckItem(__in UINT uIDCheckItem, __in UINT uCheck)
{
    return ::CheckMenuItem(m_hMenu, uIDCheckItem, uCheck);
}

#ifdef _WIN32_WCE

HWND PDLAPI LMenu::CreateBarCE(__in HWND hParent, __in UINT uId)
{
    LAppModule *theApp = LAppModule::GetApp();

    SHMENUBARINFO mbi = { 0 };
    mbi.cbSize = sizeof(SHMENUBARINFO);
    mbi.hwndParent = hParent;
    mbi.nToolBarId = uId;
    mbi.hInstRes = theApp->GetInstance();

    if (!SHCreateMenuBar(&mbi))
        return NULL;
    else
        return mbi.hwndMB;
}

#endif // _WIN32_WCE

BOOL LMenu::EnableItem(__in UINT uIDEnableItem, __in UINT uEnable)
{
    return ::EnableMenuItem(m_hMenu, uIDEnableItem, uEnable);
}

BOOL LMenu::GetItemInfo(
    __in UINT uItem,
    __in BOOL fByPosition,
    __inout LPMENUITEMINFOA lpmii)
{
#ifdef _WIN32_WCE
    return FALSE;
#else
    return ::GetMenuItemInfoA(m_hMenu, uItem, fByPosition, lpmii);
#endif // _WIN32_WCE
}

BOOL LMenu::GetItemInfo(
    __in UINT uItem,
    __in BOOL fByPosition,
    __inout LPMENUITEMINFOW lpmii)
{
    return ::GetMenuItemInfoW(m_hMenu, uItem, fByPosition, lpmii);
}

HMENU LMenu::GetSub(__in int nPos)
{
    return ::GetSubMenu(m_hMenu, nPos);
}

BOOL LMenu::SetItemInfo(
    __in UINT uItem,
    __in BOOL fByPosition,
    __in LPMENUITEMINFOA lpmii)
{
#ifdef _WIN32_WCE
    return FALSE;
#else
    return ::SetMenuItemInfoA(m_hMenu, uItem, fByPosition, lpmii);
#endif // _WIN32_WCE
}

BOOL LMenu::SetItemInfo(
    __in UINT uItem,
    __in BOOL fByPosition,
    __in LPMENUITEMINFOW lpmii)
{
    return ::SetMenuItemInfoW(m_hMenu, uItem, fByPosition, lpmii);
}

BOOL LMenu::TrackPopup(
    __in UINT uFlags,
    __in int x, __in int y,
    __in HWND hWnd)
{
#ifdef _WIN32_WCE
    return ::TrackPopupMenuEx(m_hMenu, uFlags, x, y, hWnd, NULL);
#else
    return ::TrackPopupMenu(m_hMenu, uFlags, x, y, 0, hWnd, NULL);
#endif // _WIN32_WCE
}
