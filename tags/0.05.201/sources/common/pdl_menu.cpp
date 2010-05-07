#include "..\..\include\pdl_menu.h"
#include "..\..\include\pdl_module.h"
#include "..\..\include\pdl_string.h"
#include "..\..\include\pdl_parser.h"
#ifdef _WIN32_WCE
#include <aygshell.h>
#endif // _WIN32_WCE

//////////////////////////////////////////////////////////////////////////
// LMenu

LMenu::LMenu(__in HMENU hMenu /* = NULL */) : m_hMenu(hMenu)
{
}

LMenu::operator HMENU(void) const
{
    return m_hMenu;
}

LMenu& LMenu::operator=(__in HMENU hMenu)
{
    m_hMenu = hMenu;
    return *this;
}

DWORD LMenu::CheckItem(__in UINT uIDCheckItem, __in UINT uCheck)
{
    PDLASSERT(IsMenu());
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

BOOL LMenu::Destroy(void)
{
    PDLASSERT(IsMenu());
    return ::DestroyMenu(m_hMenu);
}

BOOL LMenu::EnableItem(__in UINT uIDEnableItem, __in UINT uEnable)
{
    PDLASSERT(IsMenu());
    return ::EnableMenuItem(m_hMenu, uIDEnableItem, uEnable);
}

BOOL LMenu::GetItemInfo(
    __in UINT uItem,
    __in BOOL fByPosition,
    __inout LPMENUITEMINFOA lpmii)
{
    PDLASSERT(IsMenu());
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
    PDLASSERT(IsMenu());
    return ::GetMenuItemInfoW(m_hMenu, uItem, fByPosition, lpmii);
}

HMENU LMenu::GetSub(__in int nPos)
{
    PDLASSERT(IsMenu());
    return ::GetSubMenu(m_hMenu, nPos);
}

BOOL LMenu::IsMenu(void)
{
    return ::IsMenu(m_hMenu);
}

BOOL LMenu::LoadLanguageRes(__in LIniParser* lan, __in PCSTR name)
{
    if (NULL == lan || NULL == name || '\0' == *name)
        return FALSE;

    int idxPopup = 0;
    LoadLanguageRes(lan, name, m_hMenu, idxPopup);
    return TRUE;
}

void LMenu::LoadLanguageRes(
    LIniParser* lan,
    PCSTR name,
    HMENU hSubMenu,
    int& idxPopup)
{
    MENUITEMINFO mii = { 0 };
    mii.cbSize = sizeof(MENUITEMINFO);
    mii.fMask = MIIM_ID | MIIM_TYPE | MIIM_SUBMENU;

    char key[16];
    LString str;
    int cnt = ::GetMenuItemCount(hSubMenu);
    for (int i = 0; i < cnt; ++i)
    {
        ::GetMenuItemInfo(hSubMenu, i, TRUE, &mii);
        if (NULL != mii.hSubMenu)
        {
            wsprintfA(key, "popup%d", idxPopup++);
            lan->GetString(name, key, _T(""), &str);
            if (!str.IsEmpty())
            {
                str.ReplaceBackslashChars();
                ::ModifyMenu(hSubMenu, i, MF_BYPOSITION | MF_STRING,
                    (UINT_PTR)mii.hSubMenu, str);
            }

            LoadLanguageRes(lan, name, mii.hSubMenu, idxPopup);
        }
        else if (MFT_SEPARATOR != mii.fType)
        {
            wsprintfA(key, "%d", mii.wID);
            lan->GetString(name, key, _T(""), &str);
            if (!str.IsEmpty())
            {
                str.ReplaceBackslashChars();
                ::ModifyMenu(hSubMenu, mii.wID, MF_BYCOMMAND | MF_STRING,
                    mii.wID, str);
            }
        }
    }
}

BOOL LMenu::Remove(__in UINT uPosition, __in UINT uFlags)
{
    PDLASSERT(IsMenu());
    return ::RemoveMenu(m_hMenu, uPosition, uFlags);
}

BOOL LMenu::SetDefaultItem(__in UINT uItem, __in UINT fByPos)
{
    PDLASSERT(IsMenu());
    return ::SetMenuDefaultItem(m_hMenu, uItem, fByPos);
}

BOOL LMenu::SetItemInfo(
    __in UINT uItem,
    __in BOOL fByPosition,
    __in LPMENUITEMINFOA lpmii)
{
    PDLASSERT(IsMenu());
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
    PDLASSERT(IsMenu());
    return ::SetMenuItemInfoW(m_hMenu, uItem, fByPosition, lpmii);
}

///////////////////////////////////////////////////////////////////////////////
// LPopupMenu

LPopupMenu::LPopupMenu(__in HMENU hMenu /* = NULL */) : LMenu(hMenu)
{
}

LPopupMenu::~LPopupMenu(void)
{
    if (NULL != m_hMenu)
        Destroy();
}

LPopupMenu::operator HMENU(void) const
{
    return m_hMenu;
}

LPopupMenu& LPopupMenu::operator=(__in HMENU hMenu)
{
    m_hMenu = hMenu;
    return *this;
}

BOOL LPopupMenu::Create(void)
{
    PDLASSERT(NULL == m_hMenu);
    m_hMenu = ::CreatePopupMenu();
    return NULL != m_hMenu;
}

BOOL LPopupMenu::Track(
    __in UINT uFlags,
    __in int x, __in int y,
    __in HWND hWnd)
{
    PDLASSERT(IsMenu());
#ifdef _WIN32_WCE
    return ::TrackPopupMenuEx(m_hMenu, uFlags, x, y, hWnd, NULL);
#else
    return ::TrackPopupMenu(m_hMenu, uFlags, x, y, 0, hWnd, NULL);
#endif // _WIN32_WCE
}
