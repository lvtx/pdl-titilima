/**
 * \file pdl_message.h
 * \brief PDL 消息处理器
 * \details 这个文件中包含了对各种 Windows 消息的支持。
 */

#pragma once

#define PDL_DECLARE_MSGMAP()            \
    LRESULT OnMessage(UINT uMsg,        \
        WPARAM wParam,                  \
        LPARAM lParam,                  \
        BOOL& bHandled);

#define PDL_BEGIN_MSGMAP(Class)         \
    LRESULT Class::OnMessage(           \
        UINT uMsg,                      \
        WPARAM wParam,                  \
        LPARAM lParam,                  \
        BOOL& bHandled) {               \
        LRESULT ret = 0;                \
        bHandled = TRUE;                \
        if (FALSE) {                    \
    }

#define PDL_END_MSGMAP(Base)            \
        else {                          \
            ret = Base::OnMessage(uMsg, \
                wParam,                 \
                lParam,                 \
                bHandled);              \
        }                               \
        return ret;                     \
    }

///////////////////////////////////////////////////////////////////////////////

// Generic Message

#define DECLARE_MESSAGE_HANDLER(fn)     \
    LRESULT fn(UINT uMsg,               \
        WPARAM wParam,                  \
        LPARAM lParam,                  \
        BOOL& bHandled);
#define DECLAREV_MESSAGE_HANDLER(fn)    \
    virtual LRESULT fn(UINT uMsg,       \
        WPARAM wParam,                  \
        LPARAM lParam,                  \
        BOOL& bHandled);
#define PROCESS_MESSAGE(msg, fn)        \
    else if (msg == uMsg) {             \
        ret = fn(uMsg, wParam, lParam,  \
            bHandled);                  \
    }

// WM_CAPTURECHANGED

#define DECLARE_CAPTURECHANGED_HANDLER(fn)  \
    void fn(HWND hWnd, BOOL& bHandled);
#define DECLAREV_CAPTURECHANGED_HANDLER(fn) \
    virtual void fn(HWND hWnd,              \
        BOOL& bHandled);
#define PROCESS_CAPTURECHANGED(fn)          \
    else if (WM_CAPTURECHANGED == uMsg) {   \
        fn((HWND)lParam, bHandled);         \
    }
#define DEFAULT_CAPTURECHANGED_HANDLER(h)   \
    DoDefault(WM_CAPTURECHANGED, 0,         \
        (LPARAM)h)

// WM_CHAR

#define DECLARE_CHAR_HANDLER(fn)                \
    void fn(UINT nChar, UINT nRepCnt,           \
        UINT nFlags, BOOL& bHandled);
#define DECLAREV_CHAR_HANDLER(fn)               \
    virtual void fn(UINT nChar, UINT nRepCnt,   \
        UINT nFlags, BOOL& bHandled);
#define PROCESS_CHAR(fn)                        \
    else if (WM_CHAR == uMsg) {                 \
        fn((UINT)wParam,                        \
            LOWORD(lParam),                     \
            HIWORD(lParam),                     \
            bHandled);                          \
    }
#define DEFAULT_CHAR_HANDLER(ch, r, flags)      \
    DoDefault(WM_CHAR, (WPARAM)ch,              \
        MAKELPARAM(r, flags))

// WM_CLOSE

#define DECLARE_CLOSE_HANDLER(fn)       \
    void fn(BOOL& bHandled);
#define DECLAREV_CLOSE_HANDLER(fn)      \
    virtual void fn(BOOL& bHandled);
#define PROCESS_CLOSE(fn)               \
    else if (WM_CLOSE == uMsg) {        \
        fn(bHandled);                   \
    }
#define DEFAULT_CLOSE_HANDLER()         \
    DoDefault(WM_CLOSE, 0, 0)

// WM_COMMAND

#define DECLARE_COMMAND_HANDLER(fn)         \
    void fn(WORD wNotifyCode, WORD wID,     \
        HWND hWndCtrl, BOOL& bHandled);
#define DECLAREV_COMMAND_HANDLER(fn)        \
    virtual void fn(WORD wNotifyCode,       \
        WORD wID, HWND hWndCtrl,            \
        BOOL& bHandled);
#define PROCESS_COMMAND(fn)                 \
    else if (WM_COMMAND == uMsg) {          \
        fn(HIWORD(wParam),                  \
            LOWORD(wParam),                 \
            (HWND)lParam,                   \
            bHandled);                      \
    }
#define DEFAULT_COMMAND_HANDLER(n, id, h)   \
    DoDefault(WM_COMMAND,                   \
        MAKEWPARAM(id, n),                  \
        (LPARAM)h)

// WM_CONTEXTMENU

#define DECLARE_CONTEXTMENU_HANDLER(fn)         \
    void fn(HWND hWnd, int x, int y,            \
        BOOL& bHandled);
#define DECLAREV_CONTEXTMENU_HANDLER(fn)        \
    virtual void fn(HWND hWnd, int x, int y,    \
        BOOL& bHandled);
#define PROCESS_CONTEXTMENU(fn)                 \
    else if (WM_CONTEXTMENU == uMsg) {          \
        fn((HWND)wParam,                        \
            LOWORD(lParam),                     \
            HIWORD(lParam),                     \
            bHandled);                          \
    }
#define DEFAULT_CONTEXTMENU_HANDLER(h, x, y)    \
    DoDefault(WM_CONTEXTMENU, (WPARAM)h,        \
        MAKELPARAM(x, y))

// WM_CREATE

#define DECLARE_CREATE_HANDLER(fn)      \
    int fn(LPCREATESTRUCT lpCs,         \
        BOOL& bHandled);
#define DECLAREV_CREATE_HANDLER(fn)     \
    virtual int fn(LPCREATESTRUCT lpCs, \
        BOOL& bHandled);
#define PROCESS_CREATE(fn)              \
    else if (WM_CREATE == uMsg) {       \
        ret = (LRESULT)fn(              \
            (LPCREATESTRUCT)lParam,     \
            bHandled);                  \
    }
#define DEFAULT_CREATE_HANDLER(cs)      \
    DoDefault(WM_CREATE, 0, (LPARAM)cs)

// WM_DESTROY

#define DECLARE_DESTROY_HANDLER(fn)     \
    void fn(BOOL& bHandled);
#define DECLAREV_DESTROY_HANDLER(fn)    \
    virtual void fn(BOOL& bHandled);
#define PROCESS_DESTROY(fn)             \
    else if (WM_DESTROY == uMsg) {      \
        fn(bHandled);                   \
    }
#define DEFAULT_DESTROY_HANDLER()       \
    DoDefault(WM_DESTROY, 0, 0)

// WM_DRAWITEM

#define DECLARE_DRAWITEM_HANDLER(fn)        \
    void fn(int nIDCtl,                     \
        LPDRAWITEMSTRUCT lpDrawItemStruct,  \
        BOOL& bHandled);
#define DECLAREV_DRAWITEM_HANDLER(fn)       \
    virtual void fn(int nIDCtl,             \
        LPDRAWITEMSTRUCT lpDrawItemStruct,  \
        BOOL& bHandled);
#define PROCESS_DRAWITEM(fn)                \
    else if (WM_DRAWITEM == uMsg) {         \
        fn((int)wParam,                     \
            (LPDRAWITEMSTRUCT)lParam,       \
            bHandled);                      \
        ret = bHandled;                     \
    }
#define DEFAULT_DRAWITEM_HANDLER(id, dis)   \
    DoDefault(WM_DRAWITEM, (WPARAM)id,      \
        (LPARAM)dis)

// WM_DROPFILES

#define DECLARE_DROPFILES_HANDLER(fn)   \
    void fn(HDROP hDropInfo,            \
        BOOL& bHandled);
#define DECLAREV_DROPFILES_HANDLER(fn)  \
    virtual void fn(HDROP hDropInfo,    \
        BOOL& bHandled);
#define PROCESS_DROPFILES(fn)           \
    else if (WM_DROPFILES == uMsg) {    \
        fn((HDROP)wParam, bHandled);    \
    }
#define DEFAULT_DROPFILES_HANDLER(hd)   \
    DoDefault(WM_DROPFILES,             \
        (WPARAM)hd), 0)

// WM_ERASEBKGND

#define DECLARE_ERASEBKGND_HANDLER(fn)  \
    BOOL fn(HDC hdc, BOOL& bHandled);
#define DECLAREV_ERASEBKGND_HANDLER(fn) \
    virtual BOOL fn(HDC hdc,            \
        BOOL& bHandled);
#define PROCESS_ERASEBKGND(fn)          \
    else if (WM_ERASEBKGND == uMsg) {   \
        ret = (LRESULT)fn((HDC)wParam,  \
            bHandled);                  \
    }
#define DEFAULT_ERASEBKGND_HANDLER(h)   \
    DoDefault(WM_ERASEBKGND,            \
        (WPARAM)h, 0)

// WM_INITDIALOG

#define DECLARE_INITDIALOG_HANDLER(fn)      \
    BOOL fn(HWND hCtrlFocus,                \
        LPARAM lParam, BOOL& bHandled);
#define DECLAREV_INITDIALOG_HANDLER(fn)     \
    virtual BOOL fn(HWND hCtrlFocus,        \
        LPARAM lParam, BOOL& bHandled);
#define PROCESS_INITDIALOG(fn)              \
    else if (WM_INITDIALOG == uMsg) {       \
        ret = (LRESULT)fn((HWND)wParam,     \
            lParam, bHandled);              \
    }
#define DEFAULT_INITDIALOG_HANDLER(h, lp)   \
    DoDefault(WM_INITDIALOG, (WPARAM)h, lp)

// WM_KEYDOWN

#define DECLARE_KEYDOWN_HANDLER(fn)         \
    void fn(UINT nChar,                     \
        UINT nRepCnt, UINT nFlags,          \
        BOOL& bHandled);
#define DECLAREV_KEYDOWN_HANDLER(fn)        \
    virtual void fn(UINT nChar,             \
        UINT nRepCnt, UINT nFlags,          \
        BOOL& bHandled);
#define PROCESS_KEYDOWN(fn)                 \
    else if (WM_KEYDOWN == uMsg) {          \
        fn((UINT)wParam,                    \
            LOWORD(lParam),                 \
            HIWORD(lParam),                 \
            bHandled);                      \
    }
#define DEFAULT_KEYDOWN_HANDLER(c, r, f)    \
    DoDefault(WM_KEYDOWN, (WPARAM)c,        \
        MAKELPARAM(r, f))

// WM_KEYUP

#define DECLARE_KEYUP_HANDLER(fn)       \
    void fn(UINT nChar,                 \
        UINT nRepCnt, UINT nFlags,      \
        BOOL& bHandled);
#define DECLAREV_KEYUP_HANDLER(fn)      \
    virtual void fn(UINT nChar,         \
        UINT nRepCnt, UINT nFlags,      \
        BOOL& bHandled);
#define PROCESS_KEYUP(fn)               \
    else if (WM_KEYUP == uMsg) {        \
        fn((UINT)wParam,                \
            LOWORD(lParam),             \
            HIWORD(lParam),             \
            bHandled);                  \
    }
#define DEFAULT_KEYUP_HANDLER(c, r, f)  \
    DoDefault(WM_KEYUP, (WPARAM)c,      \
        MAKELPARAM(r, f))

// WM_LBUTTONDBLCLK

#define DECLARE_LBUTTONDBLCLK_HANDLER(fn)       \
    void fn(UINT uFlags, int x, int y,          \
        BOOL& bHandled);
#define DECLAREV_LBUTTONDBLCLK_HANDLER(fn)      \
    virtual void fn(UINT uFlags, int x, int y,  \
        BOOL& bHandled);
#define PROCESS_LBUTTONDBLCLK(fn)               \
    else if (WM_LBUTTONDBLCLK == uMsg) {        \
        fn((UINT)wParam,                        \
            GET_X_LPARAM(lParam),               \
            GET_Y_LPARAM(lParam),               \
            bHandled);                          \
    }
#define DEFAULT_LBUTTONDBLCLK_HANDLER(f, x, y)  \
    DoDefault(WM_LBUTTONDBLCLK, f,              \
        MAKELPARAM(x, y))

// WM_LBUTTONDOWN

#define DECLARE_LBUTTONDOWN_HANDLER(fn)         \
    void fn(UINT uFlags, int x, int y,          \
        BOOL& bHandled);
#define DECLAREV_LBUTTONDOWN_HANDLER(fn)        \
    virtual void fn(UINT uFlags, int x, int y,  \
        BOOL& bHandled);
#define PROCESS_LBUTTONDOWN(fn)                 \
    else if (WM_LBUTTONDOWN == uMsg) {          \
        fn((UINT)wParam,                        \
            GET_X_LPARAM(lParam),               \
            GET_Y_LPARAM(lParam),               \
            bHandled);                          \
    }
#define DEFAULT_LBUTTONDOWN_HANDLER(f, x, y)    \
    DoDefault(WM_LBUTTONDOWN, f,                \
        MAKELPARAM(x, y))

// WM_LBUTTONUP

#define DECLARE_LBUTTONUP_HANDLER(fn)           \
    void fn(UINT uFlags, int x, int y,          \
        BOOL& bHandled);
#define DECLAREV_LBUTTONUP_HANDLER(fn)          \
    virtual void fn(UINT uFlags, int x, int y,  \
        BOOL& bHandled);
#define PROCESS_LBUTTONUP(fn)                   \
    else if (WM_LBUTTONUP == uMsg) {            \
        fn((UINT)wParam,                        \
            GET_X_LPARAM(lParam),               \
            GET_Y_LPARAM(lParam),               \
            bHandled);                          \
    }
#define DEFAULT_LBUTTONUP_HANDLER(f, x, y)      \
    DoDefault(WM_LBUTTONUP, f,                  \
        MAKELPARAM(x, y))

// WM_MOUSELEAVE

#define DECLARE_MOUSELEAVE_HANDLER(fn)  \
    void fn(BOOL& bHandled);
#define DECLAREV_MOUSELEAVE_HANDLER(fn) \
    virtual void fn(BOOL& bHandled);
#define PROCESS_MOUSELEAVE(fn)          \
    else if (WM_MOUSELEAVE == uMsg) {   \
        fn(bHandled);                   \
    }
#define DEFAULT_MOUSELEAVE_HANDLER()    \
    DoDefault(WM_MOUSELEAVE, 0, 0)

// WM_MOUSEMOVE

#define DECLARE_MOUSEMOVE_HANDLER(fn)           \
    void fn(UINT uFlags, int x, int y,          \
        BOOL& bHandled);
#define DECLAREV_MOUSEMOVE_HANDLER(fn)          \
    virtual void fn(UINT uFlags, int x, int y,  \
        BOOL& bHandled);
#define PROCESS_MOUSEMOVE(fn)                   \
    else if (WM_MOUSEMOVE == uMsg) {            \
        fn((UINT)wParam,                        \
            GET_X_LPARAM(lParam),               \
            GET_Y_LPARAM(lParam),               \
            bHandled);                          \
    }
#define DEFAULT_MOUSEMOVE_HANDLER(f, x, y)      \
    DoDefault(WM_MOUSEMOVE, f,                  \
        MAKELPARAM(x, y))

// WM_MOUSEWHEEL

#define DECLARE_MOUSEWHEEL_HANDLER(fn)          \
    void fn(UINT nFlags, short zDelta,          \
        int x, int y, BOOL& bHandled);
#define DECLAREV_MOUSEWHEEL_HANDLER(fn)         \
    virtual void fn(UINT nFlags, short zDelta,  \
        int x, int y, BOOL& bHandled);
#define PROCESS_MOUSEWHEEL(fn)                  \
    else if (WM_MOUSEWHEEL == uMsg) {           \
        fn(LOWORD(wParam), HIWORD(wParam),      \
            LOWORD(lParam), HIWORD(lParam),     \
            bHandled);                          \
    }
#define DEFAULT_MOUSEWHEEL_HANDLER(f, d, x, y)  \
    DoDefault(WM_MOUSEWHEEL, MAKEWPARAM(f, d),  \
        MAKELPARAM(x, y))

// WM_NOTIFY

#define DECLARE_NOTIFY_HANDLER(fn)      \
    LRESULT fn(int idCtrl,              \
        LPNMHDR pnmh, BOOL& bHandled);
#define DECLAREV_NOTIFY_HANDLER(fn)     \
    virtual LRESULT fn(int idCtrl,      \
        LPNMHDR pnmh, BOOL& bHandled);
#define PROCESS_NOTIFY(fn)              \
    else if (WM_NOTIFY == uMsg) {       \
        ret = fn(wParam,                \
            (LPNMHDR)lParam, bHandled); \
    }
#define DEFAULT_NOTIFY_HANDLER(id, hdr) \
    DoDefault(WM_NOTIFY, id,            \
        (LPARAM)hdr)

// WM_PAINT

#define DECLARE_PAINT_HANDLER(fn)       \
    void fn(BOOL& bHandled);
#define DECLAREV_PAINT_HANDLER(fn)      \
    virtual void fn(BOOL& bHandled);
#define PROCESS_PAINT(fn)               \
    else if (WM_PAINT == uMsg) {        \
        fn(bHandled);                   \
    }
#define DEFAULT_PAINT_HANDLER()         \
    DoDefault(WM_PAINT, 0, 0)

// WM_RBUTTONDOWN

#define DECLARE_RBUTTONDOWN_HANDLER(fn)         \
    void fn(UINT uFlags, int x, int y,          \
        BOOL& bHandled);
#define DECLAREV_RBUTTONDOWN_HANDLER(fn)        \
    virtual void fn(UINT uFlags, int x, int y,  \
        BOOL& bHandled);
#define PROCESS_RBUTTONDOWN(fn)                 \
    else if (WM_RBUTTONDOWN == uMsg) {          \
        fn((UINT)wParam,                        \
            GET_X_LPARAM(lParam),               \
            GET_Y_LPARAM(lParam),               \
            bHandled);                          \
    }
#define DEFAULT_RBUTTONDOWN_HANDLER(f, x, y)    \
    DoDefault(WM_RBUTTONDOWN, f,                \
        MAKELPARAM(x, y))

// WM_RBUTTONUP

#define DECLARE_RBUTTONUP_HANDLER(fn)           \
    void fn(UINT uFlags, int x, int y,          \
        BOOL& bHandled);
#define DECLAREV_RBUTTONUP_HANDLER(fn)          \
    virtual void fn(UINT uFlags, int x, int y,  \
        BOOL& bHandled);
#define PROCESS_RBUTTONUP(fn)                   \
    else if (WM_RBUTTONUP == uMsg) {            \
        fn((UINT)wParam,                        \
            GET_X_LPARAM(lParam),               \
            GET_Y_LPARAM(lParam),               \
            bHandled);                          \
    }
#define DEFAULT_RBUTTONUP_HANDLER(f, x, y)      \
    DoDefault(WM_RBUTTONUP, f,                  \
        MAKELPARAM(x, y))

// WM_SETCURSOR

#define DECLARE_SETCURSOR_HANDLER(fn)                   \
    BOOL fn(HWND hWnd, UINT nHitTest,                   \
        UINT message, BOOL& bHandled);
#define DECLAREV_SETCURSOR_HANDLER(fn)                  \
    virtual BOOL fn(HWND hWnd, UINT nHitTest,           \
        UINT message, BOOL& bHandled);
#define PROCESS_SETCURSOR(fn)                           \
    else if (WM_SETCURSOR == uMsg) {                    \
        ret = fn((HWND)wParam,                          \
            LOWORD(lParam),                             \
            HIWORD(lParam),                             \
            bHandled);                                  \
    }
#define DEFAULT_SETCURSOR_HANDLER(hwnd, hittest, m)     \
    DoDefault(WM_SETCURSOR, (WPARAM)hwnd,               \
        MAKELPARAM(hittest, m))

// WM_SIZE

#define DECLARE_SIZE_HANDLER(fn)                \
    void fn(UINT nType, int cx, int cy,         \
        BOOL& bHandled);
#define DECLAREV_SIZE_HANDLER(fn)               \
    virtual void fn(UINT nType, int cx, int cy, \
        BOOL& bHandled);
#define PROCESS_SIZE(fn)                        \
    else if (WM_SIZE == uMsg) {                 \
        fn((UINT)wParam,                        \
            LOWORD(lParam),                     \
            HIWORD(lParam),                     \
            bHandled);                          \
    }
#define DEFAULT_SIZE_HANDLER(n, cx, cy)         \
    DoDefault(WM_SIZE, n,                       \
        MAKELPARAM(cx, cy))

// WM_TIMER

#define DECLARE_TIMER_HANDLER(fn)       \
    void fn(UINT_PTR nIDEvent,          \
        BOOL& bHandled);
#define DECLAREV_TIMER_HANDLER(fn)      \
    virtual void fn(UINT_PTR nIDEvent,  \
        BOOL& bHandled);
#define PROCESS_TIMER(fn)               \
    else if (WM_TIMER == uMsg) {        \
        fn((UINT)wParam,  bHandled);    \
        ret = 0;                        \
    }
#define DEFAULT_TIMER_HANDLER(id)       \
    DoDefault(WM_TIMER, id, 0)
