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
        WPARAM wParam, LPARAM lParam,   \
        BOOL& bHandled);
#define PROCESS_MESSAGE(msg, fn)        \
    else if (msg == uMsg) {             \
        ret = fn(uMsg, wParam, lParam,  \
            bHandled);                  \
    }

// WM_CLOSE

#define DECLARE_CLOSE_HANDLER(fn)       \
    void fn(BOOL& bHandled);
#define PROCESS_CLOSE(fn)               \
    else if (WM_CLOSE == uMsg) {        \
        fn(bHandled);                   \
    }

// WM_COMMAND

#define DECLARE_COMMAND_HANDLER(fn)     \
    void fn(WORD wNotifyCode, WORD wID, \
        HWND hWndCtrl, BOOL& bHandled);
#define PROCESS_COMMAND(fn)             \
    else if (WM_COMMAND == uMsg) {      \
        fn(HIWORD(wParam),              \
            LOWORD(wParam),             \
            (HWND)lParam,               \
            bHandled);                  \
    }

// WM_CONTEXTMENU

#define DECLARE_CONTEXTMENU_HANDLER(fn) \
    void fn(HWND hWnd, int x, int y,    \
        BOOL& bHandled);
#define PROCESS_CONTEXTMENU(fn)         \
    else if (WM_CONTEXTMENU == uMsg) {  \
        fn((HWND)wParam,                \
            LOWORD(lParam),             \
            HIWORD(lParam),             \
            bHandled);                  \
    }

// WM_CREATE

#define DECLARE_CREATE_HANDLER(fn)      \
    int fn(LPCREATESTRUCT lpCs,         \
        BOOL& bHandled);
#define PROCESS_CREATE(fn)              \
    else if (WM_CREATE == uMsg) {       \
        ret = (LRESULT)fn(              \
            (LPCREATESTRUCT)lParam,     \
            bHandled);                  \
    }

// WM_DESTROY

#define DECLARE_DESTROY_HANDLER(fn)     \
    void fn(BOOL& bHandled);
#define PROCESS_DESTROY(fn)             \
    else if (WM_DESTROY == uMsg) {      \
        fn(bHandled);                   \
    }

// WM_ERASEBKGND

#define DECLARE_ERASEBKGND_HANDLER(fn)  \
    BOOL fn(HDC hdc, BOOL& bHandled);
#define PROCESS_ERASEBKGND(fn)          \
    else if (WM_ERASEBKGND == uMsg) {   \
        ret = (LRESULT)fn((HDC)wParam,  \
            bHandled);                  \
    }

// WM_INITDIALOG

#define DECLARE_INITDIALOG_HANDLER(fn)  \
    BOOL fn(HWND hCtrlFocus,            \
        LPARAM lParam, BOOL& bHandled);
#define PROCESS_INITDIALOG(fn)          \
    else if (WM_INITDIALOG == uMsg) {   \
        ret = (LRESULT)fn((HWND)wParam, \
            lParam, bHandled);          \
    }

// WM_KEYDOWN

#define DECLARE_KEYDOWN_HANDLER(fn)     \
    void fn(UINT nChar,                 \
        UINT nRepCnt, UINT nFlags,      \
        BOOL& bHandled);
#define PROCESS_KEYDOWN(fn)             \
    else if (WM_KEYDOWN == uMsg) {      \
        fn((UINT)wParam,                \
            LOWORD(lParam),             \
            HIWORD(lParam),             \
            bHandled);                  \
    }

// WM_KEYUP

#define DECLARE_KEYUP_HANDLER(fn)       \
    void fn(UINT nChar,                 \
        UINT nRepCnt, UINT nFlags,      \
        BOOL& bHandled);
#define PROCESS_KEYUP(fn)               \
    else if (WM_KEYUP == uMsg) {        \
        fn((UINT)wParam,                \
            LOWORD(lParam),             \
            HIWORD(lParam),             \
            bHandled);                  \
    }

// WM_LBUTTONDBLCLK

#define DECLARE_LBUTTONDBLCLK_HANDLER(fn)   \
    void fn(UINT uFlags, int x, int y,      \
        BOOL& bHandled);
#define PROCESS_LBUTTONDBLCLK(fn)           \
    else if (WM_LBUTTONDBLCLK == uMsg) {    \
        fn((UINT)wParam,                    \
            GET_X_LPARAM(lParam),           \
            GET_Y_LPARAM(lParam),           \
            bHandled);                      \
    }

// WM_LBUTTONDOWN

#define DECLARE_LBUTTONDOWN_HANDLER(fn) \
    void fn(UINT uFlags, int x, int y,  \
        BOOL& bHandled);
#define PROCESS_LBUTTONDOWN(fn)         \
    else if (WM_LBUTTONDOWN == uMsg) {  \
        fn((UINT)wParam,                \
            GET_X_LPARAM(lParam),       \
            GET_Y_LPARAM(lParam),       \
            bHandled);                  \
    }

// WM_LBUTTONUP

#define DECLARE_LBUTTONUP_HANDLER(fn)   \
    void fn(UINT uFlags, int x, int y,  \
        BOOL& bHandled);
#define PROCESS_LBUTTONUP(fn)           \
    else if (WM_LBUTTONUP == uMsg) {    \
        fn((UINT)wParam,                \
            GET_X_LPARAM(lParam),       \
            GET_Y_LPARAM(lParam),       \
            bHandled);                  \
    }

// WM_MOUSELEAVE

#define DECLARE_MOUSELEAVE_HANDLER(fn)  \
    void fn(BOOL& bHandled);
#define PROCESS_MOUSELEAVE(fn)          \
    else if (WM_MOUSELEAVE == uMsg) {   \
        fn(bHandled);                   \
    }

// WM_MOUSEMOVE

#define DECLARE_MOUSEMOVE_HANDLER(fn)   \
    void fn(UINT uFlags, int x, int y,  \
        BOOL& bHandled);
#define PROCESS_MOUSEMOVE(fn)           \
    else if (WM_MOUSEMOVE == uMsg) {    \
        fn((UINT)wParam,                \
            GET_X_LPARAM(lParam),       \
            GET_Y_LPARAM(lParam),       \
            bHandled);                  \
    }

// WM_NOTIFY

#define DECLARE_NOTIFY_HANDLER(fn)      \
    LRESULT fn(int idCtrl,              \
        LPNMHDR pnmh, BOOL& bHandled);
#define PROCESS_NOTIFY(fn)              \
    else if (WM_NOTIFY == uMsg) {       \
        ret = fn(wParam,                \
            (LPNMHDR)lParam, bHandled); \
    }

// WM_PAINT

#define DECLARE_PAINT_HANDLER(fn)       \
    void fn(BOOL& bHandled);
#define PROCESS_PAINT(fn)               \
    else if (WM_PAINT == uMsg) {        \
        fn(bHandled);                   \
    }

// WM_RBUTTONDOWN

#define DECLARE_RBUTTONDOWN_HANDLER(fn) \
    void fn(UINT uFlags, int x, int y,  \
        BOOL& bHandled);
#define PROCESS_RBUTTONDOWN(fn)         \
    else if (WM_RBUTTONDOWN == uMsg) {  \
        fn((UINT)wParam,                \
            GET_X_LPARAM(lParam),       \
            GET_Y_LPARAM(lParam),       \
            bHandled);                  \
    }

// WM_SETCURSOR

#define DECLARE_SETCURSOR_HANDLER(fn)   \
    BOOL fn(HWND hWnd, UINT nHitTest,   \
        UINT message, BOOL& bHandled);
#define PROCESS_SETCURSOR(fn)           \
    else if (WM_SETCURSOR == uMsg) {    \
        ret = fn((HWND)wParam,          \
            LOWORD(lParam),             \
            HIWORD(lParam),             \
            bHandled);                  \
    }

// WM_SIZE

#define DECLARE_SIZE_HANDLER(fn)        \
    void fn(UINT nType, int cx, int cy, \
        BOOL& bHandled);
#define PROCESS_SIZE(fn)                \
    else if (WM_SIZE == uMsg) {         \
        fn((UINT)wParam,                \
            LOWORD(lParam),             \
            HIWORD(lParam),             \
            bHandled);                  \
    }
