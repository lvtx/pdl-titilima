/**
 * \file pdl_base.h
 * \brief PDL 基础头文件
 * \details 这是 PDL 的基础头文件，每个基于 PDL 的源代码文件都应该在其头部包含这个文件。
 */

#pragma once

#if _MSC_VER <= 1200
#define _WIN32_WINNT 0x0501
#endif // _MSC_VER <= 1200

#ifndef STRICT
#define STRICT
#endif // STRICT

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif // WIN32_LEAN_AND_MEAN

#ifdef _CRTIMP
#undef _CRTIMP
#endif // _CRTIMP
#define _CRTIMP

#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif // _CRT_SECURE_NO_WARNINGS

#include <Windows.h>
#include <tchar.h>
#include <CommCtrl.h>

#ifndef __in
#define __in
#endif // __in

#ifndef __in_opt
#define __in_opt
#endif // __in_opt

#ifndef __out_opt
#define __out_opt
#endif // __out_opt

/**
 * \def PDLASSERT
 * 对表达式进行断言。
 */

/**
 * \def PDLTRACE
 * 输出调试信息。
 */

/**
 * \def PDLLOG
 * 输出日志信息。
 */
#define _PDL_STR(s)     #s
#define PDL_STR(s)      _PDL_STR(s)
#define _PDL_WSTR(s)    L ## s
#define PDL_WSTR(s)     _PDL_WSTR(s)
#ifdef UNICODE
#define PDL_TSTR(s)     _PDL_WSTR(s)
#else
#define PDL_TSTR(s)     _PDL_STR(s)
#endif // UNICODE

#ifdef _DEBUG

#define PDLASSERT(expr) (void)((!!(expr)) || (LAssertBox(L###expr, PDL_WSTR(__FILE__), __LINE__)))
#define PDLVERIFY       PDLASSERT
#define PDLTRACE        LTrace
#define PDLLOG          LTrace

#else

#define PDLASSERT(expr) ((void)0)
#define PDLVERIFY(expr) ((void)(expr))
#define PDLTRACE        (void)
#define PDLLOG          LAppModule::DebugPrint

#endif // _DEBUG

/**
 * \def PDLAPI
 * PDL 函数调用约定。
 */
#ifndef PDLAPI
#define PDLAPI  __stdcall
#endif

/**
 * \def PDLINLINE
 * PDL 内联标志。
 */
#ifndef PDLINLINE
#define PDLINLINE   __inline
#endif // PDLINLINE

/**
 * \def GET_X_LPARAM
 * 从消息的 lParam 参数中获取 x 坐标。
 */
#define GET_X_LPARAM(lp)                        ((int)(short)LOWORD(lp))
/**
 * \def GET_Y_LPARAM
 * 从消息的 lParam 参数中获取 y 坐标。
 */
#define GET_Y_LPARAM(lp)                        ((int)(short)HIWORD(lp))
/**
 * \def PDL_ARGUMENT_PRESENT
 * 判断指定的参数是否可用。
 */
#ifndef PDL_ARGUMENT_PRESENT
#define PDL_ARGUMENT_PRESENT(ArgumentPointer)       \
    ((CHAR *)(ArgumentPointer) != (CHAR *)(NULL))
#endif // PDL_ARGUMENT_PRESENT

/**
 * \def PDL_NO_VTABLE
 * 指定一个类为无虚表。
 */
#ifndef PDL_NO_VTABLE
#define PDL_NO_VTABLE __declspec(novtable)
#endif // PDL_NO_VTABLE

/**
 * \def PDL_SYSBRUSH
 * 获取一个系统画刷。
 */
#ifndef PDL_SYSBRUSH
#define PDL_SYSBRUSH(br)    ((HBRUSH)(br + 1))
#endif

/**
 * \def LEOF
 * 文件结束标志
 */
#ifndef LEOF
#define LEOF    (-1)
#endif

/**
 * \def PDL_REGMSG
 * 注册一个窗口消息。
 */
#ifndef PDL_REGMSG
#define PDL_REGMSG(msg)     msg = ::RegisterWindowMessage(_T(#msg))
#endif // PDL_REGMSG

///////////////////////////////////////////////////////////////////////////////
// 全局变量声明

/**
 * \var WM_PDL_GETOBJECTA
 * \brief 获取窗口对应的 PDL 窗口类指针及类名字符串。
 * @param [in] wParam 用于接收类名的缓冲区大小，以字符计。
 * @param [out] lParam 用于接收类名的缓冲区地址。
 * @return 窗口所对应的 PDL 窗口类指针。
 */
extern UINT WM_PDL_GETOBJECTA;

/**
 * \var WM_PDL_GETOBJECTW
 * \brief 获取窗口对应的 PDL 窗口类指针及类名字符串。
 * @param [in] wParam 用于接收类名的缓冲区大小，以宽字符计。
 * @param [out] lParam 用于接收类名的缓冲区地址。
 * @return 窗口所对应的 PDL 窗口类指针。
 */
extern UINT WM_PDL_GETOBJECTW;

/**
 * \def WM_PDL_GETOBJECT
 * WM_PDL_GETOBJECTA 与 WM_PDL_GETOBJECTW 的 UNICODE 兼容宏。
 */
#ifdef UNICODE
#define WM_PDL_GETOBJECT    WM_PDL_GETOBJECTW
#else
#define WM_PDL_GETOBJECT    WM_PDL_GETOBJECTA
#endif // UNICODE

#define PDL_NOTIFY              0
#define PDL_NOTIFY_DRAWITEM     1
#define PDL_NOTIFY_CUSTOMDRAW   2

/**
 * \var WM_PDL_GETNOTIFY
 * \brief 获取窗口对应的通知对象指针。
 * @param [in] wParam 用于指定通知对象的类型。
 * @return 窗口对应的通知对象指针。
 */
extern UINT WM_PDL_GETNOTIFY;

/**
 * 指针类型转换，可以突破类成员函数的限制。
 */
template <typename R, typename T>
PDLINLINE R ptr_cast(T t)
{
    PVOID r = *reinterpret_cast<PVOID*>(&t);
    return static_cast<R>(r);
}

/**
 * 偏移量类型转换
 */
template <typename T>
PDLINLINE T offset_cast(PVOID base, int offset)
{
    PBYTE pb = reinterpret_cast<PBYTE>(base);
    return reinterpret_cast<T>(pb + offset);
}

/**
 * 弹出断言对话框。
 * @param [in] expr 断言表达式。
 * @param [in] srcfile 产生断言的源文件。
 * @param [in] nLine 源文件的行数。
 */
int PDLAPI LAssertBox(__in PCWSTR expr, __in PCWSTR srcfile, __in int nLine);

/**
 * 输出调试信息。
 */
void PDLAPI LTrace(__in PCSTR lpszFormat, ...);
/**
 * 输出调试信息。
 */
void PDLAPI LTrace(__in PCWSTR lpszFormat, ...);

/**
 * Thunk 指针定义
 */
typedef struct _tagThisThunk *PTHISTHUNK;

/**
 * 创建一个 This Thunk。
 * @param [in] This 要绑定的类对象指针。
 * @param [in] pfn 要绑定的类成员函数指针。
 * @return 如果成功则返回 Thunk 的指针，否则返回 NULL。
 * \sa LDestroyThisThunk
 */
PTHISTHUNK PDLAPI LCreateThisThunk(PVOID This, PVOID pfn);

/**
 * 销毁一个 This Thunk。
 * @param [in] thunk 一个有效的 Thunk 指针。
 * \sa LCreateThisThunk
 */
void PDLAPI LDestroyThisThunk(PTHISTHUNK thunk);

/**
 * \class LThisThunk
 * \brief This Thunk 辅助类。
 */

template <typename T>
class LThisThunk
{
    /**
     * Thunk 指针
     */
    PTHISTHUNK thunk;
public:
    /**
     * 构造函数
     */
    LThisThunk(void)
    {
        thunk = NULL;
    }
    /**
     * 析构函数
     */
    ~LThisThunk(void)
    {
        DestroyThunk();
    }
    /**
     * 类型转换操作符
     */
    operator T (void)
    {
        return (T)thunk;
    }
    /**
     * 创建一个 Thunk。
     */
    template<typename F>
    BOOL CreateThunk(PVOID This, F fn)
    {
        if (NULL != thunk)
            return FALSE;

        thunk = LCreateThisThunk(This, ptr_cast<PVOID>(fn));
        return NULL != thunk;
    }
    /**
     * 销毁 Thunk。
     */
    void DestroyThunk(void)
    {
        if (NULL != thunk)
            LDestroyThisThunk(thunk);
        thunk = NULL;
    }
};

/**
 * \class ILock
 * \brief PDL 操作锁
 */
class ILock
{
public:
    /**
     * 创建一个操作锁接口。
     * @return 操作锁接口。
     */
    static ILock* Create(void);
    /**
     * 创建一个全局操作锁接口。
     * @param [in] lpName 操作锁名称。
     * @return 操作锁接口。
     */
    static ILock* Create(__in PCSTR lpName);
    /**
     * 创建一个全局操作锁接口。
     * @param [in] lpName 操作锁名称。
     * @return 操作锁接口。
     */
    static ILock* Create(__in PCWSTR lpName);
    /**
     * 销毁操作锁。
     */
    virtual void Destroy(void) = 0;
    /**
     * 锁定操作锁。
     */
    virtual void Lock(void) = 0;
    /**
     * 解锁。
     */
    virtual void Unlock(void) = 0;
};
