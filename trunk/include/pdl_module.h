/**
 * \file pdl_module.h
 * \brief PDL 应用程序模块类
 */

#pragma once

#include "pdl_base.h"
#include "pdl_string.h"
#include "pdl_container.h"
#include <Shlwapi.h>

/**
 * \class LAppModule
 * \brief PDL 应用程序模块类
 */

class LAppModule
{
public:

    /**
     * 获得 LAppModule 对象指针。
     * @return 当前全局的 LAppModule 对象指针。
     */
    static LAppModule* GetApp(void);

    /**
     * 获得应用程序实例句柄。
     * @return 应用程序的实例句柄，也就是 WinMain 的入口参数。
     */
    HINSTANCE GetInstance(void) const;

    /**
     * 初始化 LAppModule 全局对象。
     * @param [in] hInstance 应用程序的实例句柄，也就是 WinMain 的入口参数。
     * @return 如果成功则返回生成的 LAppModule 对象指针，否则返回 NULL。
     * \note 这个函数需要在 WinMain 入口处调用。
     */
    static LAppModule* Initialize(__in HINSTANCE hInstance);

    /**
     * 销毁 LAppModule 全局对象。
     * @return 如果销毁成功则返回 TRUE，否则返回 FALSE。
     */
    static BOOL Destroy(void);

    /**
     * 将一个窗口数据指针放入暂存。
     * @param [in] lpWndData 一个窗口数据的指针。
     */
    void AddWndData(__in PVOID lpWndData);

    /**
     * 分配一段 thunk 使用的内存。
     * @param [in] cntBytes 要分配的大小，以字节计。
     * @return 如果分配成功则返回一段有效的内存地址，否则返回 NULL。
     */
    PVOID AllocThunkMemory(__in DWORD cntBytes);

    /**
     * 提取上一次暂存的窗口数据指针。
     * @return 上一次暂存的窗口数据指针。
     */
    PVOID ExtractWndData(void);

public:

    /**
     * 输出程序日志。
     * @param [in] lpszFormat 要输出的日志字符串格式。
     * \note 如果应用程序目录下存在 debuglog.yes 文件，则将日志输出到文件中；否则以调试信息输出。
     */
    static void DebugPrint(__in PCSTR lpszFormat, ...);

    /**
     * 输出程序日志。
     * @param [in] lpszFormat 要输出的日志字符串格式。
     * \note 如果应用程序目录下存在 debuglog.yes 文件，则将日志输出到文件中；否则以调试信息输出。
     */
    static void DebugPrint(__in PCWSTR lpszFormat, ...);

    HRSRC FindResource(__in PCTSTR lpName, __in PCTSTR lpType);
#ifdef UNICODE
    HRSRC FindResourceA(__in PCSTR lpName, __in PCSTR lpType);
#else
    HRSRC FindResourceW(__in PCWSTR lpName, __in PCWSTR lpType);
#endif // UNICODE

    /**
     * 获取模块的文件名。
     * @param [in] hMod 一个有效的模块句柄。
     * @param [out] path 用于接收模块文件名的 LStringA 对象指针。
     * @param [in] bFullPath 是否获取全路径的文件名。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI GetModuleName(__in HMODULE hMod, __out LStringA* name,
        __in BOOL bFullPath);

    /**
     * 获取模块的文件名。
     * @param [in] hMod 一个有效的模块句柄。
     * @param [out] path 用于接收模块文件名的 LStringW 对象指针。
     * @param [in] bFullPath 是否获取全路径的文件名。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI GetModuleName(__in HMODULE hMod, __out LStringW* path,
        __in BOOL bFullPath);

    /**
     * 获取模块所在路径。
     * @param [in] hMod 一个有效的模块句柄。
     * @param [out] path 用于接收模块路径的 LStringA 对象指针。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI GetModulePath(__in HMODULE hMod, __out LStringA* path);

    /**
     * 获取指定模块的版本信息。
     * @param [in] hMod 一个有效的模块句柄。
     * @param [out] path 用于接收版本信息的结构体指针。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI GetModuleVersion(__in HMODULE hMod,
        __out DLLVERSIONINFO* dvi);

    /**
     * 获取 LAppModule 对象的文件名。
     * @param [out] path 用于接收 LAppModule 对象文件名的 LStringA 对象指针。
     * @param [in] bFullPath 是否获取全路径的文件名。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetName(__out LStringA* name, __in BOOL bFullPath);

    /**
     * 获取 LAppModule 对象的文件名。
     * @param [out] path 用于接收 LAppModule 对象文件名的 LStringW 对象指针。
     * @param [in] bFullPath 是否获取全路径的文件名。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetName(__out LStringW* name, __in BOOL bFullPath);

    /**
     * 获取 LAppModule 对象文件所在路径。
     * @param [out] path 用于接收 LAppModule 对象文件路径的 LStringA 对象指针。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetPath(__out LStringA* path);

    /**
     * 获取 LAppModule 对象文件所在路径。
     * @param [out] path 用于接收 LAppModule 对象文件路径的 LStringW 对象指针。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetPath(__out LStringW* path);

    /**
     * 获取模块所在路径。
     * @param [out] path 用于接收模块路径的 LStringW 对象指针。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI GetModulePath(__in HMODULE hMod, __out LStringW* path);

    HACCEL LoadAccelerators(__in PCTSTR lpTableName);
    HBITMAP LoadBitmap(__in PCTSTR lpBitmapName);
    HCURSOR LoadCursor(__in PCTSTR lpCursorName);
    HICON LoadIcon(__in PCTSTR lpIconName);
    HANDLE LoadImage(__in PCTSTR name, __in UINT type, __in int cx,
        __in int cy, __in UINT fuLoad);
    HMENU LoadMenu(__in PCTSTR lpMenuName);
#ifdef UNICODE
    HACCEL LoadAcceleratorsA(__in PCSTR lpTableName);
    HBITMAP LoadBitmapA(__in PCSTR lpBitmapName);
    HCURSOR LoadCursorA(__in PCSTR lpCursorName);
    HICON LoadIconA(__in PCSTR lpIconName);
    HANDLE LoadImageA(__in PCSTR name, __in UINT type, __in int cx,
        __in int cy, __in UINT fuLoad);
    HMENU LoadMenuA(__in PCSTR lpMenuName);
#else
    HACCEL LoadAcceleratorsW(__in PCWSTR lpTableName);
    HBITMAP LoadBitmapW(__in PCWSTR lpBitmapName);
    HCURSOR LoadCursorW(__in PCWSTR lpCursorName);
    HICON LoadIconW(__in PCWSTR lpIconName);
    HANDLE LoadImageW(__in PCWSTR name, __in UINT type, __in int cx,
        __in int cy, __in UINT fuLoad);
    HMENU LoadMenuW(__in PCWSTR lpMenuName);
#endif // UNICODE
    HGLOBAL LoadResource(__in HRSRC hResInfo);
    int LoadString(__in UINT uID, __out PTSTR lpBuffer,
        __in int nBufferMax);
    int LoadString(__in UINT id, __out LString* str);
#ifdef UNICODE
    int LoadStringA(__in UINT uID, __out PSTR lpBuffer,
        __in int nBufferMax);
    int LoadStringA(__in UINT id, __out LStringA* str);
#else
    int LoadStringW(__in UINT uID, __out PWSTR lpBuffer,
        __in int nBufferMax);
    int LoadStringW(__in UINT id, __out LStringW* str);
#endif // UNICODE
    DWORD SizeofResource(__in HRSRC hResInfo);
protected:
    LAppModule(__in HINSTANCE hInstance);
    virtual ~LAppModule(void);
private:
    /**
     * 销毁 thunk 页面
     */
    static void DestroyPage(PVOID ptr);
protected:
    /**
     * 全局的 LAppModule 对象指针
     */
    static LAppModule *m_pApp;
    /**
     * 应用程序实例句柄
     */
    HINSTANCE m_hInstance;
    /**
     * 暂存窗口数据
     */
    LPtrList m_WndData;
    /**
     * 窗口数据锁
     */
    ILock* m_wdLock;
    /**
     * thunk 页面
     */
    LPtrList m_tPages;
    /**
     * thunk 锁
     */
    ILock* m_tLock;
    /**
     * 当前的页面使用
     */
    DWORD m_dwPageUsage;
};
