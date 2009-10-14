/**
 * \file pdl_module.h
 * \brief PDL 应用程序模块类
 */

#pragma once

#include "pdl_base.h"

/**
 * \class LAppModule
 * \brief PDL 应用程序模块类
 */

class LStringA;
class LStringW;
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

    HRSRC FindResourceA(__in PCSTR lpName, __in PCSTR lpType);
    HRSRC FindResourceW(__in PCWSTR lpName, __in PCWSTR lpType);

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
     * @param [out] path 用于接收模块路径的 LStringA 对象指针。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI GetModulePath(__in HMODULE hMod, __out LStringA* path);

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

    HACCEL LoadAcceleratorsA(__in PCSTR lpTableName);
    HACCEL LoadAcceleratorsW(__in PCWSTR lpTableName);
    HBITMAP LoadBitmapA(__in PCSTR lpBitmapName);
    HBITMAP LoadBitmapW(__in PCWSTR lpBitmapName);
    HCURSOR LoadCursorA(__in PCSTR lpCursorName);
    HCURSOR LoadCursorW(__in PCWSTR lpCursorName);
    HICON LoadIconA(__in PCSTR lpIconName);
    HICON LoadIconW(__in PCWSTR lpIconName);
    HANDLE LoadImageA(__in PCSTR name, __in UINT type, __in int cx,
        __in int cy, __in UINT fuLoad);
    HANDLE LoadImageW(__in PCWSTR name, __in UINT type, __in int cx,
        __in int cy, __in UINT fuLoad);
    HMENU LoadMenuA(__in PCSTR lpMenuName);
    HMENU LoadMenuW(__in PCWSTR lpMenuName);
    HGLOBAL LoadResource(__in HRSRC hResInfo);
    int LoadStringA(__in UINT uID, __out PSTR lpBuffer,
        __in int nBufferMax);
    int LoadStringW(__in UINT uID, __out PWSTR lpBuffer,
        __in int nBufferMax);
    int LoadStringA(__in UINT id, __out LStringA* str);
    int LoadStringW(__in UINT id, __out LStringW* str);
    DWORD SizeofResource(__in HRSRC hResInfo);
protected:
    LAppModule(__in HINSTANCE hInstance);
    virtual ~LAppModule(void);
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
    PVOID* m_pvWndData;
    /**
     * 窗口数据个数
     */
    int m_cntWndData;
    /**
     * 窗口数据最大个数
     */
    int m_maxWndData;
};
