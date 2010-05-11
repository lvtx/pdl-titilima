/**
 * \file pdl_module.h
 * \brief PDL Ӧ�ó���ģ����
 */

#pragma once

#include "pdl_base.h"
#include "pdl_string.h"
#include "pdl_container.h"
#include <Shlwapi.h>

/**
 * \class LAppModule
 * \brief PDL Ӧ�ó���ģ����
 */

class LAppModule
{
public:

    /**
     * ��� LAppModule ����ָ�롣
     * @return ��ǰȫ�ֵ� LAppModule ����ָ�롣
     */
    static LAppModule* GetApp(void);

    /**
     * ���Ӧ�ó���ʵ�������
     * @return Ӧ�ó����ʵ�������Ҳ���� WinMain ����ڲ�����
     */
    HINSTANCE GetInstance(void) const;

    /**
     * ��ʼ�� LAppModule ȫ�ֶ���
     * @param [in] hInstance Ӧ�ó����ʵ�������Ҳ���� WinMain ����ڲ�����
     * @return ����ɹ��򷵻����ɵ� LAppModule ����ָ�룬���򷵻� NULL��
     * \note ���������Ҫ�� WinMain ��ڴ����á�
     */
    static LAppModule* Initialize(__in HINSTANCE hInstance);

    /**
     * ���� LAppModule ȫ�ֶ���
     * @return ������ٳɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL Destroy(void);

    /**
     * ��һ����������ָ������ݴ档
     * @param [in] lpWndData һ���������ݵ�ָ�롣
     */
    void AddWndData(__in PVOID lpWndData);

    /**
     * ����һ�� thunk ʹ�õ��ڴ档
     * @param [in] cntBytes Ҫ����Ĵ�С�����ֽڼơ�
     * @return �������ɹ��򷵻�һ����Ч���ڴ��ַ�����򷵻� NULL��
     */
    PVOID AllocThunkMemory(__in DWORD cntBytes);

    /**
     * ��ȡ��һ���ݴ�Ĵ�������ָ�롣
     * @return ��һ���ݴ�Ĵ�������ָ�롣
     */
    PVOID ExtractWndData(void);

public:

    /**
     * ���������־��
     * @param [in] lpszFormat Ҫ�������־�ַ�����ʽ��
     * \note ���Ӧ�ó���Ŀ¼�´��� debuglog.yes �ļ�������־������ļ��У������Ե�����Ϣ�����
     */
    static void DebugPrint(__in PCSTR lpszFormat, ...);

    /**
     * ���������־��
     * @param [in] lpszFormat Ҫ�������־�ַ�����ʽ��
     * \note ���Ӧ�ó���Ŀ¼�´��� debuglog.yes �ļ�������־������ļ��У������Ե�����Ϣ�����
     */
    static void DebugPrint(__in PCWSTR lpszFormat, ...);

    HRSRC FindResource(__in PCTSTR lpName, __in PCTSTR lpType);
#ifdef UNICODE
    HRSRC FindResourceA(__in PCSTR lpName, __in PCSTR lpType);
#else
    HRSRC FindResourceW(__in PCWSTR lpName, __in PCWSTR lpType);
#endif // UNICODE

    /**
     * ��ȡģ����ļ�����
     * @param [in] hMod һ����Ч��ģ������
     * @param [out] path ���ڽ���ģ���ļ����� LStringA ����ָ�롣
     * @param [in] bFullPath �Ƿ��ȡȫ·�����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL PDLAPI GetModuleName(__in HMODULE hMod, __out LStringA* name,
        __in BOOL bFullPath);

    /**
     * ��ȡģ����ļ�����
     * @param [in] hMod һ����Ч��ģ������
     * @param [out] path ���ڽ���ģ���ļ����� LStringW ����ָ�롣
     * @param [in] bFullPath �Ƿ��ȡȫ·�����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL PDLAPI GetModuleName(__in HMODULE hMod, __out LStringW* path,
        __in BOOL bFullPath);

    /**
     * ��ȡģ������·����
     * @param [in] hMod һ����Ч��ģ������
     * @param [out] path ���ڽ���ģ��·���� LStringA ����ָ�롣
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL PDLAPI GetModulePath(__in HMODULE hMod, __out LStringA* path);

    /**
     * ��ȡָ��ģ��İ汾��Ϣ��
     * @param [in] hMod һ����Ч��ģ������
     * @param [out] path ���ڽ��հ汾��Ϣ�Ľṹ��ָ�롣
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL PDLAPI GetModuleVersion(__in HMODULE hMod,
        __out DLLVERSIONINFO* dvi);

    /**
     * ��ȡ LAppModule ������ļ�����
     * @param [out] path ���ڽ��� LAppModule �����ļ����� LStringA ����ָ�롣
     * @param [in] bFullPath �Ƿ��ȡȫ·�����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetName(__out LStringA* name, __in BOOL bFullPath);

    /**
     * ��ȡ LAppModule ������ļ�����
     * @param [out] path ���ڽ��� LAppModule �����ļ����� LStringW ����ָ�롣
     * @param [in] bFullPath �Ƿ��ȡȫ·�����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetName(__out LStringW* name, __in BOOL bFullPath);

    /**
     * ��ȡ LAppModule �����ļ�����·����
     * @param [out] path ���ڽ��� LAppModule �����ļ�·���� LStringA ����ָ�롣
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetPath(__out LStringA* path);

    /**
     * ��ȡ LAppModule �����ļ�����·����
     * @param [out] path ���ڽ��� LAppModule �����ļ�·���� LStringW ����ָ�롣
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetPath(__out LStringW* path);

    /**
     * ��ȡģ������·����
     * @param [out] path ���ڽ���ģ��·���� LStringW ����ָ�롣
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
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
     * ���� thunk ҳ��
     */
    static void DestroyPage(PVOID ptr);
protected:
    /**
     * ȫ�ֵ� LAppModule ����ָ��
     */
    static LAppModule *m_pApp;
    /**
     * Ӧ�ó���ʵ�����
     */
    HINSTANCE m_hInstance;
    /**
     * �ݴ洰������
     */
    LPtrList m_WndData;
    /**
     * ����������
     */
    ILock* m_wdLock;
    /**
     * thunk ҳ��
     */
    LPtrList m_tPages;
    /**
     * thunk ��
     */
    ILock* m_tLock;
    /**
     * ��ǰ��ҳ��ʹ��
     */
    DWORD m_dwPageUsage;
};
