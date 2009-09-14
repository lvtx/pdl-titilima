/**
 * \file pdl_file.h
 * \brief PDL �ļ����������
 * \details ����ļ��ж����� PDL �����е��ļ������ࣺ
 *   \li \c LFile PDL �����ļ�������
 *   \li \c LTxtFile PDL �ı��ļ�������
*/

#pragma once

#include "pdl_base.h"

/**
 * \class LFile
 * \brief PDL �����ļ�������
 * \details LFile �� PDL ������ļ�������װ������װ�˴󲿷ֳ��õ��ļ����� API ������
 */

class LFile
{
public:
    LFile(void);
    ~LFile(void);
public:
    BOOL Close(void);
    BOOL Create(__in PCSTR lpFileName, __in DWORD dwDesiredAccess,
        __in DWORD dwShareMode, __in DWORD dwCreationDisposition,
        __in DWORD dwFlagsAndAttributes = FILE_ATTRIBUTE_NORMAL);
    BOOL Create(__in PCWSTR lpFileName, __in DWORD dwDesiredAccess,
        __in DWORD dwShareMode, __in DWORD dwCreationDisposition,
        __in DWORD dwFlagsAndAttributes = FILE_ATTRIBUTE_NORMAL);

    /**
     * �жϸ������ļ��Ƿ���ڡ�
     * @param [in] lpszFileName Ҫ�жϵ��ļ�����
     * @param [in] bIncludeDir �Ƿ����Ŀ¼��
     * @return ����ļ������򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL PDLAPI Exists(__in PCSTR lpszFileName,
        __in BOOL bIncludeDir = TRUE);

    /**
     * �жϸ������ļ��Ƿ���ڡ�
     * @param [in] lpszFileName Ҫ�жϵ��ļ�����
     * @param [in] bIncludeDir �Ƿ����Ŀ¼��
     * @return ����ļ������򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL PDLAPI Exists(__in PCWSTR lpszFileName,
        __in BOOL bIncludeDir = TRUE);

    BOOL Flush(void);

    /**
     * ��ȡ�ļ��ľ����
     * @return �ļ������Ӧ�ľ����
     */
    HANDLE GetHandle(void) const;

    DWORD GetPointer(void);
    DWORD GetSize(void);

    /**
     * �жϸ������ļ����Ƿ�ȫ·����
     * @param [in] lpszFileName Ҫ�жϵ��ļ�����
     * @return ����������ļ�����ȫ·���򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL PDLAPI IsFullPathName(__in PCSTR lpszFileName);

    /**
     * �жϸ������ļ����Ƿ�ȫ·����
     * @param [in] lpszFileName Ҫ�жϵ��ļ�����
     * @return ����������ļ�����ȫ·���򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL PDLAPI IsFullPathName(__in PCWSTR lpszFileName);

    /**
     * ƥ���ļ�����
     * @param [in] lpszFileName Ҫƥ����ļ�����
     * @param [in] lpszMatch Ҫƥ���ģʽ�ַ�����
     * @return ���ƥ��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL PDLAPI MatchName(__in PCSTR lpszFileName,
        __in PCSTR lpszMatch);

    /**
     * ƥ���ļ�����
     * @param [in] lpszFileName Ҫƥ����ļ�����
     * @param [in] lpszMatch Ҫƥ���ģʽ�ַ�����
     * @return ���ƥ��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    static BOOL PDLAPI MatchName(__in PCWSTR lpszFileName,
        __in PCWSTR lpszMatch);

    DWORD Read(__out PVOID lpBuffer, __in DWORD dwSize);
    DWORD SetPointer(__in LONG lPointer, __in DWORD dwMoveMethod);
    DWORD Write(__in LPCVOID lpBuffer, __in DWORD dwSize);
protected:
    /**
     * �ļ������Ӧ�ľ��
     */
    HANDLE m_hFile;
};

/**
 * \class LTxtFile
 * \brief PDL �ı��ļ�������
 * \details LTxtFile �� PDL ���ı��ļ������ķ�װ�ࡣ
 */

class LStringA;
class LStringW;
class LTxtFile : protected LFile
{
public:
    typedef enum {
        modeAppend,      // ׷��ģʽ
        modeReadWrite,   // ��дģʽ
        modeReset        // ���ģʽ
    } MODE;
    LTxtFile(void);
    ~LTxtFile(void);
public:

    BOOL Close(void);

    /**
     * �Ƿ��Ѵ��ļ�ĩβ��
     * @return �����ǰ�Ѿ������ļ�ĩβ�򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Eof(void);

    BOOL Flush(void);

    /**
     * ��ȡһ���ַ���
     * @return �����ȡ�ɹ��򷵻ض�ȡ�����ַ������򷵻� LEOF��
     */
    int GetChar(void);

    BOOL IsUnicode(void);

    /**
     * ��ָ����ģʽ��һ���ı��ļ���
     * @param [in] lpFileName Ҫ�򿪵��ļ�����
     * @param [in] mode ���ļ���ģʽ��
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     * \sa Close
     */
    BOOL Open(__in PCSTR lpFileName, __in MODE mode);

    /**
     * ��ָ����ģʽ��һ���ı��ļ���
     * @param [in] lpFileName Ҫ�򿪵��ļ�����
     * @param [in] mode ���ļ���ģʽ��
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     * \sa Close
     */
    BOOL Open(__in PCWSTR lpFileName, __in MODE mode);

    /**
     * д���ʽ�����ַ�����
     * @param [in] format �ַ����ĸ�ʽ��
     * @return ʵ��д����ַ�����
     */
    int PrintF(__in PCSTR format, ...);

    /**
     * д���ʽ�����ַ�����
     * @param [in] format �ַ����ĸ�ʽ��
     * @return ʵ��д����ַ�����
     */
    int PrintF(__in PCWSTR format, ...);

    /**
     * д��һ���ַ���
     * @param [in] ch Ҫд����ַ���
     */
    void PutChar(int ch);

    /**
     * ��ȡ�ı���
     * @param [out] str ��ȡ����������ַ�����
     * @param [in] Ҫ��ȡ���ַ�����
     * @return ʵ�ʶ������ַ�����
     */
    DWORD Read(__out LStringA* str, __in DWORD dwSize);

    /**
     * ��ȡ�ı���
     * @param [out] str ��ȡ����������ַ�����
     * @param [in] Ҫ��ȡ���ַ�����
     * @return ʵ�ʶ������ַ�����
     */
    DWORD Read(__out LStringW* str, __in DWORD dwSize);

    /**
     * ��ȡһ���ı���
     * @param [out] str ��ȡ����������ַ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL ReadLn(__out LStringA* str);

    /**
     * ��ȡһ���ı���
     * @param [out] str ��ȡ����������ַ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL ReadLn(__out LStringW* str);

    /**
     * ���ļ���д���ı���
     * @param [in] str Ҫд����ַ�����
     * @return ���д��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Write(__in PCSTR str);

    /**
     * ���ļ���д���ı���
     * @param [in] str Ҫд����ַ�����
     * @return ���д��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Write(__in PCWSTR str);

    /**
     * ���ļ���д��һ���ı���
     * @param [in] str Ҫд����ַ�����
     * @return ���д��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL WriteLn(__in PCSTR str);

    /**
     * ���ļ���д��һ���ı���
     * @param [in] str Ҫд����ַ�����
     * @return ���д��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL WriteLn(__in PCWSTR str);

protected:
    /**
     * ����ǰ�����е����ݸ��Ƶ�Ŀ�껺������
     */
    DWORD CopyToBuffer(__out PVOID buf, __in DWORD dwCnt);
    /**
     * ���һ��з���
     */
    int FindCRLF(void);
    /**
     * ��ȡ����顣
     */
    DWORD ReadBlock(void);
    /**
     * ��ʼ��������
     */
    void StartRead(void);
    /**
     * ��ʼд������
     */
    void StartWrite(void);
protected:
    /**
     * �ļ�������־
     */
    DWORD m_dwFlags;
    /**
     * �ַ���С
     */
    DWORD m_cbChar;
    /**
     * ���ݻ�����
     */
    PBYTE m_buf;
    /**
     * ��ǰ������ָ��
     */
    DWORD m_ptr;
    /**
     * R - �ɶ����ݵĴ�С��W - ��д�������λ��
     */
    DWORD m_rwptr;
};
