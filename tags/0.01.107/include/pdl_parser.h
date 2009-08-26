/**
 * \file pdl_parser.h
 * \brief PDL ��ʽ��������
 * \details ����ļ��а����� PDL �г��õĸ�ʽ���������༰������
 *   \li \c LParseColorString ��ɫ�ַ�����������
 *   \li \c LIniParser ini �ļ�������
 */

#pragma once

#include <pdl_base.h>
#include <pdl_string.h>
#include <pdl_container.h>

/**
 * \fn LParseColorString
 * ������ɫ�ַ�����
 * @param [in] lpszString Ҫ��������ɫ�ַ�������ʽΪ "#%R%G%B" �� "%R,%G,%B"��
 * @param [in] clrDefault �ڽ���ʧ�ܵ�����·��ص�Ĭ����ɫֵ��
 * @return �����ɹ�����ɫֵ�� clrDefault Ĭ��ָ������ɫֵ��
 */

COLORREF PDLAPI LParseColorString(
    __in PCSTR lpszString,
    __in COLORREF clrDefault
);

/**
 * \fn LParseColorString
 * ������ɫ�ַ�����
 * @param [in] lpszString Ҫ��������ɫ�ַ�������ʽΪ "#%R%G%B" �� "%R,%G,%B"��
 * @param [in] clrDefault �ڽ���ʧ�ܵ�����·��ص�Ĭ����ɫֵ��
 * @return �����ɹ�����ɫֵ�� clrDefault Ĭ��ָ������ɫֵ��
 */

COLORREF PDLAPI LParseColorString(
    __in PCWSTR lpszString,
    __in COLORREF clrDefault
);

/**
 * \class LIniParser
 * \brief ini �ļ�������
 */

class LTxtFile;
class LIniParser : protected LStrListA
{
public:

    /**
     * ���캯����
     * @param [in] lock ��������
     */
    LIniParser(__in ILock* lock = NULL);

    ~LIniParser(void);
public:

    /**
     * �ر�һ�� ini �ļ������豣�档
     */
    void Close(void);

    /**
     * ��ָ���� Section �� Key ����ȡһ������ֵ��
     * @param [in] lpszSection Ҫ��ȡ�� Section ���ơ�
     * @param [in] lpszKey Ҫ��ȡ�� Key ���ơ�
     * @param [in] nDefault ����ȡʧ��ʱ�����ص�Ĭ��ֵ��
     * @return ����ɹ��򷵻� ini �б������ֵ�����򷵻��� nDefault ָ������ֵ��
     */
    int GetInt(__in PCSTR lpszSection, __in PCSTR lpszKey, __in int nDefault);

    /**
     * ��ָ���� Section �� Key ����ȡһ���ַ�����
     * @param [in] lpszSection Ҫ��ȡ�� Section ���ơ�
     * @param [in] lpszKey Ҫ��ȡ�� Key ���ơ�
     * @param [in] nDefault ����ȡʧ��ʱ�����ص�Ĭ��ֵ��
     * @param [out] strResult ��ȡ���ַ�����
     * @return ��ȡ���ַ������ȡ�
     */
    DWORD GetString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCSTR lpszDefault, __out LStringA* strResult);

    /**
     * ��ָ���� Section �� Key ����ȡһ���ַ�����
     * @param [in] lpszSection Ҫ��ȡ�� Section ���ơ�
     * @param [in] lpszKey Ҫ��ȡ�� Key ���ơ�
     * @param [in] nDefault ����ȡʧ��ʱ�����ص�Ĭ��ֵ��
     * @param [out] strResult ��ȡ���ַ�����
     * @return ��ȡ���ַ������ȡ�
     */
    DWORD GetString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCWSTR lpszDefault, __out LStringW* strResult);

    /**
     * ��ָ���� Section �� Key ����ȡһ���ַ�����
     * @param [in] lpszSection Ҫ��ȡ�� Section ���ơ�
     * @param [in] lpszKey Ҫ��ȡ�� Key ���ơ�
     * @param [in] nDefault ����ȡʧ��ʱ�����ص�Ĭ��ֵ��
     * @param [out] lpszBuffer ���ڽ��ս���Ļ�������
     * @param [in] nSize lpszBuffer �������Ĵ�С�����ַ��ơ�
     * @return ��ȡ���ַ������ȡ�
     */
    DWORD GetString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCSTR lpszDefault, __out PSTR lpszBuffer, __in DWORD nSize);

    /**
     * ��ָ���� Section �� Key ����ȡһ���ַ�����
     * @param [in] lpszSection Ҫ��ȡ�� Section ���ơ�
     * @param [in] lpszKey Ҫ��ȡ�� Key ���ơ�
     * @param [in] nDefault ����ȡʧ��ʱ�����ص�Ĭ��ֵ��
     * @param [out] lpszBuffer ���ڽ��ս���Ļ�������
     * @param [in] nSize lpszBuffer �������Ĵ�С�����ַ��ơ�
     * @return ��ȡ���ַ������ȡ�
     */
    DWORD GetString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCWSTR lpszDefault, __out PWSTR lpszBuffer, __in DWORD nSize);

    /**
     * ��ȡ���� Section ��������
     * @return ���� Section ��������
     */
    DWORD GetSectionCount(void);

    /**
     * ��һ�� ini �ļ���
     * @param lpszFileName: ini �ļ����ļ��������Ϊ NULL�������Ӧ�ó���ͬ���� ini �ļ���
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Open(__in_opt PCSTR lpszFileName);

    /**
     * ��һ�� ini �ļ���
     * @param lpszFileName: ini �ļ����ļ��������Ϊ NULL�������Ӧ�ó���ͬ���� ini �ļ���
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Open(__in_opt PCWSTR lpszFileName);

    /**
     * �Ƴ�һ�� Section��
     * @param [in] Ҫ�Ƴ��� Section ���ơ�
     */
    void RemoveSection(__in PCSTR lpszSection);

    /**
     * �����ݱ���Ϊһ�� ini �ļ���
     * @param [in] lpszFileName ini �ļ����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Save(__in_opt PCSTR lpszFileName);

    /**
     * �����ݱ���Ϊһ�� ini �ļ���
     * @param [in] lpszFileName ini �ļ����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Save(__in_opt PCWSTR lpszFileName);

    /**
     * ��ָ���� Section �� Key ������Ϊָ������ֵ��
     * @param [in] lpszSection Ҫд��� Section ���ơ�
     * @param [in] lpszKey Ҫд��� Key ���ơ�
     * @param [in] nValue Ҫд�����ֵ��
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL WriteInt(__in PCSTR lpszSection, __in PCSTR lpszKey, __in int nValue);

    /**
     * ��ָ���� Section �� Key ������Ϊָ�����ַ�����
     * @param [in] lpszSection Ҫд��� Section ���ơ�
     * @param [in] lpszKey Ҫд��� Key ���ơ�
     * @param [in] lpszValue Ҫд����ַ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL WriteString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCSTR lpszValue);

    /**
     * ��ָ���� Section �� Key ������Ϊָ�����ַ�����
     * @param [in] lpszSection Ҫд��� Section ���ơ�
     * @param [in] lpszKey Ҫд��� Key ���ơ�
     * @param [in] lpszValue Ҫд����ַ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL WriteString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCWSTR lpszValue);
private:
    /**
     * ���ݸ������ļ�������ȫ·����
     */
    void GetFilePath(__out PSTR lpFilePath, __in DWORD dwSize,
        __in PCSTR lpFileName);
    /**
     * ���ݸ������ļ�������ȫ·����
     */
    void GetFilePath(__out PWSTR lpFilePath, __in DWORD dwSize,
        __in PCWSTR lpFileName);
    /**
     * ����һ�� Key��
     */
    LIterator FindKey(__in PCSTR lpszSection, __in PCSTR lpszKey);
    /**
     * ������һ�� Section��
     */
    LIterator FindNextSection(__in LIterator it);
    /**
     * ����һ�� Section��
     */
    LIterator FindSection(__in PCSTR lpszSection);
    /**
     * ��ָ���� Section ��ָ���� Key ����ȡ�ַ�����
     */
    PSTR GetStringA(__in PCSTR lpszSection, __in PCSTR lpszKey);
    /**
     * ����һ�� LTxtFile �������� ini �ļ���
     */
    void Open(__in LTxtFile* pFile);
    /**
     * ��ָ���� Section �� ָ���� Key ��д���ַ�����
     */
    BOOL WriteStringA(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCSTR lpszValue);
private:
    /**
     * Section ��
     */
    LPtrList m_secList;
};
