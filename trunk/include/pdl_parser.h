/**
 * \file pdl_parser.h
 * \brief PDL ��ʽ��������
 * \details ����ļ��а����� PDL �г��õĸ�ʽ���������༰������
 *   \li \c LParseColorString ��ɫ�ַ�����������
 *   \li \c LIniParser ini �ļ�������
 *   \li \c LXmlParser xml ��ʽ������
 */

#pragma once

#include "pdl_base.h"
#include "pdl_string.h"
#include "pdl_container.h"

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
class LIniParser : protected LStrList
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
     * @param lpszFileName: ini �ļ����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Open(__in PCSTR lpszFileName);

    /**
     * ��һ�� ini �ļ���
     * @param lpszFileName: ini �ļ����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Open(__in PCWSTR lpszFileName);

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

/**
 * \class LXmlStream
 * \brief xml ��������
 */

class LXmlStream
{
public:
    /**
     * �������Ƿ񵽴�β����
     */
    virtual BOOL Eos(void) = 0;
    /**
     * ���������л�ȡһ���ַ���
     */
    virtual int GetChar(void) = 0;
};

typedef LIterator LXmlNode;

#define XML_FIRST   LT_FIRST
#define XML_LAST    LT_LAST
#define XML_ROOT    LT_ROOT

/**
 * \class LXmlParser
 * \brief xml ��ʽ������
 */

class LXmlParser : protected LPtrTree
{
public:
    /**
    * ���캯����
    * @param [in] lock ��������
    */
    LXmlParser(__in ILock* lock = NULL);

    ~LXmlParser(void);

    enum NodeType
    {
        Prolog,
        Element,
        Text,
        CDATA,
    };

public:

    /**
     * �ر�һ�� xml �ļ������豣�档
     */
    void Close(void);

    /**
     * ����һ�� xml ��㡣
     * @param [in] parent Ҫ�������ĸ���㡣
     * @param [in] type Ҫ�����������͡�
     * @param [in] name Ҫ�����������ơ�
     * @param [in] dwOrder Ҫ������������˳��
     * @return ��������ɹ��򷵻ؽ���ָ�룬���򷵻� NULL��
     */
    LXmlNode CreateNode(__in LXmlNode parent, __in NodeType type,
        __in PCSTR name, __in DWORD dwOrder);

    /**
     * ����һ�� xml ��㡣
     * @param [in] parent Ҫ�������ĸ���㡣
     * @param [in] type Ҫ�����������͡�
     * @param [in] name Ҫ�����������ơ�
     * @param [in] dwOrder Ҫ������������˳��
     * @return ��������ɹ��򷵻ؽ���ָ�룬���򷵻� NULL��
     */
    LXmlNode CreateNode(__in LXmlNode parent, __in NodeType type,
        __in PCWSTR name, __in DWORD dwOrder);

    /**
     * ��ȡָ�������ӽ�㡣
     * @param [in] it ָ���ĸ���㣬ʹ�� XML_ROOT ���Ի�ø���㡣
     * @param [in] type Ҫ����ӽ������ͣ�
     *   \li \c XML_FIRST ��ȡ��һ���ӽ�㡣
     *   \li \c XML_LAST ��ȡ���һ���ӽ�㡣
     * @return ����ɹ��򷵻�ָ�����ӽ�㣬���򷵻� NULL��
     */
    LXmlNode GetChild(__in LXmlNode node, __in DWORD type);

    /**
     * ��ȡָ��������һ���ֵܽ�㡣
     * @param [in] it һ����Ч�Ľ�㡣
     * @return ����ɹ��򷵻�ָ��������һ���ֵܽ�㣬���򷵻� NULL��
     */
    LXmlNode GetNextSibling(__in LXmlNode node);

    /**
     * ��ȡ�������ơ�
     * @param [in] node һ����Ч�Ľ�㡣
     * @param [out] name ���ڽ��ս�����Ƶ��ַ�������ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetNodeName(__in LXmlNode node, __out LStringA* name);

    /**
     * ��ȡ�������ơ�
     * @param [in] node һ����Ч�Ľ�㡣
     * @param [out] name ���ڽ��ս�����Ƶ��ַ�������ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetNodeName(__in LXmlNode node, __out LStringW* name);

    /**
     * ��ȡ�������ԡ�
     * @param [in] node һ����Ч�Ľ�㡣
     * @param [in] name Ҫ��ȡ���������ơ�
     * @param [out] value ���ڽ��ս������ֵ���ַ�������ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetNodeProperty(__in LXmlNode node, __in PCSTR name,
        __out LStringA* value);

    /**
     * ��ȡ�������ԡ�
     * @param [in] node һ����Ч�Ľ�㡣
     * @param [in] name Ҫ��ȡ���������ơ�
     * @param [out] value ���ڽ��ս������ֵ���ַ�������ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetNodeProperty(__in LXmlNode node, __in PCSTR name,
        __out LStringW* value);

    /**
     * ��ȡ�������͡�
     * @param [in] node һ����Ч�Ľ�㡣
     * @param [out] type ���ڽ��ս�����͵ı���ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetNodeType(__in LXmlNode node, __out NodeType* type);

    /**
     * ��ȡָ������ǰһ���ֵܽ�㡣
     * @param [in] it һ����Ч�Ľ�㡣
     * @return ����ɹ��򷵻�ָ������ǰһ���ֵܽ�㣬���򷵻� NULL��
     */
    LXmlNode GetPrevSibling(__in LXmlNode node);

    /**
     * ��һ�� xml �ļ���
     * @param [in] lpszFileName xml �ļ����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Open(__in PCSTR lpszFileName);

    /**
     * ��һ�� xml �ļ���
     * @param [in] lpszFileName xml �ļ����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Open(__in PCWSTR lpszFileName);

    /**
     * ���� xml �ļ���
     * @param [in] s һ����Ч�� xml ��������
     * @return ��������ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Parse(__in LXmlStream* s);

    /**
     * ����һ�� xml �ļ���
     * @param [in] lpszFileName xml �ļ����ļ�����
     * @param [in] strIndent �����ķָ��ַ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Save(__in PCSTR lpszFileName, __in PCSTR strIndent);

    /**
     * ����һ�� xml �ļ���
     * @param [in] lpszFileName xml �ļ����ļ�����
     * @param [in] strIndent �����ķָ��ַ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Save(__in PCWSTR lpszFileName, __in PCSTR strIndent);

    /**
     * ���ý������ԡ�
     * @param [in] node һ����Ч�Ľ�㡣
     * @param [in] name Ҫ���õ��������ơ�
     * @param [in] value Ҫ���õ�����ֵ��
     * @return ������óɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL SetNodeProperty(__in LXmlNode node, __in PCSTR name,
        __in PCSTR value);

    /**
     * ���ý������ԡ�
     * @param [in] node һ����Ч�Ľ�㡣
     * @param [in] name Ҫ���õ��������ơ�
     * @param [in] value Ҫ���õ�����ֵ��
     * @return ������óɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL SetNodeProperty(__in LXmlNode node, __in PCSTR name,
        __in PCWSTR value);

    /**
     * ���ý�����ֵ���ԡ�
     * @param [in] node һ����Ч�Ľ�㡣
     * @param [in] name Ҫ���õ��������ơ�
     * @param [in] value Ҫ���õ�����ֵ��
     * @return ������óɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL SetNodeProperty(__in LXmlNode node, __in PCSTR name,
        __in int value);

protected:
    /**
     * ��ȡһ����Ч���ַ���
     */
    int GetChar(__in LXmlStream* s);
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
     * ����ָ���Ľ�������ȡһ���ַ�����
     */
    int GetString(__in LXmlStream* s, __out LStringA* str,
        __in PCSTR lpEnd, __out char* chEnd);
    /**
     * �жϸ������ַ��Ƿ�һ��ָ���Ľ�������
     */
    BOOL IsEndChar(__in PCSTR lpEnd, __in char ch);
    /**
     * ��������ַ���
     */
    void OutputIndent(__in LTxtFile* file, __in int level,
        __in PCSTR strIndent);
    /**
     * ͨ�� LTxtFile �������ļ���
     */
    void Save(__in LTxtFile* file, __in PCSTR strIndent);
};
