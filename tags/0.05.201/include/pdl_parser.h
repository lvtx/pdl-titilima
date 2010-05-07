/**
 * \file pdl_parser.h
 * \brief PDL ��ʽ��������
 * \details ����ļ��а����� PDL �г��õĸ�ʽ���������༰������
 *   \li \c LParseColorString ��ɫ�ַ�����������
 *   \li \c LIniParser ini �ļ�������
 *   \li \c LIniSection ini ��ʽ Section ��
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
class LIniSection;
class LIniParser
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
     * ��ָ���� Section ���ƻ��һ�� Section ����
     * @param [in] lpszSection Ҫ��ȡ�� Section ���ơ�
     * @param [out] ���ڽ��շ��ص� Section ����
     * @param [in] ��� Section �������Ƿ񴴽���
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetSection(__in PCSTR lpszSection, __out LIniSection* sec,
        __in BOOL bCreate);

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
     * @param [in] lpszFileName ini �ļ����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Open(__in PCSTR lpszFileName);

    /**
     * ��һ�� ini �ļ���
     * @param [in] lpszFileName ini �ļ����ļ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Open(__in PCWSTR lpszFileName);

    /**
     * ����һ�� LTxtFile �������� ini �ļ���
     * @param [in] pFile һ����Ч�� LTxtFile ����ָ�롣
     */
    void Open(__in LTxtFile* pFile);

    /**
     * �Ƴ�һ�� Key��
     * @param [in] lpszSection Ҫ�Ƴ��� Key ���ڵ� Section ���ơ�
     * @param [in] lpszKey Ҫ�Ƴ��� Key ���ơ�
     * @return ����Ƴ��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL RemoveKey(__in PCSTR lpszSection, __in PCSTR lpszKey);

    /**
     * �Ƴ�һ�� Section��
     * @param [in] lpszSection Ҫ�Ƴ��� Section ���ơ�
     * @return ����Ƴ��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL RemoveSection(__in PCSTR lpszSection);

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
     * ��ָ���� Section �� ָ���� Key ��д���ַ�����
     */
    BOOL WriteStringA(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCSTR lpszValue);
private:
    /**
     * ����
     */
    LStrList m_data;
    /**
     * Section ��
     */
    LPtrList m_secList;
    /**
     * ״̬��־
     */
    DWORD m_dwState;
};

/**
 * \class LIniSection
 * \brief ini ��ʽ Section ��
 */

class LIniSection
{
    friend class LIniParser;
public:
    LIniSection(void);
public:

    /**
     * ��ͷ�����һ�����ݡ�
     * @param [in] lpKeyName Ҫ���õļ����ơ�
     * @param [in] lpValue Ҫ���õ�ֵ��
     * @return �����ӳɹ��򷵻���Ӻ��еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator AddHead(__in PCSTR lpKeyName, __in PCSTR lpValue);

    /**
     * ��ͷ�����һ�����ݡ�
     * @param [in] lpKeyName Ҫ���õļ����ơ�
     * @param [in] lpValue Ҫ���õ�ֵ��
     * @return �����ӳɹ��򷵻���Ӻ��еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator AddHead(__in PCSTR lpKeyName, __in PCWSTR lpValue);

    /**
     * ��ͷ�����һ�����ݡ�
     * @param [in] lpKeyName Ҫ���õļ����ơ�
     * @param [in] lpValue Ҫ���õ�ֵ��
     * @return �����ӳɹ��򷵻���Ӻ��еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator AddHead(__in PCSTR lpKeyName, __in int nValue);

    /**
     * ��β�����һ�����ݡ�
     * @param [in] lpKeyName Ҫ���õļ����ơ�
     * @param [in] lpValue Ҫ���õ�ֵ��
     * @return �����ӳɹ��򷵻���Ӻ��еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator AddTail(__in PCSTR lpKeyName, __in PCSTR lpValue);

    /**
     * ��β�����һ�����ݡ�
     * @param [in] lpKeyName Ҫ���õļ����ơ�
     * @param [in] lpValue Ҫ���õ�ֵ��
     * @return �����ӳɹ��򷵻���Ӻ��еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator AddTail(__in PCSTR lpKeyName, __in PCWSTR lpValue);

    /**
     * ��β�����һ�����ݡ�
     * @param [in] lpKeyName Ҫ���õļ����ơ�
     * @param [in] lpValue Ҫ���õ�ֵ��
     * @return �����ӳɹ��򷵻���Ӻ��еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator AddTail(__in PCSTR lpKeyName, __in int nValue);

    /**
     * ��� Section ���ݡ�
     */
    void Clear(void);

    /**
     * ��õ�һ�����ݵĵ�������
     * @return �� Section �ĵ�һ�����ݡ�
     */
    LIterator GetHead(void);

    /**
     * ���ָ���е� Key ���ơ�
     * @param [in] it һ�����ݵĵ�������
     * @param [out] str ���ڽ��� Key ���Ƶ� LStringA ����ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetKeyName(__in LIterator it, __out LStringA* str);

    /**
     * ���ָ���е� Key ���ơ�
     * @param [in] it һ�����ݵĵ�������
     * @param [out] str ���ڽ��� Key ���Ƶ� LStringW ����ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetKeyName(__in LIterator it, __out LStringW* str);

    /**
     * ���ָ������һ�����ݵĵ�������
     * @return ����ɹ��򷵻�ָ���е���һ�����ݣ����򷵻� NULL��
     */
    LIterator GetNext(__in LIterator it);

    /**
     * ���ָ���е�ֵ�ִ���
     * @param [in] it һ�����ݵĵ�������
     * @param [out] str ���ڽ������ݵ� LStringA ����ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetValue(__in LIterator it, __out LStringA* str);

    /**
     * ���ָ���е�ֵ�ִ���
     * @param [in] it һ�����ݵĵ�������
     * @param [out] str ���ڽ������ݵ� LStringW ����ָ�롣
     * @return �����ȡ�ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL GetValue(__in LIterator it, __out LStringW* str);

    /**
     * ��ָ����λ��֮ǰ����һ���ַ���ֵ��
     * @param [in] it Ҫ�����λ�á�
     * @param [in] lpKeyName Ҫ����ļ����ơ�
     * @param [in] lpValue Ҫ�����ֵ��
     * @return �������ɹ��򷵻ز�����еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator InsertBefore(__in LIterator it, __in PCSTR lpKeyName,
        __in PCSTR lpValue);

    /**
     * ��ָ����λ��֮ǰ����һ���ַ���ֵ��
     * @param [in] it Ҫ�����λ�á�
     * @param [in] lpKeyName Ҫ����ļ����ơ�
     * @param [in] lpValue Ҫ�����ֵ��
     * @return �������ɹ��򷵻ز�����еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator InsertBefore(__in LIterator it, __in PCSTR lpKeyName,
        __in PCWSTR lpValue);

    /**
     * ��ָ����λ��֮ǰ����һ������ֵ��
     * @param [in] it Ҫ�����λ�á�
     * @param [in] lpKeyName Ҫ����ļ����ơ�
     * @param [in] nValue Ҫ���������ֵ��
     * @return �������ɹ��򷵻ز�����еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator InsertBefore(__in LIterator it, __in PCSTR lpKeyName,
        __in int nValue);

    /**
     * ��ָ����λ��֮�����һ���ַ���ֵ��
     * @param [in] it Ҫ�����λ�á�
     * @param [in] lpKeyName Ҫ����ļ����ơ�
     * @param [in] lpValue Ҫ�����ֵ��
     * @return �������ɹ��򷵻ز�����еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator InsertAfter(__in LIterator it, __in PCSTR lpKeyName,
        __in PCSTR lpValue);

    /**
     * ��ָ����λ��֮�����һ���ַ���ֵ��
     * @param [in] it Ҫ�����λ�á�
     * @param [in] lpKeyName Ҫ����ļ����ơ�
     * @param [in] lpValue Ҫ�����ֵ��
     * @return �������ɹ��򷵻ز�����еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator InsertAfter(__in LIterator it, __in PCSTR lpKeyName,
        __in PCWSTR lpValue);

    /**
     * ��ָ����λ��֮�����һ������ֵ��
     * @param [in] it Ҫ�����λ�á�
     * @param [in] lpKeyName Ҫ����ļ����ơ�
     * @param [in] nValue Ҫ���������ֵ��
     * @return �������ɹ��򷵻ز�����еĵ����������򷵻� NULL��
     * \note �˺���������������Ƿ��ظ���
     */
    LIterator InsertAfter(__in LIterator it, __in PCSTR lpKeyName,
        __in int nValue);

    /**
     * ��� Section �Ƿ�Ϊ�ա�
     * @return ����� Section Ϊ���򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL IsEmpty(void);

    /**
     * ����ָ���е��������ݡ�
     * @param [in] it һ�����ݵĵ�������
     * @param [in] nValue Ҫ���õ��������ݡ�
     * @return ������óɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL SetInt(__in LIterator it, __in int nValue);

    /**
     * ����ָ���е��ַ������ݡ�
     * @param [in] it һ�����ݵĵ�������
     * @param [in] lpValue Ҫ���õ��ַ������ݡ�
     * @return ������óɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL SetValue(__in LIterator it, __in PCSTR lpValue);

    /**
     * ����ָ���е��ַ������ݡ�
     * @param [in] it һ�����ݵĵ�������
     * @param [in] lpValue Ҫ���õ��ַ������ݡ�
     * @return ������óɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL SetValue(__in LIterator it, __in PCWSTR lpValue);
private:
    /**
     * ini ״̬
     */
    PDWORD m_pState;
    /**
     * ini ����
     */
    LStrList* m_ini;
    /**
     * ��һ������
     */
    LIterator m_head;
    /**
     * ��һ�� Section
     */
    LIterator m_tail;
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
     * ��Ӳ�����һ�� xml �ַ�����
     * @param [in] parent Ҫ��ӵĸ���㡣
     * @param [in] lpXmlString Ҫ��ӵ� xml �ַ�����
     * @param [in] dwOrder ������������˳��
     * @return �����ӳɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL AddXml(__in LXmlNode parent, __in PCSTR lpXmlString,
        __in DWORD dwOrder);

    /**
     * ��Ӳ�����һ�� xml �ַ�����
     * @param [in] parent Ҫ��ӵĸ���㡣
     * @param [in] lpXmlString Ҫ��ӵ� xml �ַ�����
     * @param [in] dwOrder ������������˳��
     * @return �����ӳɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL AddXml(__in LXmlNode parent, __in PCWSTR lpXmlString,
        __in DWORD dwOrder);

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
     * ����ָ�����Ƶ��ӽ�㡣
     * @param [in] parent Ҫ�����ӽ��ĸ���㡣
     * @param [in] name Ҫ���ҵĽ�����ơ�
     * ������ҳɹ��򷵻ؽ���ָ�룬���򷵻� NULL��
     */
    LXmlNode FindChildNode(__in LXmlNode parent, __in PCSTR name);

    /**
     * ����ָ�����Ƶ��ӽ�㡣
     * @param [in] parent Ҫ�����ӽ��ĸ���㡣
     * @param [in] name Ҫ���ҵĽ�����ơ�
     * ������ҳɹ��򷵻ؽ���ָ�룬���򷵻� NULL��
     */
    LXmlNode FindChildNode(__in LXmlNode parent, __in PCWSTR name);

    /**
     * ����ָ�����Ƶ��ֵܽ�㡣
     * @param [in] node Ҫ���ҵ���ʼ��㡣
     * @param [in] name Ҫ���ҵĽ�����ơ�
     * ������ҳɹ��򷵻ؽ���ָ�룬���򷵻� NULL��
     * \note ���ҵĹ��̽������� node ��㱾��
     */
    LXmlNode FindSiblingNode(__in LXmlNode node, __in PCSTR name);

    /**
     * ����ָ�����Ƶ��ֵܽ�㡣
     * @param [in] node Ҫ���ҵ���ʼ��㡣
     * @param [in] name Ҫ���ҵĽ�����ơ�
     * ������ҳɹ��򷵻ؽ���ָ�룬���򷵻� NULL��
     * \note ���ҵĹ��̽������� node ��㱾��
     */
    LXmlNode FindSiblingNode(__in LXmlNode node, __in PCWSTR name);

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
     * �Ƴ����е��ӽ�㣬����������㱾��
     * @param [in] parent Ҫ�Ƴ��ӽ��ĸ���㡣
     * @return ����Ƴ��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL RemoveChilds(__in LXmlNode parent);

    /**
     * �Ƴ�һ����㼰���ӽ�㡣
     * @param [in] parent Ҫ�Ƴ��Ľ�㡣
     * @return ����Ƴ��ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL RemoveNode(__in LXmlNode node);

    /**
     * ����һ�� xml �ļ���
     * @param [in] lpszFileName xml �ļ����ļ�����
     * @param [in] strIndent �����ķָ��ַ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Save(__in PCSTR lpszFileName, __in PCSTR strIndent = NULL);

    /**
     * ����һ�� xml �ļ���
     * @param [in] lpszFileName xml �ļ����ļ�����
     * @param [in] strIndent �����ķָ��ַ�����
     * @return ����ɹ��򷵻� TRUE�����򷵻� FALSE��
     */
    BOOL Save(__in PCWSTR lpszFileName, __in PCSTR strIndent = NULL);

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
     * ���� xml ����
     */
    BOOL ParseXml(__in LXmlNode parent, __in LXmlStream* s,
        __in DWORD dwOrder);
    /**
     * ͨ�� LTxtFile �������ļ���
     */
    void Save(__in LTxtFile* file, __in PCSTR strIndent);
private:
    /**
     * Dirty ��־
     */
    BOOL m_bDirty;
};
