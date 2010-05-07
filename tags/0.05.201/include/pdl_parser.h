/**
 * \file pdl_parser.h
 * \brief PDL 格式解析工具
 * \details 这个文件中包括了 PDL 中常用的格式解析工具类及函数：
 *   \li \c LParseColorString 颜色字符串解析函数
 *   \li \c LIniParser ini 文件解析类
 *   \li \c LIniSection ini 格式 Section 类
 *   \li \c LXmlParser xml 格式解析类
 */

#pragma once

#include "pdl_base.h"
#include "pdl_string.h"
#include "pdl_container.h"

/**
 * \fn LParseColorString
 * 解析颜色字符串。
 * @param [in] lpszString 要解析的颜色字符串，格式为 "#%R%G%B" 或 "%R,%G,%B"。
 * @param [in] clrDefault 在解析失败的情况下返回的默认颜色值。
 * @return 解析成功的颜色值或 clrDefault 默认指定的颜色值。
 */

COLORREF PDLAPI LParseColorString(
    __in PCSTR lpszString,
    __in COLORREF clrDefault
);

/**
 * \fn LParseColorString
 * 解析颜色字符串。
 * @param [in] lpszString 要解析的颜色字符串，格式为 "#%R%G%B" 或 "%R,%G,%B"。
 * @param [in] clrDefault 在解析失败的情况下返回的默认颜色值。
 * @return 解析成功的颜色值或 clrDefault 默认指定的颜色值。
 */

COLORREF PDLAPI LParseColorString(
    __in PCWSTR lpszString,
    __in COLORREF clrDefault
);

/**
 * \class LIniParser
 * \brief ini 文件解析类
 */

class LTxtFile;
class LIniSection;
class LIniParser
{
public:

    /**
     * 构造函数。
     * @param [in] lock 操作锁。
     */
    LIniParser(__in ILock* lock = NULL);

    ~LIniParser(void);
public:

    /**
     * 关闭一个 ini 文件，不予保存。
     */
    void Close(void);

    /**
     * 从指定的 Section 和 Key 处获取一个整数值。
     * @param [in] lpszSection 要读取的 Section 名称。
     * @param [in] lpszKey 要读取的 Key 名称。
     * @param [in] nDefault 当获取失败时，返回的默认值。
     * @return 如果成功则返回 ini 中保存的数值，否则返回由 nDefault 指定的数值。
     */
    int GetInt(__in PCSTR lpszSection, __in PCSTR lpszKey, __in int nDefault);

    /**
     * 从指定的 Section 名称获得一个 Section 对象。
     * @param [in] lpszSection 要获取的 Section 名称。
     * @param [out] 用于接收返回的 Section 对象。
     * @param [in] 如果 Section 不存在是否创建。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetSection(__in PCSTR lpszSection, __out LIniSection* sec,
        __in BOOL bCreate);

    /**
     * 从指定的 Section 和 Key 处获取一个字符串。
     * @param [in] lpszSection 要读取的 Section 名称。
     * @param [in] lpszKey 要读取的 Key 名称。
     * @param [in] nDefault 当获取失败时，返回的默认值。
     * @param [out] strResult 获取的字符串。
     * @return 获取的字符串长度。
     */
    DWORD GetString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCSTR lpszDefault, __out LStringA* strResult);

    /**
     * 从指定的 Section 和 Key 处获取一个字符串。
     * @param [in] lpszSection 要读取的 Section 名称。
     * @param [in] lpszKey 要读取的 Key 名称。
     * @param [in] nDefault 当获取失败时，返回的默认值。
     * @param [out] strResult 获取的字符串。
     * @return 获取的字符串长度。
     */
    DWORD GetString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCWSTR lpszDefault, __out LStringW* strResult);

    /**
     * 从指定的 Section 和 Key 处获取一个字符串。
     * @param [in] lpszSection 要读取的 Section 名称。
     * @param [in] lpszKey 要读取的 Key 名称。
     * @param [in] nDefault 当获取失败时，返回的默认值。
     * @param [out] lpszBuffer 用于接收结果的缓冲区。
     * @param [in] nSize lpszBuffer 缓冲区的大小，以字符计。
     * @return 获取的字符串长度。
     */
    DWORD GetString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCSTR lpszDefault, __out PSTR lpszBuffer, __in DWORD nSize);

    /**
     * 从指定的 Section 和 Key 处获取一个字符串。
     * @param [in] lpszSection 要读取的 Section 名称。
     * @param [in] lpszKey 要读取的 Key 名称。
     * @param [in] nDefault 当获取失败时，返回的默认值。
     * @param [out] lpszBuffer 用于接收结果的缓冲区。
     * @param [in] nSize lpszBuffer 缓冲区的大小，以字符计。
     * @return 获取的字符串长度。
     */
    DWORD GetString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCWSTR lpszDefault, __out PWSTR lpszBuffer, __in DWORD nSize);

    /**
     * 获取所有 Section 的总数。
     * @return 所有 Section 的总数。
     */
    DWORD GetSectionCount(void);

    /**
     * 打开一个 ini 文件。
     * @param [in] lpszFileName ini 文件的文件名。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Open(__in PCSTR lpszFileName);

    /**
     * 打开一个 ini 文件。
     * @param [in] lpszFileName ini 文件的文件名。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Open(__in PCWSTR lpszFileName);

    /**
     * 借助一个 LTxtFile 对象来打开 ini 文件。
     * @param [in] pFile 一个有效的 LTxtFile 对象指针。
     */
    void Open(__in LTxtFile* pFile);

    /**
     * 移除一个 Key。
     * @param [in] lpszSection 要移除的 Key 所在的 Section 名称。
     * @param [in] lpszKey 要移除的 Key 名称。
     * @return 如果移除成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL RemoveKey(__in PCSTR lpszSection, __in PCSTR lpszKey);

    /**
     * 移除一个 Section。
     * @param [in] lpszSection 要移除的 Section 名称。
     * @return 如果移除成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL RemoveSection(__in PCSTR lpszSection);

    /**
     * 将数据保存为一个 ini 文件。
     * @param [in] lpszFileName ini 文件的文件名。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Save(__in_opt PCSTR lpszFileName);

    /**
     * 将数据保存为一个 ini 文件。
     * @param [in] lpszFileName ini 文件的文件名。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Save(__in_opt PCWSTR lpszFileName);

    /**
     * 将指定的 Section 和 Key 处设置为指定的数值。
     * @param [in] lpszSection 要写入的 Section 名称。
     * @param [in] lpszKey 要写入的 Key 名称。
     * @param [in] nValue 要写入的数值。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL WriteInt(__in PCSTR lpszSection, __in PCSTR lpszKey, __in int nValue);

    /**
     * 将指定的 Section 和 Key 处设置为指定的字符串。
     * @param [in] lpszSection 要写入的 Section 名称。
     * @param [in] lpszKey 要写入的 Key 名称。
     * @param [in] lpszValue 要写入的字符串。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL WriteString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCSTR lpszValue);

    /**
     * 将指定的 Section 和 Key 处设置为指定的字符串。
     * @param [in] lpszSection 要写入的 Section 名称。
     * @param [in] lpszKey 要写入的 Key 名称。
     * @param [in] lpszValue 要写入的字符串。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL WriteString(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCWSTR lpszValue);
private:
    /**
     * 查找一个 Key。
     */
    LIterator FindKey(__in PCSTR lpszSection, __in PCSTR lpszKey);
    /**
     * 查找下一个 Section。
     */
    LIterator FindNextSection(__in LIterator it);
    /**
     * 查找一个 Section。
     */
    LIterator FindSection(__in PCSTR lpszSection);
    /**
     * 从指定的 Section 与指定的 Key 处读取字符串。
     */
    PSTR GetStringA(__in PCSTR lpszSection, __in PCSTR lpszKey);
    /**
     * 向指定的 Section 与 指定的 Key 处写入字符串。
     */
    BOOL WriteStringA(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCSTR lpszValue);
private:
    /**
     * 数据
     */
    LStrList m_data;
    /**
     * Section 表
     */
    LPtrList m_secList;
    /**
     * 状态标志
     */
    DWORD m_dwState;
};

/**
 * \class LIniSection
 * \brief ini 格式 Section 类
 */

class LIniSection
{
    friend class LIniParser;
public:
    LIniSection(void);
public:

    /**
     * 向头部添加一行数据。
     * @param [in] lpKeyName 要设置的键名称。
     * @param [in] lpValue 要设置的值。
     * @return 如果添加成功则返回添加后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator AddHead(__in PCSTR lpKeyName, __in PCSTR lpValue);

    /**
     * 向头部添加一行数据。
     * @param [in] lpKeyName 要设置的键名称。
     * @param [in] lpValue 要设置的值。
     * @return 如果添加成功则返回添加后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator AddHead(__in PCSTR lpKeyName, __in PCWSTR lpValue);

    /**
     * 向头部添加一行数据。
     * @param [in] lpKeyName 要设置的键名称。
     * @param [in] lpValue 要设置的值。
     * @return 如果添加成功则返回添加后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator AddHead(__in PCSTR lpKeyName, __in int nValue);

    /**
     * 向尾部添加一行数据。
     * @param [in] lpKeyName 要设置的键名称。
     * @param [in] lpValue 要设置的值。
     * @return 如果添加成功则返回添加后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator AddTail(__in PCSTR lpKeyName, __in PCSTR lpValue);

    /**
     * 向尾部添加一行数据。
     * @param [in] lpKeyName 要设置的键名称。
     * @param [in] lpValue 要设置的值。
     * @return 如果添加成功则返回添加后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator AddTail(__in PCSTR lpKeyName, __in PCWSTR lpValue);

    /**
     * 向尾部添加一行数据。
     * @param [in] lpKeyName 要设置的键名称。
     * @param [in] lpValue 要设置的值。
     * @return 如果添加成功则返回添加后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator AddTail(__in PCSTR lpKeyName, __in int nValue);

    /**
     * 清空 Section 数据。
     */
    void Clear(void);

    /**
     * 获得第一行数据的迭代器。
     * @return 该 Section 的第一行数据。
     */
    LIterator GetHead(void);

    /**
     * 获得指定行的 Key 名称。
     * @param [in] it 一行数据的迭代器。
     * @param [out] str 用于接收 Key 名称的 LStringA 对象指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetKeyName(__in LIterator it, __out LStringA* str);

    /**
     * 获得指定行的 Key 名称。
     * @param [in] it 一行数据的迭代器。
     * @param [out] str 用于接收 Key 名称的 LStringW 对象指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetKeyName(__in LIterator it, __out LStringW* str);

    /**
     * 获得指定行下一行数据的迭代器。
     * @return 如果成功则返回指定行的下一行数据，否则返回 NULL。
     */
    LIterator GetNext(__in LIterator it);

    /**
     * 获得指定行的值字串。
     * @param [in] it 一行数据的迭代器。
     * @param [out] str 用于接收数据的 LStringA 对象指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetValue(__in LIterator it, __out LStringA* str);

    /**
     * 获得指定行的值字串。
     * @param [in] it 一行数据的迭代器。
     * @param [out] str 用于接收数据的 LStringW 对象指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetValue(__in LIterator it, __out LStringW* str);

    /**
     * 在指定的位置之前插入一个字符串值。
     * @param [in] it 要插入的位置。
     * @param [in] lpKeyName 要插入的键名称。
     * @param [in] lpValue 要插入的值。
     * @return 如果插入成功则返回插入后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator InsertBefore(__in LIterator it, __in PCSTR lpKeyName,
        __in PCSTR lpValue);

    /**
     * 在指定的位置之前插入一个字符串值。
     * @param [in] it 要插入的位置。
     * @param [in] lpKeyName 要插入的键名称。
     * @param [in] lpValue 要插入的值。
     * @return 如果插入成功则返回插入后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator InsertBefore(__in LIterator it, __in PCSTR lpKeyName,
        __in PCWSTR lpValue);

    /**
     * 在指定的位置之前插入一个整数值。
     * @param [in] it 要插入的位置。
     * @param [in] lpKeyName 要插入的键名称。
     * @param [in] nValue 要插入的整数值。
     * @return 如果插入成功则返回插入后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator InsertBefore(__in LIterator it, __in PCSTR lpKeyName,
        __in int nValue);

    /**
     * 在指定的位置之后插入一个字符串值。
     * @param [in] it 要插入的位置。
     * @param [in] lpKeyName 要插入的键名称。
     * @param [in] lpValue 要插入的值。
     * @return 如果插入成功则返回插入后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator InsertAfter(__in LIterator it, __in PCSTR lpKeyName,
        __in PCSTR lpValue);

    /**
     * 在指定的位置之后插入一个字符串值。
     * @param [in] it 要插入的位置。
     * @param [in] lpKeyName 要插入的键名称。
     * @param [in] lpValue 要插入的值。
     * @return 如果插入成功则返回插入后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator InsertAfter(__in LIterator it, __in PCSTR lpKeyName,
        __in PCWSTR lpValue);

    /**
     * 在指定的位置之后插入一个整数值。
     * @param [in] it 要插入的位置。
     * @param [in] lpKeyName 要插入的键名称。
     * @param [in] nValue 要插入的整数值。
     * @return 如果插入成功则返回插入后行的迭代器，否则返回 NULL。
     * \note 此函数并不检查数据是否重复。
     */
    LIterator InsertAfter(__in LIterator it, __in PCSTR lpKeyName,
        __in int nValue);

    /**
     * 获得 Section 是否为空。
     * @return 如果该 Section 为空则返回 TRUE，否则返回 FALSE。
     */
    BOOL IsEmpty(void);

    /**
     * 设置指定行的整数数据。
     * @param [in] it 一行数据的迭代器。
     * @param [in] nValue 要设置的整数数据。
     * @return 如果设置成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL SetInt(__in LIterator it, __in int nValue);

    /**
     * 设置指定行的字符串数据。
     * @param [in] it 一行数据的迭代器。
     * @param [in] lpValue 要设置的字符串数据。
     * @return 如果设置成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL SetValue(__in LIterator it, __in PCSTR lpValue);

    /**
     * 设置指定行的字符串数据。
     * @param [in] it 一行数据的迭代器。
     * @param [in] lpValue 要设置的字符串数据。
     * @return 如果设置成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL SetValue(__in LIterator it, __in PCWSTR lpValue);
private:
    /**
     * ini 状态
     */
    PDWORD m_pState;
    /**
     * ini 数据
     */
    LStrList* m_ini;
    /**
     * 第一行数据
     */
    LIterator m_head;
    /**
     * 下一个 Section
     */
    LIterator m_tail;
};

/**
 * \class LXmlStream
 * \brief xml 数据流类
 */

class LXmlStream
{
public:
    /**
     * 数据流是否到达尾部。
     */
    virtual BOOL Eos(void) = 0;
    /**
     * 从数据流中获取一个字符。
     */
    virtual int GetChar(void) = 0;
};

typedef LIterator LXmlNode;

#define XML_FIRST   LT_FIRST
#define XML_LAST    LT_LAST
#define XML_ROOT    LT_ROOT

/**
 * \class LXmlParser
 * \brief xml 格式解析类
 */

class LXmlParser : protected LPtrTree
{
public:
    /**
    * 构造函数。
    * @param [in] lock 操作锁。
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
     * 添加并解析一段 xml 字符串。
     * @param [in] parent 要添加的父结点。
     * @param [in] lpXmlString 要添加的 xml 字符串。
     * @param [in] dwOrder 创建结点的排列顺序。
     * @return 如果添加成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL AddXml(__in LXmlNode parent, __in PCSTR lpXmlString,
        __in DWORD dwOrder);

    /**
     * 添加并解析一段 xml 字符串。
     * @param [in] parent 要添加的父结点。
     * @param [in] lpXmlString 要添加的 xml 字符串。
     * @param [in] dwOrder 创建结点的排列顺序。
     * @return 如果添加成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL AddXml(__in LXmlNode parent, __in PCWSTR lpXmlString,
        __in DWORD dwOrder);

    /**
     * 关闭一个 xml 文件，不予保存。
     */
    void Close(void);

    /**
     * 创建一个 xml 结点。
     * @param [in] parent 要创建结点的父结点。
     * @param [in] type 要创建结点的类型。
     * @param [in] name 要创建结点的名称。
     * @param [in] dwOrder 要创建结点的排列顺序。
     * @return 如果创建成功则返回结点的指针，否则返回 NULL。
     */
    LXmlNode CreateNode(__in LXmlNode parent, __in NodeType type,
        __in PCSTR name, __in DWORD dwOrder);

    /**
     * 创建一个 xml 结点。
     * @param [in] parent 要创建结点的父结点。
     * @param [in] type 要创建结点的类型。
     * @param [in] name 要创建结点的名称。
     * @param [in] dwOrder 要创建结点的排列顺序。
     * @return 如果创建成功则返回结点的指针，否则返回 NULL。
     */
    LXmlNode CreateNode(__in LXmlNode parent, __in NodeType type,
        __in PCWSTR name, __in DWORD dwOrder);

    /**
     * 查找指定名称的子结点。
     * @param [in] parent 要查找子结点的父结点。
     * @param [in] name 要查找的结点名称。
     * 如果查找成功则返回结点的指针，否则返回 NULL。
     */
    LXmlNode FindChildNode(__in LXmlNode parent, __in PCSTR name);

    /**
     * 查找指定名称的子结点。
     * @param [in] parent 要查找子结点的父结点。
     * @param [in] name 要查找的结点名称。
     * 如果查找成功则返回结点的指针，否则返回 NULL。
     */
    LXmlNode FindChildNode(__in LXmlNode parent, __in PCWSTR name);

    /**
     * 查找指定名称的兄弟结点。
     * @param [in] node 要查找的起始结点。
     * @param [in] name 要查找的结点名称。
     * 如果查找成功则返回结点的指针，否则返回 NULL。
     * \note 查找的过程将不包括 node 结点本身。
     */
    LXmlNode FindSiblingNode(__in LXmlNode node, __in PCSTR name);

    /**
     * 查找指定名称的兄弟结点。
     * @param [in] node 要查找的起始结点。
     * @param [in] name 要查找的结点名称。
     * 如果查找成功则返回结点的指针，否则返回 NULL。
     * \note 查找的过程将不包括 node 结点本身。
     */
    LXmlNode FindSiblingNode(__in LXmlNode node, __in PCWSTR name);

    /**
     * 获取指定结点的子结点。
     * @param [in] it 指定的父结点，使用 XML_ROOT 可以获得根结点。
     * @param [in] type 要获得子结点的类型：
     *   \li \c XML_FIRST 获取第一个子结点。
     *   \li \c XML_LAST 获取最后一个子结点。
     * @return 如果成功则返回指定的子结点，否则返回 NULL。
     */
    LXmlNode GetChild(__in LXmlNode node, __in DWORD type);

    /**
     * 获取指定结点的下一个兄弟结点。
     * @param [in] it 一个有效的结点。
     * @return 如果成功则返回指定结点的下一个兄弟结点，否则返回 NULL。
     */
    LXmlNode GetNextSibling(__in LXmlNode node);

    /**
     * 获取结点的名称。
     * @param [in] node 一个有效的结点。
     * @param [out] name 用于接收结点名称的字符串对象指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetNodeName(__in LXmlNode node, __out LStringA* name);

    /**
     * 获取结点的名称。
     * @param [in] node 一个有效的结点。
     * @param [out] name 用于接收结点名称的字符串对象指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetNodeName(__in LXmlNode node, __out LStringW* name);

    /**
     * 获取结点的属性。
     * @param [in] node 一个有效的结点。
     * @param [in] name 要获取的属性名称。
     * @param [out] value 用于接收结点属性值的字符串对象指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetNodeProperty(__in LXmlNode node, __in PCSTR name,
        __out LStringA* value);

    /**
     * 获取结点的属性。
     * @param [in] node 一个有效的结点。
     * @param [in] name 要获取的属性名称。
     * @param [out] value 用于接收结点属性值的字符串对象指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetNodeProperty(__in LXmlNode node, __in PCSTR name,
        __out LStringW* value);

    /**
     * 获取结点的类型。
     * @param [in] node 一个有效的结点。
     * @param [out] type 用于接收结点类型的变量指针。
     * @return 如果获取成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL GetNodeType(__in LXmlNode node, __out NodeType* type);

    /**
     * 获取指定结点的前一个兄弟结点。
     * @param [in] it 一个有效的结点。
     * @return 如果成功则返回指定结点的前一个兄弟结点，否则返回 NULL。
     */
    LXmlNode GetPrevSibling(__in LXmlNode node);

    /**
     * 打开一个 xml 文件。
     * @param [in] lpszFileName xml 文件的文件名。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Open(__in PCSTR lpszFileName);

    /**
     * 打开一个 xml 文件。
     * @param [in] lpszFileName xml 文件的文件名。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Open(__in PCWSTR lpszFileName);

    /**
     * 解析 xml 文件。
     * @param [in] s 一个有效的 xml 数据流。
     * @return 如果解析成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Parse(__in LXmlStream* s);

    /**
     * 移除所有的子结点，但保留父结点本身。
     * @param [in] parent 要移除子结点的父结点。
     * @return 如果移除成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL RemoveChilds(__in LXmlNode parent);

    /**
     * 移除一个结点及其子结点。
     * @param [in] parent 要移除的结点。
     * @return 如果移除成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL RemoveNode(__in LXmlNode node);

    /**
     * 保存一个 xml 文件。
     * @param [in] lpszFileName xml 文件的文件名。
     * @param [in] strIndent 缩进的分隔字符串。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Save(__in PCSTR lpszFileName, __in PCSTR strIndent = NULL);

    /**
     * 保存一个 xml 文件。
     * @param [in] lpszFileName xml 文件的文件名。
     * @param [in] strIndent 缩进的分隔字符串。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Save(__in PCWSTR lpszFileName, __in PCSTR strIndent = NULL);

    /**
     * 设置结点的属性。
     * @param [in] node 一个有效的结点。
     * @param [in] name 要设置的属性名称。
     * @param [in] value 要设置的属性值。
     * @return 如果设置成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL SetNodeProperty(__in LXmlNode node, __in PCSTR name,
        __in PCSTR value);

    /**
     * 设置结点的属性。
     * @param [in] node 一个有效的结点。
     * @param [in] name 要设置的属性名称。
     * @param [in] value 要设置的属性值。
     * @return 如果设置成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL SetNodeProperty(__in LXmlNode node, __in PCSTR name,
        __in PCWSTR value);

    /**
     * 设置结点的数值属性。
     * @param [in] node 一个有效的结点。
     * @param [in] name 要设置的属性名称。
     * @param [in] value 要设置的属性值。
     * @return 如果设置成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL SetNodeProperty(__in LXmlNode node, __in PCSTR name,
        __in int value);

protected:
    /**
     * 读取一个有效的字符。
     */
    int GetChar(__in LXmlStream* s);
    /**
     * 根据指定的结束符读取一个字符串。
     */
    int GetString(__in LXmlStream* s, __out LStringA* str,
        __in PCSTR lpEnd, __out char* chEnd);
    /**
     * 判断给定的字符是否一个指定的结束符。
     */
    BOOL IsEndChar(__in PCSTR lpEnd, __in char ch);
    /**
     * 输出缩进字符。
     */
    void OutputIndent(__in LTxtFile* file, __in int level,
        __in PCSTR strIndent);
    /**
     * 解析 xml 流。
     */
    BOOL ParseXml(__in LXmlNode parent, __in LXmlStream* s,
        __in DWORD dwOrder);
    /**
     * 通过 LTxtFile 来保存文件。
     */
    void Save(__in LTxtFile* file, __in PCSTR strIndent);
private:
    /**
     * Dirty 标志
     */
    BOOL m_bDirty;
};
