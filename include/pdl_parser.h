/**
 * \file pdl_parser.h
 * \brief PDL 格式解析工具
 * \details 这个文件中包括了 PDL 中常用的格式解析工具类及函数：
 *   \li \c LParseColorString 颜色字符串解析函数
 *   \li \c LIniParser ini 文件解析类
 */

#pragma once

#include <pdl_base.h>
#include <pdl_string.h>
#include <pdl_container.h>

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
class LIniParser : protected LStrListA
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
     * @param lpszFileName: ini 文件的文件名，如果为 NULL，则打开与应用程序同名的 ini 文件。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Open(__in_opt PCSTR lpszFileName);

    /**
     * 打开一个 ini 文件。
     * @param lpszFileName: ini 文件的文件名，如果为 NULL，则打开与应用程序同名的 ini 文件。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Open(__in_opt PCWSTR lpszFileName);

    /**
     * 移除一个 Section。
     * @param [in] 要移除的 Section 名称。
     */
    void RemoveSection(__in PCSTR lpszSection);

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
     * 根据给定的文件名生成全路径。
     */
    void GetFilePath(__out PSTR lpFilePath, __in DWORD dwSize,
        __in PCSTR lpFileName);
    /**
     * 根据给定的文件名生成全路径。
     */
    void GetFilePath(__out PWSTR lpFilePath, __in DWORD dwSize,
        __in PCWSTR lpFileName);
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
     * 借助一个 LTxtFile 对象来打开 ini 文件。
     */
    void Open(__in LTxtFile* pFile);
    /**
     * 向指定的 Section 与 指定的 Key 处写入字符串。
     */
    BOOL WriteStringA(__in PCSTR lpszSection, __in PCSTR lpszKey,
        __in PCSTR lpszValue);
private:
    /**
     * Section 表
     */
    LPtrList m_secList;
};
