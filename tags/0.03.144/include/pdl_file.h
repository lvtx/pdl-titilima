/**
 * \file pdl_file.h
 * \brief PDL 文件操作相关类
 * \details 这个文件中定义了 PDL 中所有的文件操作类：
 *   \li \c LFile PDL 基本文件操作类
 *   \li \c LTxtFile PDL 文本文件操作类
*/

#pragma once

#include "pdl_base.h"

/**
 * \class LFile
 * \brief PDL 基本文件操作类
 * \details LFile 是 PDL 最基本文件操作封装，它封装了大部分常用的文件操作 API 函数。
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
     * 判断给定的文件是否存在。
     * @param [in] lpszFileName 要判断的文件名。
     * @param [in] bIncludeDir 是否包含目录。
     * @return 如果文件存在则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI Exists(__in PCSTR lpszFileName,
        __in BOOL bIncludeDir = TRUE);

    /**
     * 判断给定的文件是否存在。
     * @param [in] lpszFileName 要判断的文件名。
     * @param [in] bIncludeDir 是否包含目录。
     * @return 如果文件存在则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI Exists(__in PCWSTR lpszFileName,
        __in BOOL bIncludeDir = TRUE);

    BOOL Flush(void);

    /**
     * 获取文件的句柄。
     * @return 文件对象对应的句柄。
     */
    HANDLE GetHandle(void) const;

    DWORD GetPointer(void);
    DWORD GetSize(void);

    /**
     * 判断给定的文件名是否全路径。
     * @param [in] lpszFileName 要判断的文件名。
     * @return 如果给定的文件名是全路径则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI IsFullPathName(__in PCSTR lpszFileName);

    /**
     * 判断给定的文件名是否全路径。
     * @param [in] lpszFileName 要判断的文件名。
     * @return 如果给定的文件名是全路径则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI IsFullPathName(__in PCWSTR lpszFileName);

    /**
     * 匹配文件名。
     * @param [in] lpszFileName 要匹配的文件名。
     * @param [in] lpszMatch 要匹配的模式字符串。
     * @return 如果匹配成功则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI MatchName(__in PCSTR lpszFileName,
        __in PCSTR lpszMatch);

    /**
     * 匹配文件名。
     * @param [in] lpszFileName 要匹配的文件名。
     * @param [in] lpszMatch 要匹配的模式字符串。
     * @return 如果匹配成功则返回 TRUE，否则返回 FALSE。
     */
    static BOOL PDLAPI MatchName(__in PCWSTR lpszFileName,
        __in PCWSTR lpszMatch);

    DWORD Read(__out PVOID lpBuffer, __in DWORD dwSize);
    DWORD SetPointer(__in LONG lPointer, __in DWORD dwMoveMethod);
    DWORD Write(__in LPCVOID lpBuffer, __in DWORD dwSize);
protected:
    /**
     * 文件对象对应的句柄
     */
    HANDLE m_hFile;
};

/**
 * \class LTxtFile
 * \brief PDL 文本文件操作类
 * \details LTxtFile 是 PDL 对文本文件操作的封装类。
 */

class LStringA;
class LStringW;
class LTxtFile : protected LFile
{
public:
    typedef enum {
        modeAppend,      // 追加模式
        modeReadWrite,   // 读写模式
        modeReset        // 清空模式
    } MODE;
    LTxtFile(void);
    ~LTxtFile(void);
public:

    BOOL Close(void);

    /**
     * 是否已达文件末尾。
     * @return 如果当前已经到达文件末尾则返回 TRUE，否则返回 FALSE。
     */
    BOOL Eof(void);

    BOOL Flush(void);

    /**
     * 读取一个字符。
     * @return 如果读取成功则返回读取到的字符，否则返回 LEOF。
     */
    int GetChar(void);

    BOOL IsUnicode(void);

    /**
     * 以指定的模式打开一个文本文件。
     * @param [in] lpFileName 要打开的文件名。
     * @param [in] mode 打开文件的模式。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     * \sa Close
     */
    BOOL Open(__in PCSTR lpFileName, __in MODE mode);

    /**
     * 以指定的模式打开一个文本文件。
     * @param [in] lpFileName 要打开的文件名。
     * @param [in] mode 打开文件的模式。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     * \sa Close
     */
    BOOL Open(__in PCWSTR lpFileName, __in MODE mode);

    /**
     * 写入格式化的字符串。
     * @param [in] format 字符串的格式。
     * @return 实际写入的字符数。
     */
    int PrintF(__in PCSTR format, ...);

    /**
     * 写入格式化的字符串。
     * @param [in] format 字符串的格式。
     * @return 实际写入的字符数。
     */
    int PrintF(__in PCWSTR format, ...);

    /**
     * 写入一个字符。
     * @param [in] ch 要写入的字符。
     */
    void PutChar(int ch);

    /**
     * 读取文本。
     * @param [out] str 读取数据输出的字符串。
     * @param [in] 要读取的字符数。
     * @return 实际读到的字符数。
     */
    DWORD Read(__out LStringA* str, __in DWORD dwSize);

    /**
     * 读取文本。
     * @param [out] str 读取数据输出的字符串。
     * @param [in] 要读取的字符数。
     * @return 实际读到的字符数。
     */
    DWORD Read(__out LStringW* str, __in DWORD dwSize);

    /**
     * 读取一行文本。
     * @param [out] str 读取数据输出的字符串。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL ReadLn(__out LStringA* str);

    /**
     * 读取一行文本。
     * @param [out] str 读取数据输出的字符串。
     * @return 如果成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL ReadLn(__out LStringW* str);

    /**
     * 向文件中写入文本。
     * @param [in] str 要写入的字符串。
     * @return 如果写入成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Write(__in PCSTR str);

    /**
     * 向文件中写入文本。
     * @param [in] str 要写入的字符串。
     * @return 如果写入成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL Write(__in PCWSTR str);

    /**
     * 向文件中写入一行文本。
     * @param [in] str 要写入的字符串。
     * @return 如果写入成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL WriteLn(__in PCSTR str);

    /**
     * 向文件中写入一行文本。
     * @param [in] str 要写入的字符串。
     * @return 如果写入成功则返回 TRUE，否则返回 FALSE。
     */
    BOOL WriteLn(__in PCWSTR str);

protected:
    /**
     * 将当前缓存中的数据复制到目标缓冲区。
     */
    DWORD CopyToBuffer(__out PVOID buf, __in DWORD dwCnt);
    /**
     * 查找换行符。
     */
    int FindCRLF(void);
    /**
     * 读取缓存块。
     */
    DWORD ReadBlock(void);
    /**
     * 开始读操作。
     */
    void StartRead(void);
    /**
     * 开始写操作。
     */
    void StartWrite(void);
protected:
    /**
     * 文件操作标志
     */
    DWORD m_dwFlags;
    /**
     * 字符大小
     */
    DWORD m_cbChar;
    /**
     * 数据缓冲区
     */
    PBYTE m_buf;
    /**
     * 当前缓冲区指针
     */
    DWORD m_ptr;
    /**
     * R - 可读数据的大小；W - 待写入的数据位置
     */
    DWORD m_rwptr;
};
