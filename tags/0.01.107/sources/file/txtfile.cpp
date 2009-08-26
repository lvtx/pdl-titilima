#include <pdl_file.h>
#include <pdl_string.h>
#include "file.h"

#define TXTBUF_SIZE 4096

LTxtFile::LTxtFile(void) : LFile()
{
    m_dwFlags = OPFLAG_WRITE;
    m_buf = new char[TXTBUF_SIZE];
    m_ptr = 0;
    m_rwptr = 0;
}

LTxtFile::~LTxtFile(void)
{
    Close();
}

BOOL LTxtFile::Append(__in PCSTR lpFileName)
{
    if (INVALID_HANDLE_VALUE != m_hFile)
        return FALSE;

    if (!LFile::Create(lpFileName, GENERIC_WRITE, FILE_SHARE_READ,
        OPEN_ALWAYS))
    {
        return FALSE;
    }

    LFile::SetPointer(0, FILE_END);
    m_dwFlags |= OPFLAG_EOF | OPFLAG_WRITE;
    m_ptr = 0;
    m_rwptr = 0;
    return TRUE;
}

BOOL LTxtFile::Append(__in PCWSTR lpFileName)
{
    if (INVALID_HANDLE_VALUE != m_hFile)
        return FALSE;

    if (!LFile::Create(lpFileName, GENERIC_WRITE, FILE_SHARE_READ,
        OPEN_ALWAYS))
    {
        return FALSE;
    }

    LFile::SetPointer(0, FILE_END);
    m_dwFlags |= OPFLAG_EOF | OPFLAG_WRITE;
    m_ptr = 0;
    m_rwptr = 0;
    return TRUE;
}

BOOL LTxtFile::Close(void)
{
    Flush();
    return LFile::Close();
}

DWORD LTxtFile::CopyToBuffer(__out PSTR buf, __in DWORD dwCnt)
{
    PDLASSERT(!(m_dwFlags & OPFLAG_WRITE));

    DWORD cbLeft = m_rwptr - m_ptr;
    if (cbLeft == 0)
        return 0;

    DWORD cbCopy = dwCnt > cbLeft ? cbLeft : dwCnt;
    CopyMemory(buf, m_buf + m_ptr, cbCopy);
    m_ptr += cbCopy;

    if (m_ptr == m_rwptr && m_ptr < TXTBUF_SIZE)
        m_dwFlags |= OPFLAG_EOF;
    return cbCopy;
}

BOOL LTxtFile::Eof(void)
{
    if (OPFLAG_EOF & m_dwFlags)
        return TRUE;
    else
        return FALSE;
}

BOOL LTxtFile::Flush(void)
{
    if (!(OPFLAG_WRITE & m_dwFlags))
        return FALSE;

    DWORD cbWrite = m_ptr - m_rwptr;
    if (cbWrite > 0)
        LFile::Write(m_buf + m_ptr, cbWrite);
    return TRUE;
}

int LTxtFile::FindCRLF(void)
{
    for (int i = m_ptr; i < TXTBUF_SIZE; ++i)
    {
        char ch = m_buf[i];
        if ('\r' == ch || '\n' == ch)
            return i;
    }
    return -1;
}

BOOL LTxtFile::Open(__in PCSTR lpFileName)
{
    if (INVALID_HANDLE_VALUE != m_hFile)
        return FALSE;

    if (!LFile::Create(lpFileName, GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_READ, OPEN_ALWAYS))
    {
        return FALSE;
    }

    m_dwFlags |= OPFLAG_WRITE;
    m_ptr = 0;
    m_rwptr = 0;
    return TRUE;
}

BOOL LTxtFile::Open(__in PCWSTR lpFileName)
{
    if (INVALID_HANDLE_VALUE != m_hFile)
        return FALSE;

    if (!LFile::Create(lpFileName, GENERIC_READ | GENERIC_WRITE,
        FILE_SHARE_READ, OPEN_ALWAYS))
    {
        return FALSE;
    }

    m_dwFlags |= OPFLAG_WRITE;
    m_ptr = 0;
    m_rwptr = 0;
    return TRUE;
}

BOOL LTxtFile::Read(__out LStringA* str, __in DWORD dwSize)
{
    if (Eof())
        return FALSE;

    StartRead();
    PSTR buf = str->AllocBuffer(dwSize, FALSE);
    DWORD cbRead = 0;
    while (dwSize > 0 && !Eof())
    {
        cbRead = CopyToBuffer(buf + cbRead, dwSize);
        dwSize -= cbRead;
        ReadBlock();
    }

    return TRUE;
}

DWORD LTxtFile::ReadBlock(void)
{
    PDLASSERT(!(m_dwFlags & OPFLAG_WRITE));
    PDLASSERT(!(m_dwFlags & OPFLAG_EOF));

    m_ptr = 0;
    m_rwptr = LFile::Read(m_buf, TXTBUF_SIZE);
    if (0 == m_rwptr)
        m_dwFlags |= OPFLAG_EOF;
    return m_rwptr;
}

BOOL LTxtFile::ReadLn(__out LStringA* str)
{
    if (Eof())
        return FALSE;

    StartRead();

    PSTR buf = str->AllocBuffer(0, FALSE);
    DWORD cbRead = 0;
    int crlf = 0;
    do
    {
        if (m_ptr == m_rwptr)
            ReadBlock();

        // 查找换行符
        crlf = FindCRLF();

        // 计算要复制的字符数
        DWORD cbCopy;
        if (-1 == crlf)
            cbCopy = m_rwptr - m_ptr;
        else
            cbCopy = crlf - m_ptr;

        // 复制字符
        if (cbCopy > 0)
        {
            buf = str->AllocBuffer(cbRead + cbCopy, TRUE);
            CopyToBuffer(buf + cbRead, cbCopy);
        }

        cbRead += cbCopy;
    } while (-1 == crlf && !Eof());
    buf[cbRead] = '\0';

    // 移动换行符指针
    if (-1 != crlf)
    {
        char ch = m_buf[crlf];
        if ('\r' == ch)
            m_ptr += 2;
        else
            ++m_ptr;
    }

    // 指针是否越界？
    if (m_ptr >= TXTBUF_SIZE)
        ReadBlock();
    return TRUE;
}

BOOL LTxtFile::Reset(__in PCSTR lpFileName)
{
    if (INVALID_HANDLE_VALUE != m_hFile)
        return FALSE;

    if (!LFile::Create(lpFileName, GENERIC_WRITE, FILE_SHARE_READ,
        CREATE_ALWAYS))
    {
        return FALSE;
    }

    m_dwFlags |= OPFLAG_EOF | OPFLAG_WRITE;
    m_ptr = 0;
    m_rwptr = 0;
    return TRUE;
}

BOOL LTxtFile::Reset(__in PCWSTR lpFileName)
{
    if (INVALID_HANDLE_VALUE != m_hFile)
        return FALSE;

    if (!LFile::Create(lpFileName, GENERIC_WRITE, FILE_SHARE_READ,
        CREATE_ALWAYS))
    {
        return FALSE;
    }

    m_dwFlags |= OPFLAG_EOF | OPFLAG_WRITE;
    m_ptr = 0;
    m_rwptr = 0;
    return TRUE;
}

void LTxtFile::StartRead(void)
{
    if (OPFLAG_WRITE & m_dwFlags)
    {
        Flush();
        m_dwFlags &= ~OPFLAG_WRITE;
    }
}

void LTxtFile::StartWrite(void)
{
    if (!(OPFLAG_WRITE & m_dwFlags))
    {
        // 忽略未处理的数据
        LONG cbDelta = m_rwptr - m_ptr;
        if (cbDelta > 0)
            LFile::SetPointer(-cbDelta, FILE_CURRENT);

        m_ptr = 0;
        m_rwptr = 0;
    }
}

BOOL LTxtFile::Write(__in PCSTR str)
{
    if (NULL == str)
        return FALSE;

    StartWrite();
    while ('\0' != *str)
    {
        m_buf[m_ptr] = *str;
        ++m_ptr; ++str;
        if (m_ptr == TXTBUF_SIZE)
        {
            // 缓冲区满，写入数据
            LFile::Write(m_buf + m_rwptr, m_ptr - m_rwptr);
            m_ptr = m_rwptr = 0;
        }
    }

    return TRUE;
}

BOOL LTxtFile::WriteLn(__in PCSTR str)
{
    if (!Write(str))
        return FALSE;

    Write("\r\n");
    return TRUE;
}
