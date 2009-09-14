#include "..\..\include\pdl_file.h"
#include "..\..\include\pdl_string.h"
#include "file.h"

#define TXTBUF_SIZE     4096
#define UNICODE_HEADER  0xfeff

///////////////////////////////////////////////////////////////////////////////
// LTxtFile

LTxtFile::LTxtFile(void) : LFile()
{
    m_dwFlags = OPFLAG_WRITE;
    m_cbChar = sizeof(char);
    m_buf = new BYTE[TXTBUF_SIZE];
    m_ptr = 0;
    m_rwptr = 0;
}

LTxtFile::~LTxtFile(void)
{
    Close();
}

BOOL LTxtFile::Close(void)
{
    Flush();
    return LFile::Close();
}

DWORD LTxtFile::CopyToBuffer(__out PVOID buf, __in DWORD dwCnt)
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

int LTxtFile::FindCRLF(void)
{
    for (int i = m_ptr; i < TXTBUF_SIZE; i += m_cbChar)
    {
        char ch = m_buf[i];
        if ('\r' == ch || '\n' == ch)
            return i;
    }
    return -1;
}

BOOL LTxtFile::Flush(void)
{
    if (!(OPFLAG_WRITE & m_dwFlags))
        return FALSE;

    DWORD cbWrite = m_ptr - m_rwptr;
    if (cbWrite > 0)
        LFile::Write(m_buf + m_rwptr, cbWrite);
    return TRUE;
}

int LTxtFile::GetChar(void)
{
    if (OPFLAG_EOF & m_dwFlags)
        return LEOF;

    StartRead();
    if (m_ptr >= TXTBUF_SIZE || 0 == m_rwptr)
        ReadBlock();

    int ch;
    if (FILEFLAG_UNICODE & m_dwFlags)
    {
        ch = *(PCWSTR)(m_buf + m_ptr);
        m_ptr += sizeof(WCHAR);
    }
    else
    {
        ch = *(PCSTR)(m_buf + m_ptr);
        m_ptr += sizeof(char);
    }

    if (m_rwptr < TXTBUF_SIZE && m_ptr >= m_rwptr)
        m_dwFlags |= OPFLAG_EOF;
    if (m_ptr >= TXTBUF_SIZE)
        ReadBlock();
    return ch;
}

BOOL LTxtFile::Open(__in PCSTR lpFileName, __in MODE mode)
{
    DWORD dwAccess = 0;
    DWORD dwFlags = 0;
    switch (mode)
    {
    case LTxtFile::modeAppend:
        dwAccess = GENERIC_WRITE;
        dwFlags = OPEN_ALWAYS;
        break;
    case LTxtFile::modeReadWrite:
        dwAccess = GENERIC_READ | GENERIC_WRITE;
        dwFlags = OPEN_ALWAYS;
        break;
    case LTxtFile::modeReset:
        dwAccess = GENERIC_WRITE;
        dwFlags = CREATE_ALWAYS;
        break;
    default:
        return FALSE;
    }

    // 判断是否 Unicode 文件
    LFile file;
    if (file.Create(lpFileName, GENERIC_READ, FILE_SHARE_READ, OPEN_EXISTING))
    {
        WORD hdr = 0;
        DWORD dwRead = file.Read(&hdr, sizeof(WORD));
        if (sizeof(WORD) == dwRead && UNICODE_HEADER == hdr)
        {
            m_cbChar = sizeof(WCHAR);
            m_dwFlags |= FILEFLAG_UNICODE;
        }
        file.Close();
    }

    if (!LFile::Create(lpFileName, dwAccess, FILE_SHARE_READ, dwFlags))
        return FALSE;

    switch (mode)
    {
    case LTxtFile::modeAppend:
        {
            LFile::SetPointer(0, FILE_END);
            m_dwFlags |= OPFLAG_EOF | OPFLAG_WRITE;
        }
        break;
    case LTxtFile::modeReset:
        {
            if (FILEFLAG_UNICODE & m_dwFlags)
            {
                WORD hdr = UNICODE_HEADER;
                LFile::Write(&hdr, sizeof(WORD));
            }
            m_dwFlags |= OPFLAG_EOF | OPFLAG_WRITE;
        }
        break;
    case LTxtFile::modeReadWrite:
        {
            if (FILEFLAG_UNICODE & m_dwFlags)
                LFile::SetPointer(sizeof(WORD), FILE_BEGIN);
            m_dwFlags |= OPFLAG_WRITE;
            if (LFile::GetSize() == LFile::GetPointer())
                m_dwFlags |= OPFLAG_EOF;
        }
        break;
    }

    m_ptr = 0;
    m_rwptr = 0;
    return TRUE;
}

BOOL LTxtFile::Open(__in PCWSTR lpFileName, __in MODE mode)
{
    DWORD dwAccess = 0;
    DWORD dwFlags = 0;
    switch (mode)
    {
    case LTxtFile::modeAppend:
        dwAccess = GENERIC_WRITE;
        dwFlags = OPEN_ALWAYS;
        break;
    case LTxtFile::modeReadWrite:
        dwAccess = GENERIC_READ | GENERIC_WRITE;
        dwFlags = OPEN_ALWAYS;
        break;
    case LTxtFile::modeReset:
        dwAccess = GENERIC_WRITE;
        dwFlags = CREATE_ALWAYS;
        break;
    default:
        return FALSE;
    }

    // 判断是否 Unicode 文件
    LFile file;
    if (file.Create(lpFileName, GENERIC_READ, FILE_SHARE_READ, OPEN_EXISTING))
    {
        WORD hdr = 0;
        DWORD dwRead = file.Read(&hdr, sizeof(WORD));
        if (sizeof(WORD) == dwRead && UNICODE_HEADER == hdr)
        {
            m_cbChar = sizeof(WCHAR);
            m_dwFlags |= FILEFLAG_UNICODE;
        }
        file.Close();
    }

    if (!LFile::Create(lpFileName, dwAccess, FILE_SHARE_READ, dwFlags))
        return FALSE;

    switch (mode)
    {
    case LTxtFile::modeAppend:
        {
            LFile::SetPointer(0, FILE_END);
            m_dwFlags |= OPFLAG_EOF | OPFLAG_WRITE;
        }
        break;
    case LTxtFile::modeReset:
        {
            if (FILEFLAG_UNICODE & m_dwFlags)
            {
                WORD hdr = UNICODE_HEADER;
                LFile::Write(&hdr, sizeof(WORD));
            }
            m_dwFlags |= OPFLAG_EOF | OPFLAG_WRITE;
        }
        break;
    case LTxtFile::modeReadWrite:
        {
            if (FILEFLAG_UNICODE & m_dwFlags)
                LFile::SetPointer(sizeof(WORD), FILE_BEGIN);
            m_dwFlags |= OPFLAG_WRITE;
            if (LFile::GetSize() == LFile::GetPointer())
                m_dwFlags |= OPFLAG_EOF;
        }
        break;
    }

    m_ptr = 0;
    m_rwptr = 0;
    return TRUE;
}

int LTxtFile::PrintF(__in PCSTR format, ...)
{
    char str[1024];
    va_list arglist;
    va_start(arglist, format);
    int cnt = wvsprintfA(str, format, arglist);
    va_end(arglist);

    Write(str);
    return cnt;
}

int LTxtFile::PrintF(__in PCWSTR format, ...)
{
    WCHAR str[1024];
    va_list arglist;
    va_start(arglist, format);
    int cnt = wvsprintfW(str, format, arglist);
    va_end(arglist);

    Write(str);
    return cnt;
}

void LTxtFile::PutChar(int ch)
{
    if (FILEFLAG_UNICODE & m_dwFlags)
    {
        WCHAR w[2] = { (WCHAR)ch, L'\0' };
        Write(w);
    }
    else
    {
        char c[2] = { (char)ch, '\0' };
        Write(c);
    }
}

DWORD LTxtFile::Read(__out LStringA* str, __in DWORD dwSize)
{
    if (FILEFLAG_UNICODE & m_dwFlags)
    {
        LStringW strW;
        if (0 == Read(&strW, dwSize))
            return 0;

        str->Copy(strW);
        return str->GetLength();
    }

    if (Eof())
        return 0;

    StartRead();
    PSTR buf = str->AllocBuffer(dwSize, FALSE);
    DWORD cbRead = 0;
    DWORD ret = 0;
    while (dwSize > 0 && !Eof())
    {
        cbRead = CopyToBuffer(buf + cbRead, dwSize);
        dwSize -= cbRead;
        ret += cbRead;
        ReadBlock();
    }

    return ret;
}

DWORD LTxtFile::Read(__out LStringW* str, __in DWORD dwSize)
{
    if (!(FILEFLAG_UNICODE & m_dwFlags))
    {
        LStringA strA;
        if (0 == Read(&strA, dwSize))
            return 0;

        str->Copy(strA);
        return str->GetLength();
    }

    if (Eof())
        return 0;

    StartRead();
    PWSTR buf = str->AllocBuffer(dwSize, FALSE);
    DWORD cbRead = 0;
    DWORD ret = 0;
    dwSize *= sizeof(WCHAR);
    while (dwSize > 0 && !Eof())
    {
        cbRead = CopyToBuffer(buf + cbRead, dwSize);
        dwSize -= cbRead;
        ret += cbRead;
        ReadBlock();
    }

    ret /= sizeof(WCHAR);
    return ret;
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
    if (FILEFLAG_UNICODE & m_dwFlags)
    {
        LStringW strW;
        if (!ReadLn(&strW))
            return FALSE;

        str->Copy(strW);
        return TRUE;
    }

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

BOOL LTxtFile::ReadLn(__out LStringW* str)
{
    if (!(FILEFLAG_UNICODE & m_dwFlags))
    {
        LStringA strA;
        if (!ReadLn(&strA))
            return FALSE;

        str->Copy(strA);
        return TRUE;
    }

    if (Eof())
        return FALSE;

    StartRead();

    PWSTR buf = str->AllocBuffer(0, FALSE);
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
            buf = str->AllocBuffer((cbRead + cbCopy) / sizeof(WCHAR), TRUE);
            CopyToBuffer(buf + cbRead, cbCopy);
        }

        cbRead += cbCopy;
    } while (-1 == crlf && !Eof());
    buf[cbRead / sizeof(WCHAR)] = L'\0';

    // 移动换行符指针
    if (-1 != crlf)
    {
        char ch = m_buf[crlf];
        if ('\r' == ch)
            m_ptr += 2 * sizeof(WCHAR);
        else
            m_ptr += sizeof(WCHAR);
    }

    // 指针是否越界？
    if (m_ptr >= TXTBUF_SIZE)
        ReadBlock();
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

    if (FILEFLAG_UNICODE & m_dwFlags)
    {
        LStringW strW = str;
        return Write(strW);
    }

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

BOOL LTxtFile::Write(__in PCWSTR str)
{
    if (NULL == str)
        return FALSE;

    if (!(FILEFLAG_UNICODE & m_dwFlags))
    {
        LStringA strA = str;
        return Write(strA);
    }

    StartWrite();
    while (L'\0' != *str)
    {
        *((PWSTR)(m_buf + m_ptr)) = *str;
        m_ptr += sizeof(WCHAR); ++str;
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

BOOL LTxtFile::WriteLn(__in PCWSTR str)
{
    if (!Write(str))
        return FALSE;

    Write(L"\r\n");
    return TRUE;
}
