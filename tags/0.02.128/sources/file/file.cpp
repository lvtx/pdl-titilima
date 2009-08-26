#include <pdl_file.h>

LFile::LFile(void) : m_hFile(INVALID_HANDLE_VALUE)
{
}

LFile::~LFile(void)
{
    Close();
}

BOOL LFile::Close(void)
{
    if (INVALID_HANDLE_VALUE == m_hFile)
        return FALSE;

    BOOL ret = ::CloseHandle(m_hFile);
    m_hFile = INVALID_HANDLE_VALUE;
    return ret;
}

BOOL LFile::Create(
    __in PCSTR lpFileName,
    __in DWORD dwDesiredAccess,
    __in DWORD dwShareMode,
    __in DWORD dwCreationDisposition,
    __in DWORD dwFlagsAndAttributes /* = FILE_ATTRIBUTE_NORMAL */)
{
#ifdef _WIN32_WCE
    LStringW str = lpFileName;
    return Create(str, dwDesiredAccess, dwShareMode, dwCreationDisposition,
        dwFlagsAndAttributes);
#else
    m_hFile = ::CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, NULL,
        dwCreationDisposition, dwFlagsAndAttributes, NULL);
    return INVALID_HANDLE_VALUE != m_hFile;
#endif // _WIN32_WCE
}

BOOL LFile::Create(
    __in PCWSTR lpFileName,
    __in DWORD dwDesiredAccess,
    __in DWORD dwShareMode,
    __in DWORD dwCreationDisposition,
    __in DWORD dwFlagsAndAttributes /* = FILE_ATTRIBUTE_NORMAL */)
{
    m_hFile = ::CreateFileW(lpFileName, dwDesiredAccess, dwShareMode, NULL,
        dwCreationDisposition, dwFlagsAndAttributes, NULL);
    return INVALID_HANDLE_VALUE != m_hFile;
}

BOOL LFile::Exists(
    __in PCSTR lpszFileName,
    __in BOOL bIncludeDir)
{
    PDLASSERT(NULL != lpszFileName);
#ifndef _WIN32_WCE
    BOOL bRet;
    WIN32_FIND_DATAA wfd;
    HANDLE hFind;
    hFind = FindFirstFileA(lpszFileName, &wfd);
    bRet = INVALID_HANDLE_VALUE != hFind;
    FindClose(hFind);
    if (bRet && !bIncludeDir)
    {
        if (FILE_ATTRIBUTE_DIRECTORY & wfd.dwFileAttributes)
            bRet = FALSE;
    }
    return bRet;
#else
    LStringW str = lpszFileName;
    return Exists(str);
#endif // _WIN32_WCE
}

BOOL LFile::Exists(
    __in PCWSTR lpszFileName,
    __in BOOL bIncludeDir /* = TRUE */)
{
    PDLASSERT(NULL != lpszFileName);

    BOOL bRet;
    WIN32_FIND_DATAW wfd;
    HANDLE hFind;
    hFind = FindFirstFileW(lpszFileName, &wfd);
    bRet = INVALID_HANDLE_VALUE != hFind;
    FindClose(hFind);
    if (bRet && !bIncludeDir)
    {
        if (FILE_ATTRIBUTE_DIRECTORY & wfd.dwFileAttributes)
            bRet = FALSE;
    }
    return bRet;
}

BOOL LFile::Flush(void)
{
    return ::FlushFileBuffers(m_hFile);
}

HANDLE LFile::GetHandle(void) const
{
    return m_hFile;
}

DWORD LFile::GetPointer(void)
{
    return LFile::SetPointer(0, FILE_CURRENT);
}

DWORD LFile::GetSize(void)
{
    return ::GetFileSize(m_hFile, NULL);
}

BOOL LFile::MatchName(__in PCSTR lpszFileName, __in PCSTR lpszMatch)
{
    PDLASSERT(NULL != lpszFileName && NULL != lpszMatch);

    if ('\0' == *lpszFileName && '\0' == *lpszMatch)
    {
        // 如果都达到了字符串尾部，则说明匹配成功
        return TRUE;
    }
    if ('\0' == *lpszMatch)
    {
        // 如果通配符字符串达到了尾部，则说明不匹配
        return FALSE;
    }
    if ('\0' == *lpszFileName)
    {
        if ('*' == *lpszMatch)
        {
            ++lpszMatch;
            return LFile::MatchName(lpszFileName, lpszMatch);
        }
        else
        {
            return FALSE;
        }
    }
    if ('?' == *lpszMatch)
    {                   
        ++lpszMatch;
        ++lpszFileName;
        return LFile::MatchName(lpszFileName, lpszMatch);
    }
    if('*' == *lpszMatch)
    {
        // 尝试匹配 0 个字符
        ++lpszMatch;
        if (LFile::MatchName(lpszFileName, lpszMatch))
            return TRUE;
        else
            --lpszMatch;

        // 尝试匹配 1 个字符
        ++lpszMatch;
        ++lpszFileName;
        if (LFile::MatchName(lpszFileName, lpszMatch))
        {
            return TRUE;
        }
        else
        {
            --lpszMatch;
            --lpszFileName;
        }
        // 尝试匹配多个字符
        ++lpszFileName;
        return LFile::MatchName(lpszFileName, lpszMatch);
    }
    // 匹配普通字符
    CHAR ch1 = *lpszFileName;
    CHAR ch2 = *lpszMatch;
    // 将字符统一转换为大写
    if ('a' <= ch1 && ch1 <= 'z')
        ch1 += 'Z' - 'z';
    if ('a' <= ch2 && ch2 <= 'z')
        ch2 += 'Z' - 'z';
    if (ch1 != ch2)
        return FALSE;
    ++lpszMatch;
    ++lpszFileName;
    return LFile::MatchName(lpszFileName, lpszMatch);
}

BOOL LFile::MatchName(__in PCWSTR lpszFileName, __in PCWSTR lpszMatch)
{
    PDLASSERT(NULL != lpszFileName && NULL != lpszMatch);

    if (L'\0' == *lpszFileName && L'\0' == *lpszMatch)
    {
        // 如果都达到了字符串尾部，则说明匹配成功
        return TRUE;
    }
    if (L'\0' == *lpszMatch)
    {
        // 如果通配符字符串达到了尾部，则说明不匹配
        return FALSE;
    }
    if (L'\0' == *lpszFileName)
    {
        if (L'*' == *lpszMatch)
        {
            ++lpszMatch;
            return LFile::MatchName(lpszFileName, lpszMatch);
        }
        else
        {
            return FALSE;
        }
    }
    if (L'?' == *lpszMatch)
    {                   
        ++lpszMatch;
        ++lpszFileName;
        return LFile::MatchName(lpszFileName, lpszMatch);
    }
    if(L'*' == *lpszMatch)
    {
        // 尝试匹配 0 个字符
        ++lpszMatch;
        if (LFile::MatchName(lpszFileName, lpszMatch))
            return TRUE;
        else
            --lpszMatch;

        // 尝试匹配 1 个字符
        ++lpszMatch;
        ++lpszFileName;
        if (LFile::MatchName(lpszFileName, lpszMatch))
        {
            return TRUE;
        }
        else
        {
            --lpszMatch;
            --lpszFileName;
        }
        // 尝试匹配多个字符
        ++lpszFileName;
        return LFile::MatchName(lpszFileName, lpszMatch);
    }
    // 匹配普通字符
    WCHAR ch1 = *lpszFileName;
    WCHAR ch2 = *lpszMatch;
    // 将字符统一转换为大写
    if (L'a' <= ch1 && ch1 <= L'z')
        ch1 += L'Z' - L'z';
    if (L'a' <= ch2 && ch2 <= L'z')
        ch2 += L'Z' - L'z';
    if (ch1 != ch2)
        return FALSE;

    ++lpszMatch;
    ++lpszFileName;
    return LFile::MatchName(lpszFileName, lpszMatch);
}

DWORD LFile::Read(__out PVOID lpBuffer, __in DWORD dwSize)
{
    DWORD dwRead;
    if (::ReadFile(m_hFile, lpBuffer, dwSize, &dwRead, NULL))
        return dwRead;
    else
        return -1;
}

DWORD LFile::SetPointer(__in LONG lPointer, __in DWORD dwMoveMethod)
{
    return ::SetFilePointer(m_hFile, lPointer, NULL, dwMoveMethod);
}

DWORD LFile::Write(__in LPCVOID lpBuffer, __in DWORD dwSize)
{
    DWORD dwWritten;
    if (::WriteFile(m_hFile, lpBuffer, dwSize, &dwWritten, NULL))
        return dwWritten;
    else
        return -1;
}
