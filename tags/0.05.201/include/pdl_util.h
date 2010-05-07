/**
 * \file pdl_util.h
 * \brief PDL ������
 * \details ����ļ��ж����� PDL ��һЩ���õĹ����ࣺ
 *   \li \c LBuffer PDL ���ܻ�������
 */

#pragma once

/**
 * \class LBuffer
 * \brief PDL ���ܻ�������
 * \details LBuffer ���ṩ�˶Ի������ķ�װ��
 */
class LBuffer
{
    enum { DEFSIZE = 16 };
    typedef const void* Ptr;
public:
    LBuffer(void)
    {
        m_data = new BYTE[DEFSIZE];
        m_dwSize = DEFSIZE;
    }
    LBuffer(DWORD dwSize, BYTE value = 0)
    {
        PDLASSERT(dwSize > 0);
        m_data = new BYTE[dwSize];
        m_dwSize = dwSize;
        for (DWORD i = 0; i < dwSize; ++i)
            m_data[i] = value;
    }
    LBuffer(Ptr data, DWORD dwSize)
    {
        PDLASSERT(NULL != data && dwSize > 0);
        m_dwSize = dwSize;
        m_data = new BYTE[dwSize];
        CopyMemory(m_data, data, dwSize);
    }
    ~LBuffer(void)
    {
        delete [] m_data;
    }
    BYTE& operator[](int idx)
    {
        PDLASSERT(idx >= 0);
        if ((DWORD)idx >= m_dwSize)
            CheckSize(idx * 2);
        return m_data[idx];
    }
    const BYTE& operator[](int idx) const
    {
        PDLASSERT(idx >= 0 && (DWORD)idx < m_dwSize);
        return m_data[idx];
    }
    operator PVOID(void)
    {
        return m_data;
    }
    operator PBYTE(void)
    {
        return m_data;
    }
    PBYTE operator+(DWORD delta)
    {
        PDLASSERT(delta >= 0);
        return m_data + delta;
    }
public:
    void CopyFrom(Ptr data, DWORD dwSize, DWORD dwPos = 0)
    {
        PDLASSERT(NULL != data && dwSize > 0);
        CheckSize(dwSize + dwPos);
        CopyMemory(m_data + dwPos, data, dwSize);
    }
    DWORD GetSize(void) const
    {
        return m_dwSize;
    }
    BOOL LoadFromFile(PCTSTR lpFile)
    {
        HANDLE hFile = CreateFile(lpFile, GENERIC_READ, FILE_SHARE_READ, NULL,
            OPEN_EXISTING, 0, NULL);
        if (INVALID_HANDLE_VALUE == hFile)
            return FALSE;

        BOOL ret = FALSE;
        DWORD dwSize = GetFileSize(hFile, NULL);
        if (0 != dwSize)
        {
            CheckSize(dwSize);
            ret = ReadFile(hFile, m_data, dwSize, &m_dwSize, NULL);
        }

        CloseHandle(hFile);
        return ret;
    }
    BOOL SaveToFile(PCTSTR lpFile, DWORD dwSize = 0, DWORD dwAttr = FILE_ATTRIBUTE_ARCHIVE)
    {
        HANDLE hFile = CreateFile(lpFile, GENERIC_WRITE, 0, NULL,
            CREATE_ALWAYS, dwAttr, NULL);
        if (INVALID_HANDLE_VALUE == hFile)
            return FALSE;

        DWORD dwWritten;
        if (0 == dwSize)
            dwSize = m_dwSize;
        BOOL ret = WriteFile(hFile, m_data, dwSize, &dwWritten, NULL);
        CloseHandle(hFile);
        return ret;
    }
private:
    void CheckSize(DWORD dwSize)
    {
        if (m_dwSize < dwSize)
        {
            PBYTE data = new BYTE[dwSize];
            CopyMemory(data, m_data, m_dwSize);

            delete [] m_data;
            m_data = data;
            m_dwSize = dwSize;
        }
    }
private:
    PBYTE m_data;
    DWORD m_dwSize;
};
