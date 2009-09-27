///////////////////////////////////////////////////////////////////////////////
// FileName:    ptrvector.cpp
// Created:     2009/06/06
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 指针向量类实现
///////////////////////////////////////////////////////////////////////////////

#include "..\..\include\pdl_container.h"
#include "..\common\lock.h"

#define VECTOR_DEFMAXCNT    32
#define VECTOR_ITERATING    0x00000001

LPtrVector::LPtrVector(void)
{
    m_dwStatus = 0;
    m_data = NULL;
    m_dwUnitSize = 0;
    m_dwUnitCnt = 0;
    m_dwMaxCnt = 0;
    m_nGrowCnt = 0;
    m_pfnCopy = NULL;
    m_pfnDestroy = NULL;
    m_lock = LDummyLock::Get();
}

LPtrVector::~LPtrVector(void)
{
    Destroy();
}

BOOL LPtrVector::Clear(void)
{
    if (NULL == m_data || (VECTOR_ITERATING & m_dwStatus))
        return FALSE;

    LAutoLock lock(m_lock);
    if (NULL != m_pfnDestroy)
    {
        PBYTE dst = m_data;
        for (DWORD i = 0; i < m_dwUnitCnt; ++i)
        {
            m_pfnDestroy(dst);
            dst += m_dwUnitSize;
        }
    }
    m_dwUnitCnt = 0;
    return TRUE;
}

BOOL LPtrVector::CopyAt(__in int idx, __out PVOID buf)
{
    if (NULL == m_data || (int)m_dwUnitCnt <= idx || 0 == m_dwUnitCnt)
        return FALSE;

    if (idx < 0)
        idx = m_dwUnitCnt - 1;
    PVOID src = DataFromPos(idx);
    CopyMemory(buf, src, m_dwUnitSize);
    if (NULL != m_pfnCopy)
        m_pfnCopy(buf, src);
    return TRUE;
}

BOOL LPtrVector::Create(
    __in DWORD dwUnitSize,
    __in DWORD dwMaxCnt,
    __in int nGrowCnt /* = -1 */,
    __in CopyPtr pfnCopy /* = NULL */,
    __in DestructPtr pfnDestroy /* = NULL */,
    __in ILock* lock /* = NULL */)
{
    if (NULL != m_data)
        Destroy();

    m_dwUnitSize = dwUnitSize;
    m_dwUnitCnt = 0;
    m_dwMaxCnt = (dwMaxCnt > 0) ? dwMaxCnt : VECTOR_DEFMAXCNT;
    m_nGrowCnt = nGrowCnt;
    m_pfnCopy = pfnCopy;
    m_pfnDestroy = pfnDestroy;
    if (NULL != lock)
        m_lock = lock;
    else
        m_lock = LDummyLock::Get();

    m_data = new BYTE[dwUnitSize * dwMaxCnt];
    return NULL != m_data;
}

PDLINLINE PVOID LPtrVector::DataFromPos(__in int idx)
{
    return m_data + m_dwUnitSize * idx;
}

void LPtrVector::Destroy(void)
{
    if (NULL == m_data || (VECTOR_ITERATING & m_dwStatus))
        return;

    LAutoLock lock(m_lock);
    if (NULL != m_pfnDestroy)
    {
        PBYTE dst = m_data;
        for (DWORD i = 0; i < m_dwUnitCnt; ++i)
        {
            m_pfnDestroy(dst);
            dst += m_dwUnitSize;
        }
    }

    delete [] m_data;
    m_data = NULL;
    m_dwUnitSize = 0;
    m_dwUnitCnt = 0;
    m_dwMaxCnt = 0;
    m_nGrowCnt = 0;
    m_pfnCopy = NULL;
    m_pfnDestroy = NULL;

    m_lock = NULL;
}

int LPtrVector::ForEach(__in IteratePtr pfnCallBack, __in PVOID param)
{
    LAutoLock lock(m_lock);
    for (int i = 0; i < (int)m_dwUnitCnt; ++i)
    {
        if (!pfnCallBack(DataFromPos(i), param))
            return i;
    }
    return -1;
}

BOOL LPtrVector::GetAt(__in int idx, __out PVOID buf)
{
    if (NULL == m_data || (int)m_dwUnitCnt <= idx || 0 == m_dwUnitCnt)
        return FALSE;

    if (idx < 0)
        idx = m_dwUnitCnt - 1;
    CopyMemory(buf, DataFromPos(idx), m_dwUnitSize);
    return TRUE;
}

DWORD LPtrVector::GetCount(void)
{
    PDLASSERT(NULL != m_data);
    return m_dwUnitCnt;
}

PDLINLINE ILock* LPtrVector::GetSafeLock(void) const
{
    return (VECTOR_ITERATING & m_dwStatus) ? LDummyLock::Get() : m_lock;
}

void LPtrVector::Grow(void)
{
    DWORD dwMaxCnt = m_dwMaxCnt;
    if (m_nGrowCnt > 0)
        dwMaxCnt += m_nGrowCnt;
    else
        dwMaxCnt *= 2;

    PBYTE newmem = new BYTE[dwMaxCnt * m_dwUnitSize];
    CopyMemory(newmem, m_data, m_dwUnitSize * m_dwUnitCnt);
    delete [] m_data;
    m_data = newmem;
    m_dwMaxCnt = dwMaxCnt;
}

int LPtrVector::InsertAfter(__in int idx, __in LPCVOID pvData)
{
    if (VECTOR_ITERATING & m_dwStatus)
        return -1;

    if (NULL == m_data || idx >= (int)m_dwUnitCnt)
        return -1;

    if (idx < 0)
        idx = m_dwUnitCnt - 1;

    LAutoLock lock(m_lock);
    if (m_dwUnitCnt == m_dwMaxCnt)
        Grow();

    PBYTE dst = (PBYTE)DataFromPos(idx + 1);
    DWORD dwSize = (m_dwUnitCnt - idx - 1) * m_dwUnitSize;
    MoveMemory(dst + m_dwUnitSize, dst, dwSize);
    CopyMemory(dst, pvData, m_dwUnitSize);
    if (NULL != m_pfnCopy)
        m_pfnCopy(dst, pvData);
    ++m_dwUnitCnt;
    return idx + 1;
}

int LPtrVector::InsertBefore(__in int idx, __in LPCVOID pvData)
{
    if (VECTOR_ITERATING & m_dwStatus)
        return -1;

    if (NULL == m_data || idx < 0 || idx >= (int)m_dwUnitCnt)
        return -1;

    LAutoLock lock(m_lock);
    if (m_dwUnitCnt == m_dwMaxCnt)
        Grow();

    PBYTE dst = (PBYTE)DataFromPos(idx);
    DWORD dwSize = (m_dwUnitCnt - idx) * m_dwUnitSize;
    MoveMemory(dst + m_dwUnitSize, dst, dwSize);
    CopyMemory(dst, pvData, m_dwUnitSize);
    if (NULL != m_pfnCopy)
        m_pfnCopy(dst, pvData);
    ++m_dwUnitCnt;
    return idx;
}

BOOL LPtrVector::Remove(__in int idx)
{
    if (VECTOR_ITERATING & m_dwStatus)
        return FALSE;
    if (NULL == m_data || idx >= (int)m_dwUnitCnt)
        return FALSE;

    LAutoLock lock(m_lock);
    --m_dwUnitCnt;
    if (idx < 0)
    {
        if (NULL != m_pfnDestroy)
            m_pfnDestroy(DataFromPos(m_dwUnitCnt));
        return TRUE;
    }

    PBYTE dst = (PBYTE)DataFromPos(idx);
    if (NULL != m_pfnDestroy)
        m_pfnDestroy(dst);

    DWORD dwSize = (m_dwUnitCnt - idx) * m_dwUnitSize;
    MoveMemory(dst, dst + m_dwUnitSize, dwSize);
    return TRUE;
}

BOOL LPtrVector::SetAt(__in int idx, __in LPCVOID pvData)
{
    if (NULL == m_data || idx >= (int)m_dwUnitCnt || 0 == m_dwUnitCnt)
        return FALSE;

    LAutoLock lock(GetSafeLock());
    if (idx < 0)
    {
        idx = m_dwUnitCnt - 1;
        if (m_dwUnitCnt == m_dwMaxCnt)
            Grow();
    }

    PVOID p = DataFromPos(idx);
    if (NULL != m_pfnDestroy)
        m_pfnDestroy(p);
    CopyMemory(p, pvData, m_dwUnitSize);
    if (NULL != m_pfnCopy)
        m_pfnCopy(p, pvData);
    return TRUE;
}

BOOL LPtrVector::Sort(__in ComparePtr pfnCompare)
{
    if (NULL == m_data || 0 == m_dwUnitCnt || NULL == pfnCompare)
        return FALSE;

    LAutoLock lock(m_lock);
    m_dwStatus = VECTOR_ITERATING;
    PBYTE tmp = new BYTE[m_dwUnitSize];

    for (int i = 0; i < (int)m_dwUnitCnt - 1; ++i)
    {
        PVOID p1 = DataFromPos(i);
        for (int j = i + 1; j < (int)m_dwUnitCnt; ++j)
        {
            PVOID p2 = DataFromPos(j);
            if (pfnCompare(p1, p2))
            {
                CopyMemory(tmp, p1, m_dwUnitSize);
                CopyMemory(p1, p2, m_dwUnitSize);
                CopyMemory(p2, tmp, m_dwUnitSize);
            }
        }
    }

    delete [] tmp;
    m_dwStatus &= ~VECTOR_ITERATING;
    return TRUE;
}
