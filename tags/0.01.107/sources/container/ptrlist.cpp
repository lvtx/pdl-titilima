///////////////////////////////////////////////////////////////////////////////
// FileName:    ptrlist.cpp
// Created:     2009/06/06
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 指针链表类实现
///////////////////////////////////////////////////////////////////////////////

#include <pdl_container.h>
#include "..\common\lock.h"

#define LIST_ITERATING  0x00000001

typedef struct _tagNode NODE, *PNODE;
struct _tagNode {
    PNODE prev;
    PNODE next;
    BYTE data[1];
};

LPtrList::LPtrList(void)
{
    m_dwStatus = 0;
    m_itHead = NULL;
    m_itTail = NULL;
    m_dwUnitSize = 0;
    m_pfnCopy = NULL;
    m_pfnDestroy = NULL;
    m_lock = NULL;
}

LPtrList::~LPtrList(void)
{
    Destroy();
}

PVOID LPtrList::AddHead(__in LPCVOID ptr)
{
    if (LIST_ITERATING & m_dwStatus)
        return NULL;

    PNODE node = (PNODE)New(ptr);
    if (NULL == node)
        return NULL;

    LAutoLock lock(m_lock);
    if (NULL == m_itHead)
    {
        m_itHead = node;
        m_itTail = node;
    }
    else
    {
        PNODE head = (PNODE)m_itHead;
        node->next = head;
        head->prev = node;
        m_itHead = node;
    }
    return node->data;
}

PVOID LPtrList::AddTail(__in LPCVOID ptr)
{
    if (LIST_ITERATING & m_dwStatus)
        return NULL;

    PNODE node = (PNODE)New(ptr);
    if (NULL == node)
        return NULL;

    LAutoLock lock(m_lock);
    if (NULL == m_itTail)
    {
        m_itHead = node;
        m_itTail = node;
    }
    else
    {
        PNODE tail = (PNODE)m_itTail;
        node->prev = (PNODE)m_itTail;
        tail->next = node;
        m_itTail = node;
    }
    return node->data;
}

BOOL LPtrList::Clear(void)
{
    if (NULL == m_itHead || (LIST_ITERATING & m_dwStatus))
        return FALSE;

    LAutoLock lock(m_lock);
    PNODE node = (PNODE)m_itHead;
    while (NULL != node)
    {
        if (NULL != m_pfnDestroy)
            m_pfnDestroy(node->data);

        PNODE del = node;
        node = node->next;
        delete [] del;
    }

    m_itHead = NULL;
    m_itTail = NULL;
    return TRUE;
}

void LPtrList::Create(
    __in DWORD dwUnitSize,
    __in CopyPtr pfnCopy /* = NULL */,
    __in DestructPtr pfnDestroy /* = NULL */,
    __in ILock* lock /* = NULL */)
{
    Destroy();

    m_dwUnitSize = dwUnitSize;
    m_pfnCopy = pfnCopy;
    m_pfnDestroy = pfnDestroy;
    if (NULL != lock)
        m_lock = lock;
    else
        m_lock = &g_lock;
}

void LPtrList::Destroy(void)
{
    if (Clear())
    {
        m_dwUnitSize = 0;
        m_pfnCopy = NULL;
        m_pfnDestroy = NULL;
        m_lock = NULL;
    }
}

LIterator LPtrList::ForEach(__in IteratePtr pfnCallBack, __in PVOID param)
{
    if (NULL == m_itHead)
        return NULL;

    LAutoLock lock(m_lock);
    m_dwStatus = LIST_ITERATING;
    PNODE node = (PNODE)m_itHead;
    while (NULL != node)
    {
        if (!pfnCallBack(this, node, param))
            break;

        node = node->next;
    }

    m_dwStatus &= ~LIST_ITERATING;
    return node;
}

BOOL LPtrList::GetAt(__in LIterator it, __out PVOID p)
{
    if (NULL == it)
        return FALSE;

    PNODE node = (PNODE)it;
    CopyMemory(p, node->data, m_dwUnitSize);
    return TRUE;
}

DWORD LPtrList::GetCount(void)
{
    if (NULL == m_itHead)
        return 0;

    DWORD ret = 0;
    LAutoLock lock(GetSafeLock());
    PNODE node = (PNODE)m_itHead;
    while (NULL != node)
    {
        ++ret;
        node = node->next;
    }
    return ret;
}

LIterator LPtrList::GetHeadIterator(void)
{
    return m_itHead;
}

void LPtrList::GetNextIterator(__inout LIterator* it)
{
    PNODE node = (PNODE)*it;
    *it = node->next;
}

void LPtrList::GetPrevIterator(__inout LIterator* it)
{
    PNODE node = (PNODE)*it;
    *it = node->prev;
}

PDLINLINE ILock* LPtrList::GetSafeLock(void) const
{
    return (LIST_ITERATING & m_dwStatus) ? &g_lock : m_lock;
}

LIterator LPtrList::GetTailIterator(void)
{
    return m_itTail;
}

LIterator LPtrList::InsertAfter(__in LIterator it, __in LPCVOID ptr)
{
    if (LIST_ITERATING & m_dwStatus)
        return NULL;

    if (m_itTail == it)
        return AddTail(ptr);

    PNODE node = (PNODE)New(ptr);
    if (NULL == node)
        return NULL;

    LAutoLock lock(m_lock);
    PNODE prev = (PNODE)it;
    PNODE next = prev->next;
    node->prev = prev;
    node->next = next;
    next->prev = node;
    prev->next = node;
    return node;
}

LIterator LPtrList::InsertBefore(__in LIterator it, __in LPCVOID ptr)
{
    if (LIST_ITERATING & m_dwStatus)
        return NULL;

    if (m_itHead == it)
        return AddHead(ptr);

    PNODE node = (PNODE)New(ptr);
    if (NULL == node)
        return NULL;

    LAutoLock lock(m_lock);
    PNODE next = (PNODE)it;
    PNODE prev = next->prev;
    node->prev = prev;
    node->next = next;
    next->prev = node;
    prev->next = node;
    return node;
}

LIterator LPtrList::New(__in LPCVOID ptr)
{
    PNODE node = (PNODE)new BYTE[sizeof(NODE) + m_dwUnitSize - 1];
    node->prev = NULL;
    node->next = NULL;
    CopyMemory(node->data, ptr, m_dwUnitSize);
    if (NULL != m_pfnCopy)
        m_pfnCopy(node->data, ptr);
    return node;
}

BOOL LPtrList::Remove(__in LIterator it)
{
    if (NULL == it || LIST_ITERATING & m_dwStatus)
        return FALSE;

    LAutoLock lock(m_lock);
    PNODE node = (PNODE)it;
    PNODE prev = node->prev;
    PNODE next = node->next;
    if (NULL != m_pfnDestroy)
        m_pfnDestroy(node->data);

    if (m_itHead == it)
    {
        m_itHead = next;
        if (NULL != next)
            next->prev = NULL;
    }
    else
    {
        prev->next = next;
    }
    if (m_itTail == it)
    {
        m_itTail = prev;
        if (NULL != prev)
            prev->next = NULL;
    }
    else
    {
        next->prev = prev;
    }
    delete [] node;
    return TRUE;
}

void LPtrList::SetAt(__in LIterator it, __in LPCVOID ptr)
{
    LAutoLock lock(GetSafeLock());

    PNODE node = (PNODE)it;
    if (NULL != m_pfnDestroy)
        m_pfnDestroy(node->data);
    CopyMemory(node->data, ptr, m_dwUnitSize);
    if (NULL != m_pfnCopy)
        m_pfnCopy(node->data, ptr);
}
