///////////////////////////////////////////////////////////////////////////////
// FileName:    ptrlist.cpp
// Created:     2009/06/06
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 指针链表类实现
///////////////////////////////////////////////////////////////////////////////

#include "..\..\include\pdl_container.h"
#include "..\common\lock.h"
#include "..\..\include\pdl_util.h"

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
    m_lock = LDummyLock::Get();
}

LPtrList::~LPtrList(void)
{
    Destroy();
}

LIterator LPtrList::AddHead(__in LPCVOID ptr)
{
    if (LIST_ITERATING & m_dwStatus)
        return NULL;

    PNODE node = (PNODE)New(ptr);
    if (NULL == node)
        return NULL;

    LAutoLock lock(m_lock);
    if (NULL == m_itHead)
    {
        m_itHead = (LIterator)node;
        m_itTail = (LIterator)node;
    }
    else
    {
        PNODE head = (PNODE)m_itHead;
        node->next = head;
        head->prev = node;
        m_itHead = (LIterator)node;
    }
    return (LIterator)node;
}

LIterator LPtrList::AddTail(__in LPCVOID ptr)
{
    if (LIST_ITERATING & m_dwStatus)
        return NULL;

    PNODE node = (PNODE)New(ptr);
    if (NULL == node)
        return NULL;

    LAutoLock lock(m_lock);
    if (NULL == m_itTail)
    {
        m_itHead = (LIterator)node;
        m_itTail = (LIterator)node;
    }
    else
    {
        PNODE tail = (PNODE)m_itTail;
        node->prev = (PNODE)m_itTail;
        tail->next = node;
        m_itTail = (LIterator)node;
    }
    return (LIterator)node;
}

BOOL LPtrList::Clear(void)
{
    if (LIST_ITERATING & m_dwStatus)
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

BOOL LPtrList::CopyAt(__in LIterator it, __out PVOID p)
{
    if (NULL == it)
        return FALSE;

    PNODE node = (PNODE)it;
    CopyMemory(p, node->data, m_dwUnitSize);
    if (NULL != m_pfnCopy)
        m_pfnCopy(p, node->data);
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
        m_lock = LDummyLock::Get();
}

void LPtrList::Destroy(void)
{
    if (0 == m_dwUnitSize)
        return;

    m_dwStatus &= ~LIST_ITERATING;
    Clear();

    m_dwUnitSize = 0;
    m_pfnCopy = NULL;
    m_pfnDestroy = NULL;
    m_lock = NULL;
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
        if (!pfnCallBack(node->data, param))
            break;

        node = node->next;
    }

    m_dwStatus &= ~LIST_ITERATING;
    return (LIterator)node;
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

LIterator LPtrList::GetNextIterator(__in LIterator it)
{
    if (NULL == it)
        return NULL;
    return (LIterator)(((PNODE)it)->next);
}

LIterator LPtrList::GetPrevIterator(__in LIterator it)
{
    if (NULL == it)
        return NULL;
    return (LIterator)(((PNODE)it)->prev);
}

PDLINLINE ILock* LPtrList::GetSafeLock(void) const
{
    return (LIST_ITERATING & m_dwStatus) ? LDummyLock::Get() : m_lock;
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
    return (LIterator)node;
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
    return (LIterator)node;
}

void LPtrList::Modify(__in LIterator it, __in LPCVOID ptr)
{
    LAutoLock lock(GetSafeLock());

    PNODE node = (PNODE)it;
    CopyMemory(node->data, ptr, m_dwUnitSize);
}

LIterator LPtrList::New(__in LPCVOID ptr)
{
    PNODE node = (PNODE)new BYTE[sizeof(NODE) + m_dwUnitSize - 1];
    node->prev = NULL;
    node->next = NULL;
    CopyMemory(node->data, ptr, m_dwUnitSize);
    if (NULL != m_pfnCopy)
        m_pfnCopy(node->data, ptr);
    return (LIterator)node;
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
        m_itHead = (LIterator)next;
        if (NULL != next)
            next->prev = NULL;
    }
    else
    {
        prev->next = next;
    }
    if (m_itTail == it)
    {
        m_itTail = (LIterator)prev;
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

BOOL LPtrList::Sort(__in ComparePtr pfnCompare)
{
    if (NULL == m_itHead || NULL == pfnCompare || m_itHead == m_itTail)
        return FALSE;

    LAutoLock lock(m_lock);
    m_dwStatus = LIST_ITERATING;
    PBYTE tmp = new BYTE[m_dwUnitSize];

    PNODE p1 = (PNODE)m_itHead;
    while ((LIterator)p1 != m_itTail)
    {
        PNODE p2 = p1->next;
        while (NULL != p2)
        {
            if (pfnCompare(p1->data, p2->data))
            {
                CopyMemory(tmp, p1->data, m_dwUnitSize);
                CopyMemory(p1->data, p2->data, m_dwUnitSize);
                CopyMemory(p2->data, tmp, m_dwUnitSize);
            }

            p2 = p2->next;
        }

        p1 = p1->next;
    }

    delete [] tmp;
    m_dwStatus &= ~LIST_ITERATING;
    return TRUE;
}
