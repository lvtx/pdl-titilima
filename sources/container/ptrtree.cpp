///////////////////////////////////////////////////////////////////////////////
// FileName:    ptrtree.cpp
// Created:     2009/08/29
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 树类实现
///////////////////////////////////////////////////////////////////////////////

#include <pdl_base.h>
#include <pdl_container.h>
#include "..\common\lock.h"

#define TREE_ITERATING  0x00000001

typedef struct _tagTNode TNODE, *PTNODE;
struct _tagTNode {
    PTNODE parent;
    PTNODE prev;
    PTNODE next;
    PTNODE firstchild;
    PTNODE lastchild;
    BYTE data[1];
};

LPtrTree::LPtrTree(void)
{
    m_dwStatus = 0;
    m_itRootFirst = NULL;
    m_itRootLast = NULL;
    m_dwUnitSize = 0;
    m_pfnCopy = NULL;
    m_pfnDestroy = NULL;
    m_lock = &g_lock;
}

LPtrTree::~LPtrTree(void)
{
    Destroy();
}

LIterator LPtrTree::AddChild(
    __in LIterator it,
    __in LPCVOID ptr,
    __in DWORD type)
{
    if (TREE_ITERATING & m_dwStatus)
        return NULL;
    if (LT_FIRST != type && LT_LAST != type)
        return NULL;

    PTNODE node = (PTNODE)New(ptr);
    if (NULL == node)
        return NULL;

    LAutoLock lock(m_lock);
    if (LT_ROOT == it)
    {
        if (LT_FIRST == type)
        {
            node->next = (PTNODE)m_itRootFirst;
            m_itRootFirst = (LIterator)node;
            if (NULL == m_itRootLast)
                m_itRootLast = m_itRootFirst;
            else
                node->next->prev = node;
        }
        else
        {
            node->prev = (PTNODE)m_itRootLast;
            m_itRootLast = (LIterator)node;
            if (NULL == m_itRootFirst)
                m_itRootFirst = m_itRootLast;
            else
                node->prev->next = node;
        }
    }
    else
    {
        PTNODE parent = (PTNODE)it;
        node->parent = parent;
        if (LT_FIRST == type)
        {
            node->next = parent->firstchild;
            parent->firstchild = node;
            if (NULL == parent->lastchild)
                parent->lastchild = parent->firstchild;
            else
                node->next->prev = node;
        }
        else
        {
            node->prev = parent->lastchild;
            parent->lastchild = node;
            if (NULL == parent->firstchild)
                parent->firstchild = parent->lastchild;
            else
                node->prev->next = node;
        }
    }

    return (LIterator)node;
}

BOOL LPtrTree::Clear(void)
{
    if (TREE_ITERATING & m_dwStatus)
        return FALSE;

    LAutoLock lock(m_lock);
    PTNODE node = (PTNODE)m_itRootFirst;

    while (NULL != node)
    {
        PTNODE del = node;
        node = node->next;
        Remove((LIterator)del);
    }

    m_itRootFirst = NULL;
    m_itRootLast = NULL;
    return TRUE;
}

void LPtrTree::Create(
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

void LPtrTree::Destroy(void)
{
    m_dwStatus &= ~TREE_ITERATING;
    Clear();

    m_dwUnitSize = 0;
    m_pfnCopy = NULL;
    m_pfnDestroy = NULL;
    m_lock = NULL;
}

BOOL LPtrTree::GetAt(__in LIterator it, __out PVOID p)
{
    if (NULL == it)
        return FALSE;

    PTNODE node = (PTNODE)it;
    CopyMemory(p, node->data, m_dwUnitSize);
    return TRUE;
}

LIterator LPtrTree::GetChild(__in LIterator it, __in DWORD type)
{
    if (LT_FIRST == type)
    {
        if (LT_ROOT == it)
            return m_itRootFirst;
        else
            return (LIterator)(((PTNODE)it)->firstchild);
    }
    else if (LT_LAST == type)
    {
        if (LT_ROOT == it)
            return m_itRootLast;
        else
            return (LIterator)(((PTNODE)it)->lastchild);
    }
    return NULL;
}

LIterator LPtrTree::GetNextSibling(__in LIterator it)
{
    if (NULL == it)
        return NULL;
    return (LIterator)(((PTNODE)it)->next);
}

LIterator LPtrTree::GetPrevSibling(__in LIterator it)
{
    if (NULL == it)
        return NULL;
    return (LIterator)(((PTNODE)it)->prev);
}

PDLINLINE ILock* LPtrTree::GetSafeLock(void) const
{
    return (TREE_ITERATING & m_dwStatus) ? &g_lock : m_lock;
}

LIterator LPtrTree::New(__in LPCVOID ptr)
{
    PTNODE node = (PTNODE)new BYTE[sizeof(TNODE) + m_dwUnitSize - 1];
    node->parent = NULL;
    node->prev = NULL;
    node->next = NULL;
    node->firstchild = NULL;
    node->lastchild = NULL;
    CopyMemory(node->data, ptr, m_dwUnitSize);
    if (NULL != m_pfnCopy)
        m_pfnCopy(node->data, ptr);
    return (LIterator)node;
}

BOOL LPtrTree::Remove(__in LIterator it)
{
    if (NULL == it || TREE_ITERATING & m_dwStatus)
        return FALSE;

    LAutoLock lock(m_lock);

    // 结点脱链
    PTNODE node = (PTNODE)it;
    PTNODE prev = node->prev;
    PTNODE next = node->next;
    PTNODE parent = node->parent;

    if (NULL != prev)
        prev->next = next;
    else if (NULL != parent)
        parent->firstchild = next;
    else if (m_itRootFirst == it)
        m_itRootFirst = (LIterator)next;

    if (NULL != next)
        next->prev = prev;
    else if (NULL != parent)
        parent->lastchild = prev;
    else if (m_itRootLast == it)
        m_itRootLast = (LIterator)prev;
    node->parent = NULL;
    node->next = NULL;
    node->prev = NULL;

    // 迭代删除子树
    LPtrVector stack;
    stack.Create(sizeof(PTNODE), 16);
    while (NULL != node)
    {
        if (NULL != node->firstchild)
        {
            // 如含有子结点，则将结点入栈
            stack.InsertAfter(-1, &node);
            node = node->firstchild;
            continue;
        }

        // 销毁叶子结点的数据
        if (NULL != m_pfnDestroy)
            m_pfnDestroy(node->data);

        // 销毁叶子结点本身
        PTNODE del = node;
        node = node->next;
        delete [] del;

        if (NULL == node)
        {
            // 本层已无结点，弹出父结点
            if (stack.GetAt(-1, &node))
            {
                stack.Remove(-1);
                node->firstchild = NULL;
                node->lastchild = NULL;
            }
        }
    }

    return TRUE;
}

void LPtrTree::SetAt(__in LIterator it, __in LPCVOID ptr)
{
    LAutoLock lock(GetSafeLock());

    PTNODE node = (PTNODE)it;
    if (NULL != m_pfnDestroy)
        m_pfnDestroy(node->data);

    CopyMemory(node->data, ptr, m_dwUnitSize);
    if (NULL != m_pfnCopy)
        m_pfnCopy(node->data, ptr);
}
