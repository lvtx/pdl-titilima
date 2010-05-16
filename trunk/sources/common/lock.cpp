///////////////////////////////////////////////////////////////////////////////
// FileName:    lock.cpp
// Created:     2009/06/06
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 锁的全局变量定义
///////////////////////////////////////////////////////////////////////////////

#include "..\..\include\pdl_container.h"
#include "lock.h"

ILock* LDummyLock::Get(void)
{
    static LDummyLock lock;
    return &lock;
}

ILock* ILock::Create(void)
{
    return new LLock;
}

ILock* ILock::Create(__in PCSTR lpName)
{
    return new LGlobalLock(lpName);
}

ILock* ILock::Create(__in PCWSTR lpName)
{
    return new LGlobalLock(lpName);
}

LLock::LLock(void)
{
    ::InitializeCriticalSection(&m_cs);
}

LLock::~LLock(void)
{
    ::DeleteCriticalSection(&m_cs);
}

void LLock::Destroy(void)
{
    delete this;
}

void LLock::Lock(void)
{
    EnterCriticalSection(&m_cs);
}

void LLock::Unlock(void)
{
    LeaveCriticalSection(&m_cs);
}

LGlobalLock::LGlobalLock(__in PCSTR lpName)
{
    m_tid = 0;
    m_cnt = 0;
    m_hMutex = ::CreateMutexA(NULL, FALSE, lpName);
}

LGlobalLock::LGlobalLock(__in PCWSTR lpName)
{
    m_tid = 0;
    m_cnt = 0;
    m_hMutex = ::CreateMutexW(NULL, FALSE, lpName);
}

LGlobalLock::~LGlobalLock(void)
{
    ::CloseHandle(m_hMutex);
}

void LGlobalLock::Destroy(void)
{
    delete this;
}

void LGlobalLock::Lock(void)
{
    DWORD tid = GetCurrentThreadId();
    if (tid == m_tid)
    {
        ++m_cnt;
        return;
    }

    WaitForSingleObject(m_hMutex, INFINITE);
    m_tid = tid;
    m_cnt = 1;
}

void LGlobalLock::Unlock(void)
{
    if (GetCurrentThreadId() == m_tid)
        --m_cnt;
    if (m_cnt > 0)
        return;

    m_tid = 0;
    m_cnt = 0;
    ReleaseMutex(m_hMutex);
}
