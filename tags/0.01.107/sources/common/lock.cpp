///////////////////////////////////////////////////////////////////////////////
// FileName:    lock.cpp
// Created:     2009/06/06
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: 锁的全局变量定义
///////////////////////////////////////////////////////////////////////////////

#include <pdl_container.h>
#include "lock.h"

LDummyLock g_lock;

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
    ::EnterCriticalSection(&m_cs);
}

void LLock::Unlock(void)
{
    ::LeaveCriticalSection(&m_cs);
}

LGlobalLock::LGlobalLock(__in PCSTR lpName)
{
    m_hMutex = ::CreateMutexA(NULL, FALSE, lpName);
}

LGlobalLock::LGlobalLock(__in PCWSTR lpName)
{
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
    ::WaitForSingleObject(m_hMutex, INFINITE);
}

void LGlobalLock::Unlock(void)
{
    ::ReleaseMutex(m_hMutex);
}
