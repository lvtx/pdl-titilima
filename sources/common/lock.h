///////////////////////////////////////////////////////////////////////////////
// FileName:    lock.h
// Created:     2009/06/06
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: ²Ù×÷ËøÀà
///////////////////////////////////////////////////////////////////////////////

#pragma once

#include "..\..\include\pdl_container.h"

class LDummyLock : public ILock
{
public:
    static ILock* Get(void);
    void Destroy(void) { delete this; }
    void Lock(void) { /* Dummy */ }
    void Unlock(void) { /* Dummy */ }
};

class LLock : public ILock
{
public:
    LLock(void);
    ~LLock(void);
public:
    void Destroy(void);
    void Lock(void);
    void Unlock(void);
private:
    CRITICAL_SECTION m_cs;
};

class LGlobalLock : public ILock
{
public:
    LGlobalLock(__in PCSTR lpName);
    LGlobalLock(__in PCWSTR lpName);
    ~LGlobalLock(void);
public:
    void Destroy(void);
    void Lock(void);
    void Unlock(void);
private:
    HANDLE m_hMutex;
    DWORD m_tid;
    DWORD m_cnt;
};
