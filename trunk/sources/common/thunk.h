#pragma once

class LThunk
{
#pragma pack(push, 1)
    struct WndThunk
    {
#ifdef _WIN32_WCE
        DWORD	m_mov_r0;		// mov	r0, pThis
        DWORD	m_mov_pc;		// mov	pc, pFunc
        DWORD	m_pThis;
        DWORD	m_pFunc;
#else
        DWORD       m_mov;     // mov dword ptr [esp+0x4], pThis (esp+0x4 is hWnd)
        DWORD_PTR   m_this;
        BYTE        m_jmp;     // jmp WndProc
        DWORD_PTR   m_relproc; // relative jmp
#endif // _WIN32_WCE
    };
#pragma pack(pop)

public:
    void Init(PVOID proc, PVOID pThis)
    {
#ifdef _WIN32_WCE
        m_thunk.m_mov_r0 = 0xe59f0000;
        m_thunk.m_mov_pc = 0xe59ff000;
        m_thunk.m_pThis = (DWORD)pThis;
        m_thunk.m_pFunc = (DWORD)proc;
#else
        // mov dword ptr [esp + 4], pThis
        m_thunk.m_mov = 0x042444c7;
        m_thunk.m_this = reinterpret_cast<DWORD_PTR>(pThis);
        // jmp proc
        m_thunk.m_jmp = 0xe9;
        m_thunk.m_relproc = (INT_PTR)proc - ((INT_PTR)this
            + sizeof(WndThunk));
#endif // _WIN32_WCE

        FlushInstructionCache(GetCurrentProcess(), &m_thunk,
            sizeof(WndThunk));
    }
    WndThunk& GetThunk(void)
    {
        return m_thunk;
    }
private:
    WndThunk m_thunk;
};
