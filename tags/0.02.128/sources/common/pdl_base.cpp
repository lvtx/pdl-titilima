#include <pdl_base.h>

#pragma pack(push, 1)
typedef struct _tagThisThunk {
    BYTE PopEax;  // 0x58
    BYTE Push;    // 0x68
    PVOID This;
    BYTE PushEax; // 0x50
    BYTE Jmp;     // 0xe9
    PBYTE Addr;
} THISTHUNK;
#pragma pack(pop)

int PDLAPI LAssertBox(__in PCWSTR expr, __in PCWSTR srcfile, __in int nLine)
{
    WCHAR app[MAX_PATH];
    ::GetModuleFileNameW(NULL, app, MAX_PATH);
    PCWSTR p = wcsrchr(app, L'\\') + 1;

    int n = 128 + lstrlenW(p) + lstrlenW(expr) + lstrlenW(srcfile);
    PWSTR text = new WCHAR[n];
    wsprintfW(text, L"���ĳ��� %s �������ԡ�\n\n"
        L"���ʽ��%s\n"
        L"Դ�ļ���%s\n"
        L"�����У�%d\n\n"
        L"�����Ե������ֹ������ֹ�������У������ԡ��жϵ���������"
        L"�����ԡ��������г���",
        p, expr, srcfile, nLine);
    n = ::MessageBoxW(NULL, text, L"PDL ����",
        MB_ABORTRETRYIGNORE | MB_DEFBUTTON2 | MB_SYSTEMMODAL \
        | MB_ICONEXCLAMATION);
    delete [] text;

    if (IDABORT == n)
        ExitProcess(0);
    else if (IDRETRY == n)
        DebugBreak();
    return 0;
}

PTHISTHUNK PDLAPI LCreateThisThunk(PVOID This, PVOID pfn)
{
    PTHISTHUNK thunk = new THISTHUNK;
    if (NULL != thunk)
    {
        // pop eax
        thunk->PopEax = 0x58;
        // push this
        thunk->Push = 0x68;
        thunk->This = This;
        // push eax
        thunk->PushEax = 0x50;
        // jmp addr
        thunk->Jmp = 0xe9;
        thunk->Addr = (PBYTE)((int)pfn - (int)thunk - sizeof(THISTHUNK));
        ::FlushInstructionCache(::GetCurrentProcess(), thunk,
            sizeof(THISTHUNK));
    }
    return thunk;
}

void PDLAPI LDestroyThisThunk(PTHISTHUNK thunk)
{
    delete thunk;
}

void PDLAPI LTrace(__in PCSTR lpszFormat, ...)
{
    char str[1024];
    va_list arglist;
    va_start(arglist, lpszFormat);
    int cnt = wvsprintfA(str, lpszFormat, arglist);
    va_end(arglist);

    OutputDebugStringA(str);
}

void PDLAPI LTrace(__in PCWSTR lpszFormat, ...)
{
    WCHAR str[1024];
    va_list arglist;
    va_start(arglist, lpszFormat);
    int cnt = wvsprintfW(str, lpszFormat, arglist);
    va_end(arglist);

    OutputDebugStringW(str);
}
