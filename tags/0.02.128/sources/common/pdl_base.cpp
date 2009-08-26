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
    wsprintfW(text, L"您的程序 %s 发生断言。\n\n"
        L"表达式：%s\n"
        L"源文件：%s\n"
        L"代码行：%d\n\n"
        L"您可以点击“终止”以终止程序运行，“重试”中断到调试器，"
        L"“忽略”继续运行程序。",
        p, expr, srcfile, nLine);
    n = ::MessageBoxW(NULL, text, L"PDL 断言",
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
