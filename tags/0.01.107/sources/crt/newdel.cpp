#include <pdl_base.h>

void* operator new(size_t _Size)
{
    return HeapAlloc(GetProcessHeap(), 0, _Size);
}

void operator delete(void* ptr)
{
    HeapFree(GetProcessHeap(), 0, ptr);
}

void* operator new[](size_t _Size)
{
    return HeapAlloc(GetProcessHeap(), 0, _Size);
}

void operator delete [](void* ptr)
{
    HeapFree(GetProcessHeap(), 0, ptr);
}
