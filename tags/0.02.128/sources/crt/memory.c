#include <pdl_base.h>

void __cdecl free(void* ptr)
{
    HeapFree(GetProcessHeap(), 0, ptr);
}

void* __cdecl malloc(size_t size)
{
    return HeapAlloc(GetProcessHeap(), 0, size);
}

void* __cdecl memchr(const void* buf, int c, size_t count)
{
    PBYTE p = (PBYTE)buf;
    while (count > 0 && (*p != (BYTE)c))
    {
        ++p;
        --count;
    }

    return count > 0 ? (void *)p : NULL;
}

int __cdecl memcmp(const void* buf1, const void* buf2, size_t count)
{
    if (0 == count)
        return 0;
    while (--count && *(char*)buf1 == *(char*)buf2)
    {
        buf1 = (char *)buf1 + 1;
        buf2 = (char *)buf2 + 1;
    }
    return *((unsigned char *)buf1) - *((unsigned char *)buf2);
}

void* __cdecl memcpy(void * dst, const void* src, size_t count)
{
    void* ret = dst;
    while (count--)
    {
        *(char *)dst = *(char *)src;
        dst = (char *)dst + 1;
        src = (char *)src + 1;
    }
    return ret;
}

void* __cdecl memmove(void * dst, const void * src, size_t count)
{
    void* ret = dst;
    if (dst <= src || (char *)dst >= ((char *)src + count))
    {
        // 非重叠缓冲区
        while (count--)
        {
            *(char *)dst = *(char *)src;
            dst = (char *)dst + 1;
            src = (char *)src + 1;
        }
    }
    else
    {
        // 重叠缓冲区
        dst = (char *)dst + count - 1;
        src = (char *)src + count - 1;

        while (count--)
        {
            *(char *)dst = *(char *)src;
            dst = (char *)dst - 1;
            src = (char *)src - 1;
        }
    }
    return ret;
}

void* __cdecl memset(void *dst, int val, size_t count)
{
    void *start = dst;
    while (count--)
    {
        *(char *)dst = (char)val;
        dst = (char *)dst + 1;
    }
    return start;
}

void* __cdecl realloc(void* memblock, size_t size)
{
    if (NULL == memblock)
        return malloc(size);

    if (0 == size)
    {
        free(memblock);
        return NULL;
    }

    return HeapReAlloc(GetProcessHeap(), 0, memblock, size);
}
