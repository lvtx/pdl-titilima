#define _FILE_DEFINED
struct _iobuf;
typedef struct _iobuf FILE;

#include <pdl_base.h>
#include <stdio.h>
#include "file.h"

FILE __iob[3];

FILE* __iob_func(void)
{
    return __iob;
}

void init_stdio(void)
{
    // stdin
    __iob[0].handle = GetStdHandle(STD_INPUT_HANDLE);
    __iob[0].buf = NULL;
    __iob[0].bufptr = 0;

    // stdout
    __iob[1].handle = GetStdHandle(STD_OUTPUT_HANDLE);
    __iob[1].buf = NULL;
    __iob[1].bufptr = 0;

    // stderr
    __iob[2].handle = GetStdHandle(STD_ERROR_HANDLE);
    __iob[2].buf = NULL;
    __iob[2].bufptr = 0;
}

int __cdecl remove(const char* path)
{
    if (DeleteFileA(path))
        return 0;
    else
        return -1;
}

int __cdecl rename(const char* oldname, const char* newname)
{
    if (MoveFileA(oldname, newname))
        return 0;
    else
        return -1;
}

int __cdecl sprintf(char* buffer, const char* format, ...)
{
    va_list arglist;
    int ret;

    PDLASSERT(NULL != buffer && NULL != format);
    va_start(arglist, format);
    ret = wvsprintfA(buffer, format, arglist);
    va_end(arglist);

    return ret;
}
