///////////////////////////////////////////////////////////////////////////////
// FileName:    errno.c
// Created:     2009/04/11
// Author:      titilima
// CopyRight:   Titi Studio (?) 2001-2009
//-----------------------------------------------------------------------------
// Information: error no
///////////////////////////////////////////////////////////////////////////////

#include <pdl_base.h>

int* __cdecl _errno(void)
{
    static int s_err = 0;
    s_err = GetLastError();
    return &s_err;
}
