#include "..\..\include\pdl_base.h"
#include "..\..\include\pdl_parser.h"
#include <stdlib.h>

int PDLAPI x2i(__in int x)
{
    int ret = -1;
    switch (x)
    {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        ret = x - '0';
        break;
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
        ret = x - 'a' + 10;
        break;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
        ret = x - 'A' + 10;
        break;
    }
    return ret;
}

COLORREF PDLAPI LParseColorString(
    __in PCSTR lpszString,
    __in COLORREF clrDefault)
{
    PDLASSERT(NULL != lpszString);

    int clr[3] = { -1, -1, -1 }; // R, G, B
    int i;

    if ('#' == *lpszString)
    {
        int ch1, ch2;
        for (i = 0; i < 3; ++i)
        {
            ch1 = x2i(lpszString[i * 2 + 1]);
            if (-1 == ch1)
                return clrDefault;
            ch2 = x2i(lpszString[i * 2 + 2]);
            if (-1 == ch2)
                return clrDefault;

            clr[i] = ch1 * 0x10 + ch2;
        }
    }
    else
    {
        const char* p = lpszString;
        const char* q = strchr(lpszString, ',');
        if (NULL == q)
            return clrDefault;
        clr[0] = atoi(p);

        p = q + 1;
        q = strchr(p, ',');
        if (NULL == q)
            return clrDefault;
        clr[1] = atoi(p);

        p = q + 1;
        clr[2] = atoi(p);
    }

    for (i = 0; i < 3; ++i)
    {
        if (0 > clr[i] || clr[i] > 255)
            return clrDefault;
    }

    return RGB(clr[0], clr[1], clr[2]);
}

COLORREF PDLAPI LParseColorString(
    __in PCWSTR lpszString,
    __in COLORREF clrDefault)
{
    PDLASSERT(NULL != lpszString);

    int clr[3] = { -1, -1, -1 }; // R, G, B
    int i;

    if (L'#' == *lpszString)
    {
        int ch1, ch2;
        for (i = 0; i < 3; ++i)
        {
            ch1 = x2i(lpszString[i * 2 + 1]);
            if (-1 == ch1)
                return clrDefault;
            ch2 = x2i(lpszString[i * 2 + 2]);
            if (-1 == ch2)
                return clrDefault;

            clr[i] = ch1 * 0x10 + ch2;
        }
    }
    else
    {
        const wchar_t* p = lpszString;
        const wchar_t* q = wcschr(lpszString, L',');
        if (NULL == q)
            return clrDefault;
        clr[0] = _wtoi(p);

        p = q + 1;
        q = wcschr(p, L',');
        if (NULL == q)
            return clrDefault;
        clr[1] = _wtoi(p);

        p = q + 1;
        clr[2] = _wtoi(p);
    }

    for (i = 0; i < 3; ++i)
    {
        if (0 > clr[i] || clr[i] > 255)
            return clrDefault;
    }

    return RGB(clr[0], clr[1], clr[2]);
}
