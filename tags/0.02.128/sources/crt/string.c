#include <pdl_base.h>

char* __cdecl strcat(char* dst, const char* src)
{
    char* cp = dst;
    while (*cp)
        cp++;
    while (*cp++ = *src++)
        ;
    return dst;
}

wchar_t* __cdecl wcscat(wchar_t* dst, const wchar_t* src)
{
    wchar_t* cp = dst;
    while (*cp)
        cp++;
    while (*cp++ = *src++)
        ;
    return dst;
}

char* __cdecl strchr (const char * string, int ch)
{
    while (*string && *string != (char)ch)
        string++;

    if (*string == (char)ch)
        return (char *)string;
    return NULL;
}

wchar_t* __cdecl wcschr(const wchar_t* string, wchar_t ch)
{
    while (*string && *string != (wchar_t)ch)
        string++;

    if (*string == (wchar_t)ch)
        return (wchar_t *)string;
    return NULL;
}

int __cdecl strcmp(const char* string1, const char* string2)
{
    int ret = 0;

    while (0 == (ret = *(unsigned char*)string1 - *(unsigned char *)string2)
        && '\0' != *string2)
    {
        ++string1;
        ++string2;
    }

    if (ret < 0)
        ret = -1;
    else if (ret > 0)
        ret = 1;

    return ret;
}

int __cdecl strcoll(const char* string1, const char* string2)
{
    return strcmp(string1, string2);
}

size_t __cdecl strcspn(const char* string, const char* strCharSet)
{
    const unsigned char *str = string;
    const unsigned char *ctrl = strCharSet;

    unsigned char map[32];
    int count;

    /* Clear out bit map */
    for (count=0; count<32; count++)
        map[count] = 0;

    /* Set bits in control map */
    while (0 != *ctrl)
    {
        map[*ctrl >> 3] |= (1 << (*ctrl & 7));
        ctrl++;
    }

    /* 1st char in control map stops search */
    count = 0;
    map[0] |= 1;    /* null chars not considered */
    while (!(map[*str >> 3] & (1 << (*str & 7))))
    {
        ++count;
        ++str;
    }
    return count;
}

char* __cdecl strcpy(char* dst, const char* src)
{
    char* cp = dst;
    while (*cp++ = *src++)
        ;
    return dst;
}

wchar_t* __cdecl wcscpy(wchar_t* dst, const wchar_t* src)
{
    wchar_t* cp = dst;
    while (*cp++ = *src++)
        ;
    return dst;
}

char* __cdecl strncpy(char* strDest, const char* strSource, size_t count)
{
    char* start = strDest;

    while (0 != count && (*strDest++ = *strSource++))    /* copy string */
        --count;

    if (0 != count)                              /* pad out with zeroes */
    {
        while (--count)
            *strDest++ = '\0';
    }

    return start;
}

wchar_t* __cdecl wcsncpy(
    wchar_t* strDest,
    const wchar_t* strSource,
    size_t count)
{
    wchar_t* start = strDest;

    while (0 != count && (*strDest++ = *strSource++))    /* copy string */
        --count;

    if (0 != count)                              /* pad out with zeroes */
    {
        while (--count)
            *strDest++ = L'\0';
    }

    return start;
}

char* __cdecl strrchr(const char *string, int ch)
{
    char *start = (char*)string;
    while (*string++)
        ;
    while (--string != start && *string != (char)ch)
        ;
    if (*string == (char)ch)
        return (char*)string;

    return NULL;
}

unsigned char* __cdecl _mbsrchr(const char* string, int ch)
{
    unsigned char *start = (unsigned char*)string;
    while (*string++)
        ;
    while (--string != start && *string != (unsigned char)ch)
        ;
    if (*string == (unsigned char)ch)
        return (unsigned char*)string;

    return NULL;
}

wchar_t* __cdecl wcsrchr(const wchar_t * string, wchar_t ch)
{
    wchar_t *start = (wchar_t*)string;
    while (*string++)
        ;
    while (--string != start && *string != (wchar_t)ch)
        ;
    if (*string == (wchar_t)ch)
        return (wchar_t*)string;

    return NULL;
}

char* __cdecl strerror(int errnum)
{
    return "Unknown error.";
}

size_t __cdecl strlen(const char* str)
{
    const char* eos = str;
    while (*eos++)
        ;
    return eos - str - 1;
}

size_t __cdecl wcslen(const wchar_t* wcs)
{
    const wchar_t* eos = wcs;
    while (*eos++)
        ;
    return (size_t)(eos - wcs - 1);
}

char* __cdecl strncat(char* strDest, const char *strSource, size_t count)
{
    char* start = strDest;

    while (*strDest++)
        ;
    strDest--;

    while (count--)
    {
        if (!(*strDest++ = *strSource++))
            return start;
    }

    *strDest = '\0';
    return start;
}

wchar_t* __cdecl wcsncat(
    wchar_t* strDest,
    const wchar_t* strSource,
    size_t count)
{
    wchar_t* start = strDest;

    while (*strDest++)
        ;
    strDest--;

    while (count--)
    {
        if (!(*strDest++ = *strSource++))
            return start;
    }

    *strDest = L'\0';
    return start;
}


char* __cdecl strstr(const char* str, const char* strSearch)
{
    char *cp = (char *)str;
    char *s1, *s2;

    if ('\0' == *strSearch)
        return (char *)str;

    while ('\0' != *cp)
    {
        s1 = cp;
        s2 = (char *)strSearch;

        while ('\0' != *s1 && '\0' != *s2 && *s1 == *s2)
        {
            ++s1;
            ++s2;
        }

        if ('\0' == *s2)
            return cp;

        ++cp;
    }

    return NULL;
}

wchar_t* __cdecl wcsstr(const wchar_t* str, const wchar_t* strSearch)
{
    wchar_t *cp = (wchar_t *)str;
    wchar_t *s1, *s2;

    if (L'\0' == *strSearch)
        return (wchar_t *)str;

    while (L'\0' != *cp)
    {
        s1 = cp;
        s2 = (wchar_t *) strSearch;

        while (L'\0' != *s1 && L'\0' != *s2 && *s1 == *s2)
        {
            ++s1;
            ++s2;
        }

        if (L'\0' == *s2)
            return cp;

        cp++;
    }

    return NULL;
}
