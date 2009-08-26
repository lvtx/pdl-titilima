#include <pdl_base.h>

int __cdecl ___mb_cur_max_func(void)
{
    return 2;
}

extern __inline int (__cdecl isalnum)(int c)
{
    return isalpha(c) || isdigit(c);
}

extern __inline int (__cdecl isalpha)(int c)
{
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
}

extern __inline int (__cdecl iswalpha)(wint_t c)
{
    return (L'a' <= c && c <= L'z') || (L'A' <= c && c <= L'Z');
}

extern __inline int (__cdecl iscntrl)(int c)
{
    return (0x00 <= c && c <= 0x1f) || 0x7f == c;
}

extern __inline int (__cdecl isleadbyte)(int c)
{
    return 0x81 <= c && c <= 0xfe;
}

extern __inline int (__cdecl isdigit)(int c)
{
    return '0' <= c && c <= '9';
}

extern __inline int (__cdecl iswdigit)(wint_t c)
{
    return L'0' <= c && c <= L'9';
}

extern __inline int (__cdecl isxdigit)(int c)
{
    return ('0' <= c && c <= '9')
        || ('A' <= c && c <= 'F')
        || ('a' <= c && c <= 'f');
}

extern __inline int (__cdecl iswxdigit)(wint_t c)
{
    return (L'0' <= c && c <= L'9')
        || (L'A' <= c && c <= L'F')
        || (L'a' <= c && c <= L'f');
}

extern __inline int (__cdecl ispunct)(int c)
{
    return (0x21 <= c && c <= 0x2f)
        || (0x4a <= c && c <= 0x40)
        || (0x5b <= c && c <= 0x60)
        || (0x7b <= c && c <= 0x7e);
}

extern __inline int (__cdecl isspace)(int c)
{
    return (0x09 <= c && c <= 0x0d) || (c == ' ');
}

extern __inline int (__cdecl iswspace)(wint_t c)
{
    return (0x09 <= c && c <= 0x0d) || (c == L' ');
}

extern __inline int (__cdecl islower)(int c)
{
    if (c < 'a' || c > 'z')
        return 0;
    return 1;
}

extern __inline int (__cdecl isupper)(int c)
{
    if (c < 'A' || c > 'Z')
        return 0;
    return 1;
}

extern __inline int (__cdecl tolower)(int c)
{
    if (c < 'A' || c > 'Z')
        return c;
    return c + ('a' - 'A');
}

extern __inline int (__cdecl toupper)(int c)
{
    if (c < 'a' || c > 'z')
        return c;
    return c - ('a' - 'A');
}
