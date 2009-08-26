// 之所以要加入 _WCTYPE_INLINE_DEFINED 宏，
// 是为了使 iswspace 和 iswdigit 不被 iswctype 代替
#define _WCTYPE_INLINE_DEFINED
#include <pdl_base.h>
#include <errno.h>
#include <float.h>
#include <math.h>

unsigned int randseed = 0;

long __cdecl atol(const char* string)
{
    int cur, sig;
    long ret;
    while (isspace(*string))
        ++string;

    cur = *string++;
    sig = cur;

    if ('-' == cur || '+' == cur)
        cur = *string++;

    ret = 0;
    while (isdigit(cur))
    {
        ret = 10 * ret + (cur - '0');
        cur = *string++;
    }

    if ('-' == sig)
        return -ret;
    else
        return ret;
}

int __cdecl atoi(const char* string)
{
    return (int)atol(string);
}

long __cdecl _wtol(const wchar_t* string)
{
    int cur, sig;
    long ret;
    while (iswspace(*string))
        ++string;

    cur = *string++;
    sig = cur;

    if (L'-' == cur || L'+' == cur)
        cur = *string++;

    ret = 0;
    while (iswdigit(cur))
    {
        ret = 10 * ret + (cur - L'0');
        cur = *string++;
    }

    if (L'-' == sig)
        return -ret;
    else
        return ret;
}

int __cdecl _wtoi(const wchar_t* string)
{
    return (int)_wtol(string);
}

void __cdecl exit(int status)
{
    ExitProcess(status);
}

int __cdecl mbtowc(wchar_t* wchar, const char* mbchar, size_t count)
{
    if (NULL == mbchar || '\0' == *mbchar)
        return 0;

    MultiByteToWideChar(CP_ACP, 0, mbchar, -1, wchar, (int)count);
    return sizeof(char);
}

int __cdecl rand(void)
{
    randseed = randseed * 214013L + 2531011L;
    return (randseed >> 16) & 0x7fff;
}

void __cdecl srand(unsigned int seed)
{
    randseed = seed;
}

double strtod(const char *str, char **endptr)
{
    double number;
    int exponent;
    int negative;
    char *p = (char *) str;
    double p10;
    int n;
    int num_digits;
    int num_decimals;

    // Skip leading whitespace
    while (isspace(*p)) p++;

    // Handle optional sign
    negative = 0;
    switch (*p) 
    {		
    case '-': negative = 1; // Fall through to increment position
    case '+': p++;
    }

    number = 0.;
    exponent = 0;
    num_digits = 0;
    num_decimals = 0;

    // Process string of digits
    while (isdigit(*p))
    {
        number = number * 10. + (*p - '0');
        p++;
        num_digits++;
    }

    // Process decimal part
    if (*p == '.') 
    {
        p++;

        while (isdigit(*p))
        {
            number = number * 10. + (*p - '0');
            p++;
            num_digits++;
            num_decimals++;
        }

        exponent -= num_decimals;
    }

    if (num_digits == 0)
    {
        errno = ERANGE;
        return 0.0;
    }

    // Correct for sign
    if (negative) number = -number;

    // Process an exponent string
    if (*p == 'e' || *p == 'E') 
    {
        // Handle optional sign
        negative = 0;
        switch (*++p) 
        {	
        case '-': negative = 1;	// Fall through to increment pos
        case '+': p++;
        }

        // Process string of digits
        n = 0;
        while (isdigit(*p)) 
        {	
            n = n * 10 + (*p - '0');
            p++;
        }

        if (negative) 
            exponent -= n;
        else
            exponent += n;
    }

    if (exponent < DBL_MIN_EXP  || exponent > DBL_MAX_EXP)
    {
        errno = ERANGE;
        return HUGE_VAL;
    }

    // Scale the result
    p10 = 10.;
    n = exponent;
    if (n < 0) n = -n;
    while (n) 
    {
        if (n & 1) 
        {
            if (exponent < 0)
                number /= p10;
            else
                number *= p10;
        }
        n >>= 1;
        p10 *= p10;
    }

    if (number == HUGE_VAL) errno = ERANGE;
    if (endptr) *endptr = p;

    return number;
}

unsigned long __cdecl strtoul(const char* nptr, char** endptr, int base)
{
    // TODO:
    return 0;
}
