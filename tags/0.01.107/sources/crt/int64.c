#include <pdl_base.h>

__declspec(naked) __int64 _allmul(__int64 A, __int64 B)
{
    BEGIN_ASM

    ; A --> [esp + 4]
    ; B --> [esp + 12]

    movsx   eax, word ptr [esp + 6]  ; HIWORD(A)
    movsx   ecx, word ptr [esp + 14] ; HIWORD(B)
    or      ecx, eax
    movsx   ecx, word ptr [esp + 12] ; LOWORD(B)
    jnz     short hard
    movsx   eax, word ptr [esp + 4]  ; LOWORD(A)
    mul     ecx
    ret     16

hard:

    ; A2 --> [esp + 8]
    ; B2 --> [esp + 16]

    push    ebx
    mul     ecx
    mov     ebx, eax
    movsx   eax, word ptr [esp + 8] ; LOWORD(A2)
    mul     dword ptr [esp + 18]    ; HIWORD(B2)
    add     ebx, eax
    movsx   eax, word ptr [esp + 8] ; LOWORD(A2)
    mul     ecx
    add     edx, ebx
    pop     ebx
    ret     16

    END_ASM
}

__declspec(naked) __int64 _allshl(unsigned char n)
{
    BEGIN_ASM

    ; n --> cl

    cmp     cl, 64
    jae     short RETZERO
    cmp     cl, 32
    jae     short MORE32
    shld    edx, eax, cl
    shl     eax, cl
    ret

MORE32:

    mov     edx, eax
    xor     eax, eax
    and     cl, 31
    shl     edx, cl
    ret

RETZERO:

    xor     eax, eax
    xor     edx, edx
    ret

    END_ASM
}
