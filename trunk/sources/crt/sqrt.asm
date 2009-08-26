;-----------------------------------------------------------------------------
; sqrt.asm - floating point square root
; Ported from Al Maromaty's free C Runtime Library
;-----------------------------------------------------------------------------
                .386
_TEXT           segment use32 para public 'CODE'
                public  _sqrt
                public  __CIsqrt

_sqrt           proc    near
                assume  cs:_TEXT
                push    ebp
                mov     ebp,esp
                fld     qword ptr [ebp+8]       ; Load real from stack
                fsqrt                           ; Take the square root
                pop     ebp
                ret
_sqrt           endp

__CIsqrt        proc    near
                assume  cs:_TEXT
                push    ebp
                mov     ebp,esp
                fld     qword ptr [ebp+8]       ; Load real from stack
                fsqrt                           ; Take the square root
                pop     ebp
                ret
__CIsqrt        endp

_TEXT           ends
                end
