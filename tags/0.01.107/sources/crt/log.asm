;-----------------------------------------------------------------------------
; log.asm - floating point logarithm
; Ported from Al Maromaty's free C Runtime Library
;-----------------------------------------------------------------------------
                .386
_TEXT           segment use32 para public 'CODE'
                public  _log
                public  __CIlog

_log            proc    near
                assume  cs:_TEXT
                push    ebp
                mov     ebp,esp
                fld     qword ptr [ebp+8]       ; Load real from stack
                fldln2                          ; Load log base e of 2
                fxch    st(1)                   ; Exchange st, st(1)
                fyl2x                           ; Compute the natural log(x)
                pop     ebp
                ret
_log            endp

__CIlog         proc    near
                assume  cs:_TEXT
                push    ebp
                mov     ebp,esp
                fld     qword ptr [ebp+8]       ; Load real from stack
                fldln2                          ; Load log base e of 2
                fxch    st(1)                   ; Exchange st, st(1)
                fyl2x                           ; Compute the natural log(x)
                pop     ebp
                ret
__CIlog         endp

_TEXT           ends
                end
