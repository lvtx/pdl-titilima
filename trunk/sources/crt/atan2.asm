;-----------------------------------------------------------------------------
; atan2.asm - floating point arc tangent (2 argument)
; Ported from Al Maromaty's free C Runtime Library
;-----------------------------------------------------------------------------
                .386
_TEXT           segment use32 para public 'CODE'
                public  _atan2
                public  __CIatan2
                
_atan2          proc    near
                assume  cs:_TEXT
                push    ebp
                mov     ebp,esp
                fld     qword ptr [ebp+8]       ; Load real from stack
                fld     qword ptr [ebp+16]      ; Load real from stack
                fpatan                          ; Take the arctangent
                mov     esp,ebp
                pop     ebp
                ret
_atan2          endp

__CIatan2       proc    near
                assume  cs:_TEXT
                push    ebp
                mov     ebp,esp
                fld     qword ptr [ebp+8]       ; Load real from stack
                fld     qword ptr [ebp+16]      ; Load real from stack
                fpatan                          ; Take the arctangent
                mov     esp,ebp
                pop     ebp
                ret
__CIatan2       endp

_TEXT           ends
                end
