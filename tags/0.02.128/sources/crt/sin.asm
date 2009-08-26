;-----------------------------------------------------------------------------
; sin.asm - floating point sine
;-----------------------------------------------------------------------------
                .386
_TEXT           segment use32 para public 'CODE'
                public  _sin
                public  __CIsin

_sin            proc    near
                assume  cs:_TEXT
                push    ebp                     ; Save register bp
                mov     ebp,esp                 ; Point to the stack frame
                fld     qword ptr [ebp+8]       ; Load real from stack
                fsin                            ; Take the sine
                pop     ebp                     ; Restore register bp
                ret                             ; Return to caller
_sin            endp

__CIsin         proc    near
                assume  cs:_TEXT
                push    ebp                     ; Save register bp
                mov     ebp,esp                 ; Point to the stack frame
                fld     qword ptr [ebp+8]       ; Load real from stack
                fsin                            ; Take the sine
                pop     ebp                     ; Restore register bp
                ret                             ; Return to caller
__CIsin        endp

_TEXT           ends
                end
