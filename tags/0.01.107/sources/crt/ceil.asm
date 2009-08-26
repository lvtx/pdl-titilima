;-----------------------------------------------------------------------------
; ceil.asm - floating point ceiling
; Ported from Al Maromaty's free C Runtime Library
;-----------------------------------------------------------------------------
                .386
_TEXT           segment use32 para public 'CODE'
                public  _ceil
                
_ceil           proc    near
                assume  cs:_TEXT
                push    ebp
                mov     ebp,esp
                sub     esp,4                   ; Allocate temporary space
                fld     qword ptr [ebp+8]       ; Load real from stack
                fstcw   [ebp-2]                 ; Save control word
                fclex                           ; Clear exceptions
                mov     word ptr [ebp-4],0b63h  ; Rounding control word
                fldcw   [ebp-4]                 ; Set new rounding control
                frndint                         ; Round to integer
                fclex                           ; Clear exceptions
                fldcw   [ebp-2]                 ; Restore control word
                mov     esp,ebp                 ; Deallocate temporary space
                pop     ebp
                ret
_ceil           endp

_TEXT           ends
                end
