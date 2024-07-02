    global x86_64_syscall
    section .text

x86_64_syscall:
    push rax
    mov r10, rcx
    mov rax, r9 ; the last parameter is used as the syscall number instead of an actual parameter
    syscall
    pop rax
    ret

; system-v user application: rdi, rsi, rdx, rcx, r8, r9
; system-v kern application: rdi, rsi, rdx, r10, r8, r9
