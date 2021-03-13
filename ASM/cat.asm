bits 64
section .bss
    fd resb 4
    buffer resb 0x1000
    size equ 0x1000

section .text
global _start
_start:
; int open(const char *pathname, int flags, mode_t mode);
;         arg0 (%rdi), arg1 (%rsi), arg2 (%rdx)

    mov rax, 0x2 ; open(
    mov rdi, [rsp + 0x10] ; argv[1]
    xor esi, esi          ; O_RDONLY
    mov rdx, 0444o        ; -r--r--r--
    syscall      ; )

    mov [fd], rax

; ssize_t read(int fd, void *buf, size_t count);
;         arg0 (%rdi), arg1 (%rsi), arg2 (%rdx)

    xor eax, eax ; read(
    mov rdi, [fd]    ; int fd
    mov rsi, buffer  ; void *buf
    mov rdx, size    ; size_t count
    syscall      ; )

; ssize_t write(int fd, const void *buf, size_t count);
;         arg0 (%rdi), arg1 (%rsi), arg2 (%rdx)

    mov rax, 0x1 ; write(
    mov rdi, 0x1    ; STDOUT
                    ; rsi points to buf
                    ; rdx is 0x1000
    syscall      ; )

; int close(int fd);
;         arg0 (%rdi)

    mov rax, 0x3 ; close(
    mov rdi, [fd]   ; int fd
    syscall      ; )

; void exit(int status);
;         arg0 (%rdi)

    mov rax, 60 ; exit(
    xor edi, edi  ; 0
    syscall     ; )

