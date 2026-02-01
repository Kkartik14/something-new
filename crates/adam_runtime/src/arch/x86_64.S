// Adam Runtime — x86_64 context switch
//
// void adam_context_switch(CpuContext *old, CpuContext *new)
//
// System V AMD64 ABI: old in %rdi, new in %rsi
//
// CpuContext layout (offsets in bytes):
//   0:  rsp
//   8:  rbx
//  16:  rbp
//  24:  r12
//  32:  r13
//  40:  r14
//  48:  r15

.globl _adam_context_switch
.p2align 4

_adam_context_switch:
    // %rdi = pointer to old CpuContext
    // %rsi = pointer to new CpuContext

    // ---- Save current context into *old (%rdi) ----
    movq    %rsp, 0(%rdi)
    movq    %rbx, 8(%rdi)
    movq    %rbp, 16(%rdi)
    movq    %r12, 24(%rdi)
    movq    %r13, 32(%rdi)
    movq    %r14, 40(%rdi)
    movq    %r15, 48(%rdi)

    // ---- Restore new context from *new (%rsi) ----
    movq    0(%rsi), %rsp
    movq    8(%rsi), %rbx
    movq    16(%rsi), %rbp
    movq    24(%rsi), %r12
    movq    32(%rsi), %r13
    movq    40(%rsi), %r14
    movq    48(%rsi), %r15

    // Return — pops return address from stack and jumps to it.
    ret
