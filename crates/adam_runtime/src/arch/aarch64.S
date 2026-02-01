// Adam Runtime — aarch64 context switch
//
// void adam_context_switch(CpuContext *old, CpuContext *new)
//
// Saves callee-saved registers into `old`, restores from `new`, then returns.
// When this function "returns" after restoring `new`, execution continues
// wherever `new` was previously suspended (or at its initial LR).
//
// CpuContext layout (offsets in bytes):
//   0:  sp
//   8:  x19
//  16:  x20
//  24:  x21
//  32:  x22
//  40:  x23
//  48:  x24
//  56:  x25
//  64:  x26
//  72:  x27
//  80:  x28
//  88:  x29 (FP)
//  96:  x30 (LR)
// 104:  d8
// 112:  d9
// 120:  d10
// 128:  d11
// 136:  d12
// 144:  d13
// 152:  d14
// 160:  d15

.globl _adam_context_switch
.p2align 2

_adam_context_switch:
    // x0 = pointer to old CpuContext (save current state here)
    // x1 = pointer to new CpuContext (restore state from here)

    // ---- Save current context into *old (x0) ----
    mov     x2, sp
    str     x2,  [x0, #0]      // sp
    stp     x19, x20, [x0, #8]
    stp     x21, x22, [x0, #24]
    stp     x23, x24, [x0, #40]
    stp     x25, x26, [x0, #56]
    stp     x27, x28, [x0, #72]
    stp     x29, x30, [x0, #88]    // FP, LR
    stp     d8,  d9,  [x0, #104]
    stp     d10, d11, [x0, #120]
    stp     d12, d13, [x0, #136]
    stp     d14, d15, [x0, #152]

    // ---- Restore new context from *new (x1) ----
    ldr     x2,  [x1, #0]      // sp
    mov     sp, x2
    ldp     x19, x20, [x1, #8]
    ldp     x21, x22, [x1, #24]
    ldp     x23, x24, [x1, #40]
    ldp     x25, x26, [x1, #56]
    ldp     x27, x28, [x1, #72]
    ldp     x29, x30, [x1, #88]    // FP, LR
    ldp     d8,  d9,  [x1, #104]
    ldp     d10, d11, [x1, #120]
    ldp     d12, d13, [x1, #136]
    ldp     d14, d15, [x1, #152]

    // Return — this jumps to the restored x30 (LR).
    ret
