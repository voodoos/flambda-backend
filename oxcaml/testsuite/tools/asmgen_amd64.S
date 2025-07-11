/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2000 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifdef SYS_macosx
#define ALIGN 4
#define WEAK .weak_definition
#else
#define ALIGN 16
#define WEAK .weak
#endif

#ifdef SYS_macosx
#define CALL_GEN_CODE _call_gen_code
#define CAML_C_CALL _caml_c_call
#define CAML_NEGF_MASK _caml_negf_mask
#define CAML_ABSF_MASK _caml_absf_mask
#define CAML_CALL_GC _caml_call_gc
#define CAML_CALL_GC_AVX _caml_call_gc_avx
#define CAML_CALL_GC_AVX512 _caml_call_gc_avx512
#else
#define CALL_GEN_CODE call_gen_code
#define CAML_C_CALL caml_c_call
#define CAML_NEGF_MASK caml_negf_mask
#define CAML_ABSF_MASK caml_absf_mask
#define CAML_CALL_GC caml_call_gc
#define CAML_CALL_GC_AVX caml_call_gc_avx
#define CAML_CALL_GC_AVX512 caml_call_gc_avx512
#endif

        .globl CAML_CALL_GC
        WEAK CAML_CALL_GC
CAML_CALL_GC:
        int3
        .globl CAML_CALL_GC_AVX
        WEAK CAML_CALL_GC_AVX
CAML_CALL_GC_AVX:
        int3
        .globl CAML_CALL_GC_AVX512
        WEAK CAML_CALL_GC_AVX512
CAML_CALL_GC_AVX512:
        int3

        .globl  CALL_GEN_CODE
        .align  ALIGN
CALL_GEN_CODE:
        pushq   %rbx
        pushq   %rbp
        pushq   %r12
        pushq   %r13
        pushq   %r14
        pushq   %r15
        movq    %rdi, %r10
        movq    %rsi, %rax
        movq    %rdx, %rbx
        movq    %rcx, %rdi
        movq    %r8, %rsi
        call    *%r10
        popq    %r15
        popq    %r14
        popq    %r13
        popq    %r12
        popq    %rbp
        popq    %rbx
        ret

        .globl  CAML_C_CALL
        .align  ALIGN
CAML_C_CALL:
        jmp     *%rax

#ifdef SYS_macosx
        .literal16
#elif defined(SYS_mingw64) || defined(SYS_cygwin)
        .section        .rodata.cst8
#else
        .section        .rodata.cst8,"aM",@progbits,8
#endif
        .globl  CAML_NEGF_MASK
        .align  ALIGN
CAML_NEGF_MASK:
        .quad   0x8000000000000000, 0
        .globl  CAML_ABSF_MASK
        .align  ALIGN
CAML_ABSF_MASK:
        .quad   0x7FFFFFFFFFFFFFFF, 0

        .comm   young_limit, 8

#if defined(SYS_linux)
    /* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
#endif
