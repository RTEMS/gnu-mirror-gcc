/* Copyright (C) 2014-2024 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "asmcfi.h"

#define BTI_C	hint	34
#define PACIASP	hint	25
#define AUTIASP	hint	29
#define PACIBSP	hint	27
#define AUTIBSP	hint	31
#define CHKFEAT_X16	hint	40
#define MRS_GCSPR(x)	mrs	x, s3_3_c2_c5_1
#define GCSPOPM(x)	sysl	x, #3, c7, c7, #1
#define GCSSS1(x)	sys	#3, c7, c7, #2, x
#define GCSSS2(x)	sysl	x, #3, c7, c7, #3

#define L(name) .L##name

#if defined(HAVE_AS_CFI_PSEUDO_OP) && defined(__GCC_HAVE_DWARF2_CFI_ASM)
# define cfi_window_save .cfi_window_save
# define cfi_b_key_frame .cfi_b_key_frame
#else
# define cfi_window_save
# define cfi_b_key_frame
#endif

#if __ARM_FEATURE_PAC_DEFAULT & 1
# define CFI_PAC_TOGGLE	cfi_window_save
# define CFI_PAC_KEY
# define PAC_AND_BTI	PACIASP
# define AUT	AUTIASP
#elif __ARM_FEATURE_PAC_DEFAULT & 2
# define CFI_PAC_TOGGLE	cfi_window_save
# define CFI_PAC_KEY	cfi_b_key_frame
# define PAC_AND_BTI	PACIBSP
# define AUT	AUTIBSP
#else
# define CFI_PAC_TOGGLE
# define CFI_PAC_KEY
# define PAC_AND_BTI	BTI_C
# define AUT
#endif

	.text
	.align	2
	.global	_ITM_beginTransaction
	.type	_ITM_beginTransaction, %function

_ITM_beginTransaction:
	cfi_startproc
	CFI_PAC_KEY
	PAC_AND_BTI
	CFI_PAC_TOGGLE
	mov	x1, sp
	stp	x29, x30, [sp, -11*16]!
	cfi_adjust_cfa_offset(11*16)
	cfi_rel_offset(x29, 0)
	cfi_rel_offset(x30, 8)
	mov	x29, sp
	stp	x19, x20, [sp, 1*16]
	stp	x21, x22, [sp, 2*16]
	stp	x23, x24, [sp, 3*16]
	stp	x25, x26, [sp, 4*16]
	stp	x27, x28, [sp, 5*16]
	stp	d8, d9,   [sp, 6*16]
	stp	d10, d11, [sp, 7*16]
	stp	d12, d13, [sp, 8*16]
	stp	d14, d15, [sp, 9*16]

	/* GCS support.  */
	mov	x2, 0
	mov	x16, 1
	CHKFEAT_X16
	tbnz	x16, 0, L(gcs_done_sj)
	MRS_GCSPR (x2)
	add	x2, x2, 8 /* GCS after _ITM_beginTransaction returns.  */
L(gcs_done_sj):
	stp	x2, x1, [sp, 10*16]

	/* Invoke GTM_begin_transaction with the struct we just built.  */
	mov	x1, sp
	bl	GTM_begin_transaction

	/* Return; we don't need to restore any of the call-saved regs.  */
	ldp	x29, x30, [sp], 11*16
	cfi_adjust_cfa_offset(-11*16)
	cfi_restore(x29)
	cfi_restore(x30)
	AUT
	CFI_PAC_TOGGLE
	ret
	cfi_endproc
	.size	_ITM_beginTransaction, . - _ITM_beginTransaction

	.align	2
	.global	GTM_longjmp
	.hidden	GTM_longjmp
	.type	GTM_longjmp, %function

GTM_longjmp:
	/* The first parameter becomes the return value (x0).
	   The third parameter is ignored for now.  */
	cfi_startproc
	CFI_PAC_KEY
	BTI_C
	ldp	x19, x20, [x1, 1*16]
	ldp	x21, x22, [x1, 2*16]
	ldp	x23, x24, [x1, 3*16]
	ldp	x25, x26, [x1, 4*16]
	ldp	x27, x28, [x1, 5*16]
	ldp	d8, d9,   [x1, 6*16]
	ldp	d10, d11, [x1, 7*16]
	ldp	d12, d13, [x1, 8*16]
	ldp	d14, d15, [x1, 9*16]

	/* GCS support.  */
	mov	x16, 1
	CHKFEAT_X16
	tbnz	x16, 0, L(gcs_done_lj)
	MRS_GCSPR (x7)
	ldr	x3, [x1, 10*16]
	mov	x4, x3
	/* x7: GCSPR now.  x3, x4: target GCSPR.  x5, x6: tmp regs.  */
L(gcs_scan):
	cmp	x7, x4
	b.eq	L(gcs_pop)
	sub	x4, x4, 8
	/* Check for a cap token.  */
	ldr	x5, [x4]
	and	x6, x4, 0xfffffffffffff000
	orr	x6, x6, 1
	cmp	x5, x6
	b.ne	L(gcs_scan)
L(gcs_switch):
	add	x7, x4, 8
	GCSSS1 (x4)
	GCSSS2 (xzr)
L(gcs_pop):
	cmp	x7, x3
	b.eq	L(gcs_done_lj)
	GCSPOPM (xzr)
	add	x7, x7, 8
	b	L(gcs_pop)
L(gcs_done_lj):

	ldr	x3, [x1, 10*16 + 8]
	ldp	x29, x30, [x1]
	cfi_def_cfa(x1, 0)
	CFI_PAC_TOGGLE
	mov	sp, x3
	AUT
	CFI_PAC_TOGGLE
	br	x30
	cfi_endproc
	.size	GTM_longjmp, . - GTM_longjmp

/* GNU_PROPERTY_AARCH64_* macros from elf.h for use in asm code.  */
#define FEATURE_1_AND 0xc0000000
#define FEATURE_1_BTI 1
#define FEATURE_1_PAC 2
#define FEATURE_1_GCS 4

/* Supported features based on the code generation options.  */
#if defined(__ARM_FEATURE_BTI_DEFAULT)
# define BTI_FLAG FEATURE_1_BTI
#else
# define BTI_FLAG 0
#endif

#if __ARM_FEATURE_PAC_DEFAULT & 3
# define PAC_FLAG FEATURE_1_PAC
#else
# define PAC_FLAG 0
#endif

#if __ARM_FEATURE_GCS_DEFAULT
# define GCS_FLAG FEATURE_1_GCS
#else
# define GCS_FLAG 0
#endif

/* Add a NT_GNU_PROPERTY_TYPE_0 note.  */
#define GNU_PROPERTY(type, value)	\
  .section .note.gnu.property, "a";	\
  .p2align 3;				\
  .word 4;				\
  .word 16;				\
  .word 5;				\
  .asciz "GNU";				\
  .word type;				\
  .word 4;				\
  .word value;				\
  .word 0;

#if defined(__linux__) || defined(__FreeBSD__)
.section .note.GNU-stack, "", %progbits

/* Add GNU property note if built with branch protection.  */
# if (BTI_FLAG|PAC_FLAG|GCS_FLAG) != 0
GNU_PROPERTY (FEATURE_1_AND, BTI_FLAG|PAC_FLAG|GCS_FLAG)
# endif
#endif
