	.global dumpregs
	.global myfunc
	.type dumpregs,%function
	.type myfunc,%function

#ifdef __LP64__
#define PTR_REG(n)      x##n
#else
#define PTR_REG(n)      w##n
#endif

#ifdef __CHERI_PURE_CAPABILITY__

dumpregs:
myfunc:
      mov	c16, csp
      mov	c17, csp
      sub	csp,  csp, 544 // 512 for registers and 32 for old csp and lr

      stp	c8, c9, [c17, #-32]! // 480
      stp	c6, c7, [c17, #-32]! // 448
      stp	c4, c5, [c17, #-32]! // 416
      stp	c2, c3, [c17, #-32]! // 384
      stp	c0, c1, [c17, #-32]! // 352

      sub	c17, c17, 8
      st4	{ v4.h, v5.h, v6.h, v7.h }[0], [c17] //344
      sub	c17, c17, 8
      st4	{ v0.h, v1.h, v2.h, v3.h }[0], [c17] //336

      stp	x8, x9, [c17, #-16]! //320

      stp	q6, q7, [c17, #-32]! //288
      stp	q4, q5, [c17, #-32]! //256
      stp	q2, q3, [c17, #-32]! //224
      stp	q0, q1, [c17, #-32]! //192

      stp	x6, x7, [c17, #-16]! //176
      stp	x4, x5, [c17, #-16]! //160
      stp	x2, x3, [c17, #-16]! //144
      stp	x0, x1, [c17, #-16]! //128

      stp	w6, w7, [c17, #-8]!  //120
      stp	w4, w5, [c17, #-8]!  //112
      stp	w2, w3, [c17, #-8]!  //104
      stp	w0, w1, [c17, #-8]!  // 96

      stp	s6, s7, [c17, #-8]!  // 88
      stp	s4, s5, [c17, #-8]!  // 80
      stp	s2, s3, [c17, #-8]!  // 72
      stp	s0, s1, [c17, #-8]!  // 64

      stp	d6, d7, [c17, #-16]! // 48
      stp	d4, d5, [c17, #-16]! // 32
      stp	d2, d3, [c17, #-16]! // 16
      stp	d0, d1, [c17, #-16]! //  0

      add	c0, csp,  #32
      stp	c16, c30, [c17, #-32]!

      adrp	c9, :got:which_kind_of_test		// determine the type of test
      ldr	c9, [c9, #:got_lo12:which_kind_of_test]
      ldr	w9, [c9, #0]
      cmp	w9, #1
      bgt	LABEL_TEST_FUNC_RETURN
      bl	testfunc			// parameter passing test or va_arg code gen test
      b		LABEL_RET
LABEL_TEST_FUNC_RETURN:
      adrp	c9, :got:testfunc_ptr
      ldr	c9, [c9, #:got_lo12:testfunc_ptr]
      ldr	c9, [c9, #0]
      blr	c9				// function return value test
      adrp	c9, :got:saved_return_address
      ldr	c9, [c9, #:got_lo12:saved_return_address]
      ldr	c9, [c9, #0]
      str	c9, [csp, #16]			// Update the copy of LR reg saved on stack
LABEL_RET:
      ldp	c0, c30, [csp]
      mov	csp, c0
      ret

.weak	testfunc
.weak	testfunc_ptr
.weak	saved_return_address

#else

dumpregs:
myfunc:
      mov	x16, sp
      mov	x17, sp
      sub	sp,  sp, 368 // 352 for registers and 16 for old sp and lr

      sub	x17, x17, 8
      st4	{ v4.h, v5.h, v6.h, v7.h }[0], [x17] //344
      sub	x17, x17, 8
      st4	{ v0.h, v1.h, v2.h, v3.h }[0], [x17] //336

      stp	x8, x9, [x17, #-16]! //320

      stp	q6, q7, [x17, #-32]! //288
      stp	q4, q5, [x17, #-32]! //256
      stp	q2, q3, [x17, #-32]! //224
      stp	q0, q1, [x17, #-32]! //192

      stp	x6, x7, [x17, #-16]! //176
      stp	x4, x5, [x17, #-16]! //160
      stp	x2, x3, [x17, #-16]! //144
      stp	x0, x1, [x17, #-16]! //128

      stp	w6, w7, [x17, #-8]!  //120
      stp	w4, w5, [x17, #-8]!  //112
      stp	w2, w3, [x17, #-8]!  //104
      stp	w0, w1, [x17, #-8]!  // 96

      stp	s6, s7, [x17, #-8]!  // 88
      stp	s4, s5, [x17, #-8]!  // 80
      stp	s2, s3, [x17, #-8]!  // 72
      stp	s0, s1, [x17, #-8]!  // 64

      stp	d6, d7, [x17, #-16]! // 48
      stp	d4, d5, [x17, #-16]! // 32
      stp	d2, d3, [x17, #-16]! // 16
      stp	d0, d1, [x17, #-16]! //  0

      add	x0, sp,  #16
      stp	x16, x30, [x17, #-16]!

      adrp	x9, which_kind_of_test		// determine the type of test
      add	x9, x9, :lo12:which_kind_of_test
      ldr	w9, [x9, #0]
      cmp	w9, #1
      bgt	LABEL_TEST_FUNC_RETURN
      bl	testfunc			// parameter passing test or va_arg code gen test
      b		LABEL_RET
LABEL_TEST_FUNC_RETURN:
      adrp	x9, testfunc_ptr
      add	x9, x9, :lo12:testfunc_ptr
      ldr      PTR_REG(9), [x9, #0]
      blr	x9				// function return value test
      adrp	x9, saved_return_address
      add	x9, x9, :lo12:saved_return_address
      ldr	x9, [x9, #0]
      str	x9, [sp, #8]			// Update the copy of LR reg saved on stack
LABEL_RET:
      ldp	x0, x30, [sp]
      mov	sp, x0
      ret

.weak	testfunc
.weak	testfunc_ptr
.weak	saved_return_address
#endif
