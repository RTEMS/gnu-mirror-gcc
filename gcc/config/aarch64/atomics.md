;; Machine description for AArch64 processor synchronization primitives.
;; Copyright (C) 2009-2020 Free Software Foundation, Inc.
;; Contributed by ARM Ltd.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Instruction patterns.

(define_expand "@atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")			;; bool out
   (match_operand:ALLI_TIC 1 "register_operand" "")		;; val out
   (match_operand:ALLI_TIC 2 "aarch64_sync_memory_operand" "")	;; memory
   (match_operand:ALLI_TIC 3 "nonmemory_operand" "")		;; expected
   (match_operand:ALLI_TIC 4 "aarch64_reg_or_zero" "")		;; desired
   (match_operand:SI 5 "const_int_operand")			;; is_weak
   (match_operand:SI 6 "const_int_operand")			;; mod_s
   (match_operand:SI 7 "const_int_operand")]			;; mod_f
  ""
  {
    aarch64_expand_compare_and_swap (operands);
    DONE;
  }
)

(define_mode_attr cas_short_expected_pred
  [(QI "aarch64_reg_or_imm") (HI "aarch64_plushi_operand")])
(define_mode_attr cas_short_expected_imm
  [(QI "n") (HI "Uph")])

(define_insn_and_split "@aarch64_compare_and_swap<mode>"
  [(set (reg:CC CC_REGNUM)					;; bool out
    (unspec_volatile:CC [(const_int 0)] UNSPECV_ATOMIC_CMPSW))
   (set (match_operand:SI 0 "register_operand" "=&r")		;; val out
    (zero_extend:SI
      (match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q"))) ;; memory
   (set (match_dup 1)
    (unspec_volatile:SHORT
      [(match_operand:SHORT 2 "<cas_short_expected_pred>"
			      "r<cas_short_expected_imm>")	;; expected
       (match_operand:SHORT 3 "aarch64_reg_or_zero" "rZ")	;; desired
       (match_operand:SI 4 "const_int_operand")			;; is_weak
       (match_operand:SI 5 "const_int_operand")			;; mod_s
       (match_operand:SI 6 "const_int_operand")]		;; mod_f
      UNSPECV_ATOMIC_CMPSW))
   (clobber (match_scratch:SI 7 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_compare_and_swap (operands);
    DONE;
  }
)

(define_insn_and_split "@aarch64_compare_and_swap<mode>"
  [(set (reg:CC CC_REGNUM)					;; bool out
    (unspec_volatile:CC [(const_int 0)] UNSPECV_ATOMIC_CMPSW))
   (set (match_operand:GPIC 0 "register_operand" "=&r")		;; val out
    (match_operand:GPIC 1 "aarch64_sync_memory_operand" "+Q"))   ;; memory
   (set (match_dup 1)
    (unspec_volatile:GPIC
      [(match_operand:GPIC 2 "aarch64_plus_operand" "rIJ")	;; expect
       (match_operand:GPIC 3 "aarch64_reg_or_zero" "rZ")	;; desired
       (match_operand:SI 4 "const_int_operand")			;; is_weak
       (match_operand:SI 5 "const_int_operand")			;; mod_s
       (match_operand:SI 6 "const_int_operand")]		;; mod_f
      UNSPECV_ATOMIC_CMPSW))
   (clobber (match_scratch:SI 7 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_compare_and_swap (operands);
    DONE;
  }
)

(define_insn_and_split "@aarch64_compare_and_swap<mode>"
  [(set (reg:CC CC_REGNUM)					;; bool out
    (unspec_volatile:CC [(const_int 0)] UNSPECV_ATOMIC_CMPSW))
   (set (match_operand:JUST_TI 0 "register_operand" "=&r")	;; val out
    (match_operand:JUST_TI 1 "aarch64_sync_memory_operand" "+Q")) ;; memory
   (set (match_dup 1)
    (unspec_volatile:JUST_TI
      [(match_operand:JUST_TI 2 "aarch64_reg_or_zero" "rZ")	;; expect
       (match_operand:JUST_TI 3 "aarch64_reg_or_zero" "rZ")	;; desired
       (match_operand:SI 4 "const_int_operand")			;; is_weak
       (match_operand:SI 5 "const_int_operand")			;; mod_s
       (match_operand:SI 6 "const_int_operand")]		;; mod_f
      UNSPECV_ATOMIC_CMPSW))
   (clobber (match_scratch:SI 7 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_compare_and_swap (operands);
    DONE;
  }
)

(define_insn "@aarch64_compare_and_swap<mode>_lse"
  [(set (match_operand:SI 0 "register_operand" "+r")		;; val out
    (zero_extend:SI
     (match_operand:SHORT 1 "aarch64_sync_memory_operand" "+Q"))) ;; memory
   (set (match_dup 1)
    (unspec_volatile:SHORT
      [(match_dup 0)						;; expected
       (match_operand:SHORT 2 "aarch64_reg_or_zero" "rZ")	;; desired
       (match_operand:SI 3 "const_int_operand")]		;; mod_s
      UNSPECV_ATOMIC_CMPSW))]
  "TARGET_LSE"
{
  enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
  if (is_mm_relaxed (model))
    return "cas<atomic_sfx>\t%<w>0, %<w>2, %1";
  else if (is_mm_acquire (model) || is_mm_consume (model))
    return "casa<atomic_sfx>\t%<w>0, %<w>2, %1";
  else if (is_mm_release (model))
    return "casl<atomic_sfx>\t%<w>0, %<w>2, %1";
  else
    return "casal<atomic_sfx>\t%<w>0, %<w>2, %1";
})

(define_insn "@aarch64_compare_and_swap<mode>_lse"
  [(set (match_operand:GPIC 0 "register_operand" "+r")		;; val out
    (match_operand:GPIC 1 "aarch64_sync_memory_operand" "+Q"))   ;; memory
   (set (match_dup 1)
    (unspec_volatile:GPIC
      [(match_dup 0)						;; expected
       (match_operand:GPIC 2 "aarch64_reg_or_zero" "rZ")	;; desired
       (match_operand:SI 3 "const_int_operand")]		;; mod_s
      UNSPECV_ATOMIC_CMPSW))]
  "TARGET_LSE"
{
  enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
  if (is_mm_relaxed (model))
    return "cas<atomic_sfx>\t%<w>0, %<w>2, %1";
  else if (is_mm_acquire (model) || is_mm_consume (model))
    return "casa<atomic_sfx>\t%<w>0, %<w>2, %1";
  else if (is_mm_release (model))
    return "casl<atomic_sfx>\t%<w>0, %<w>2, %1";
  else
    return "casal<atomic_sfx>\t%<w>0, %<w>2, %1";
})

(define_insn "@aarch64_compare_and_swap<mode>_lse"
  [(set (match_operand:JUST_TI 0 "register_operand" "+r")	;; val out
    (match_operand:JUST_TI 1 "aarch64_sync_memory_operand" "+Q")) ;; memory
   (set (match_dup 1)
    (unspec_volatile:JUST_TI
      [(match_dup 0)						;; expect
       (match_operand:JUST_TI 2 "register_operand" "r")		;; desired
       (match_operand:SI 3 "const_int_operand")]		;; mod_s
      UNSPECV_ATOMIC_CMPSW))]
  "TARGET_LSE"
{
  enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
  if (is_mm_relaxed (model))
    return "casp\t%0, %R0, %2, %R2, %1";
  else if (is_mm_acquire (model) || is_mm_consume (model))
    return "caspa\t%0, %R0, %2, %R2, %1";
  else if (is_mm_release (model))
    return "caspl\t%0, %R0, %2, %R2, %1";
  else
    return "caspal\t%0, %R0, %2, %R2, %1";
})

(define_expand "atomic_exchange<mode>"
 [(match_operand:ALLIC 0 "register_operand")
  (match_operand:ALLIC 1 "aarch64_sync_memory_operand")
  (match_operand:ALLIC 2 "aarch64_reg_or_zero")
  (match_operand:SI 3 "const_int_operand")]
  ""
  {
    /* Use an atomic SWP when available.  */
    if (TARGET_LSE)
      {
	emit_insn (gen_aarch64_atomic_exchange<mode>_lse
		   (operands[0], operands[1], operands[2], operands[3]));
      }
    else if (TARGET_OUTLINE_ATOMICS)
      {
	machine_mode mode = <MODE>mode;
	gcc_assert (mode != CADImode || TARGET_CAPABILITY_FAKE);

	rtx func = aarch64_atomic_ool_func (mode, operands[3],
					    &aarch64_ool_swp_names);
	rtx rval = emit_library_call_value (func, operands[0], LCT_NORMAL,
					    mode, operands[2], mode,
					    XEXP (operands[1], 0), Pmode);
        emit_move_insn (operands[0], rval);
      }
    else
      {
	emit_insn (gen_aarch64_atomic_exchange<mode>
		   (operands[0], operands[1], operands[2], operands[3]));
      }
    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_exchange<mode>"
  [(set (match_operand:ALLIC 0 "register_operand" "=&r")		;; output
    (match_operand:ALLIC 1 "aarch64_sync_memory_operand" "+Q"))	;; memory
   (set (match_dup 1)
    (unspec_volatile:ALLIC
      [(match_operand:ALLIC 2 "aarch64_reg_or_zero" "rZ")	;; input
       (match_operand:SI 3 "const_int_operand" "")]		;; model
      UNSPECV_ATOMIC_EXCHG))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 4 "=&r"))]
  ;; The LSE version of this pattern has the same form as this base Armv8-A
  ;; version, but without the clobbers.  We always want to use the LSE version
  ;; where possible, so prevent combine from converting an LSE pattern into
  ;; a base Armv8-A one.
  "!TARGET_LSE"
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (SET, operands[0], NULL_RTX, operands[1],
			     operands[2], operands[3], NULL_RTX, operands[4]);
    DONE;
  }
)

(define_insn "aarch64_atomic_exchange<mode>_lse"
  [(set (match_operand:ALLIC 0 "register_operand" "=r")
    (match_operand:ALLIC 1 "aarch64_sync_memory_operand" "+Q"))
   (set (match_dup 1)
    (unspec_volatile:ALLIC
      [(match_operand:ALLIC 2 "aarch64_reg_or_zero" "rZ")
       (match_operand:SI 3 "const_int_operand" "")]
      UNSPECV_ATOMIC_EXCHG))]
  "TARGET_LSE"
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
    if (is_mm_relaxed (model))
      return "swp<atomic_sfx>\t%<w>2, %<w>0, %1";
    else if (is_mm_acquire (model) || is_mm_consume (model))
      return "swpa<atomic_sfx>\t%<w>2, %<w>0, %1";
    else if (is_mm_release (model))
      return "swpl<atomic_sfx>\t%<w>2, %<w>0, %1";
    else
      return "swpal<atomic_sfx>\t%<w>2, %<w>0, %1";
  }
)

(define_expand "atomic_<atomic_optab><mode>"
 [(match_operand:ALLI 0 "aarch64_sync_memory_operand")
  (atomic_op:ALLI
   (match_operand:ALLI 1 "<atomic_op_operand>")
   (match_operand:SI 2 "const_int_operand"))]
  ""
  {
    rtx (*gen) (rtx, rtx, rtx);

    /* Use an atomic load-operate instruction when possible.  */
    if (TARGET_LSE)
      {
	switch (<CODE>)
	  {
	  case MINUS:
	    operands[1] = expand_simple_unop (<MODE>mode, NEG, operands[1],
					      NULL, 1);
	    /* fallthru */
	  case PLUS:
	    gen = gen_aarch64_atomic_add<mode>_lse;
	    break;
	  case IOR:
	    gen = gen_aarch64_atomic_ior<mode>_lse;
	    break;
	  case XOR:
	    gen = gen_aarch64_atomic_xor<mode>_lse;
	    break;
	  case AND:
	    operands[1] = expand_simple_unop (<MODE>mode, NOT, operands[1],
					      NULL, 1);
	    gen = gen_aarch64_atomic_bic<mode>_lse;
	    break;
	  default:
	    gcc_unreachable ();
	  }
	operands[1] = force_reg (<MODE>mode, operands[1]);
      }
    else if (TARGET_OUTLINE_ATOMICS)
      {
        const atomic_ool_names *names;
	switch (<CODE>)
	  {
	  case MINUS:
	    operands[1] = expand_simple_unop (<MODE>mode, NEG, operands[1],
					      NULL, 1);
	    /* fallthru */
	  case PLUS:
	    names = &aarch64_ool_ldadd_names;
	    break;
	  case IOR:
	    names = &aarch64_ool_ldset_names;
	    break;
	  case XOR:
	    names = &aarch64_ool_ldeor_names;
	    break;
	  case AND:
	    operands[1] = expand_simple_unop (<MODE>mode, NOT, operands[1],
					      NULL, 1);
	    names = &aarch64_ool_ldclr_names;
	    break;
	  default:
	    gcc_unreachable ();
	  }
        machine_mode mode = <MODE>mode;
	rtx func = aarch64_atomic_ool_func (mode, operands[2], names);
	emit_library_call_value (func, NULL_RTX, LCT_NORMAL, mode,
				 operands[1], mode,
				 XEXP (operands[0], 0), Pmode);
        DONE;
      }
    else
      gen = gen_aarch64_atomic_<atomic_optab><mode>;

    emit_insn (gen (operands[0], operands[1], operands[2]));
    DONE;
  }
)

(define_expand "atomic_<atomic_optab>cadi"
 [(match_operand:CADI 0 "aarch64_sync_memory_operand")
  (atomic_op:CADI
   (match_operand:DI 1 "<atomic_op_operand>")
   (match_operand:SI 2 "const_int_operand"))]
  ""
  {
    rtx (*gen) (rtx, rtx, rtx);

    /* Use an atomic load-operate instruction when possible.
       Note that there is no point using outline atomics when actually
       targetting Morello since we know the architecture that we're targetting,
       and anyway there aren't the LSE instructions to act on capability data.

       (OOL atomics was introduced so we could generate a program which would
       work on ARMv8-a targets but also efficiently use the LSE instructions
       when running on a system that supports it).  */
    if (TARGET_OUTLINE_ATOMICS && TARGET_CAPABILITY_FAKE)
      {
	const atomic_ool_names *names;

	switch (<CODE>)
	  {
	  case MINUS:
	    operands[1] = expand_simple_unop (DImode, NEG, operands[1],
					      NULL_RTX, 1);
	    /* fallthru */
	  case PLUS:
	    names = &aarch64_ool_ldadd_names;
	    break;
	  case IOR:
	    names = &aarch64_ool_ldset_names;
	    break;
	  case XOR:
	    names = &aarch64_ool_ldeor_names;
	    break;
	  case AND:
	    operands[1] = expand_simple_unop (DImode, NOT, operands[1],
					      NULL_RTX, 1);
	    names = &aarch64_ool_ldclr_names;
	    break;
	  default:
	    gcc_unreachable ();
	  }
	rtx func = aarch64_atomic_ool_func (CADImode, operands[2], names);
	emit_library_call_value (func, NULL_RTX, LCT_NORMAL, CADImode,
				 operands[1], DImode,
				 XEXP (operands[0], 0), Pmode);
        DONE;
      }
    else
      gen = gen_aarch64_atomic_<atomic_optab>cadi;

    emit_insn (gen (operands[0], operands[1], operands[2]));
    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_<atomic_optab><mode>"
 [(set (match_operand:ALLI 0 "aarch64_sync_memory_operand" "+Q")
   (unspec_volatile:ALLI
    [(atomic_op:ALLI (match_dup 0)
      (match_operand:ALLI 1 "<atomic_op_operand>" "r<const_atomic>"))
     (match_operand:SI 2 "const_int_operand")]
    UNSPECV_ATOMIC_OP))
  (clobber (reg:CC CC_REGNUM))
  (clobber (match_scratch:ALLI 3 "=&r"))
  (clobber (match_scratch:SI 4 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (<CODE>, NULL_RTX, operands[3], operands[0],
			     operands[1], operands[2], NULL_RTX, operands[4]);
    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_<atomic_capoptab>cadi"
[(replace_address_value:CADI (match_operand:CADI 0 "aarch64_sync_memory_operand" "+Q")
   (unspec_volatile:DI
    [(match_operand:DI 1 "<atomic_capop_operand>" "r<atomic_capopconst_operand>")
     (match_operand:SI 2 "const_int_operand")]
    ATOMIC_CAPOP))
  (clobber (reg:CC CC_REGNUM))
  (clobber (match_scratch:CADI 3 "=&r"))
  (clobber (match_scratch:DI 4 "=&r"))
  (clobber (match_scratch:SI 5 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (<atomic_capopcode>, NULL_RTX, operands[3],
			     operands[0], operands[1], operands[2],
			     operands[4], operands[5]);
    DONE;
  }
)

;; It is tempting to want to use ST<OP> for relaxed and release
;; memory models here.  However, that is incompatible with the
;; C++ memory model for the following case:
;;
;;	atomic_fetch_add(ptr, 1, memory_order_relaxed);
;;	atomic_thread_fence(memory_order_acquire);
;;
;; The problem is that the architecture says that ST<OP> (and LD<OP>
;; insns where the destination is XZR) are not regarded as a read.
;; However we also implement the acquire memory barrier with DMB LD,
;; and so the ST<OP> is not blocked by the barrier.

(define_insn "aarch64_atomic_<atomic_ldoptab><mode>_lse"
  [(set (match_operand:ALLI 0 "aarch64_sync_memory_operand" "+Q")
	(unspec_volatile:ALLI
	  [(match_dup 0)
	   (match_operand:ALLI 1 "register_operand" "r")
	   (match_operand:SI 2 "const_int_operand")]
      ATOMIC_LDOP))
   (clobber (match_scratch:ALLI 3 "=r"))]
  "TARGET_LSE"
  {
   enum memmodel model = memmodel_from_int (INTVAL (operands[2]));
   if (is_mm_relaxed (model))
     return "ld<atomic_ldop><atomic_sfx>\t%<w>1, %<w>3, %0";
   else if (is_mm_release (model))
     return "ld<atomic_ldop>l<atomic_sfx>\t%<w>1, %<w>3, %0";
   else if (is_mm_acquire (model) || is_mm_consume (model))
     return "ld<atomic_ldop>a<atomic_sfx>\t%<w>1, %<w>3, %0";
   else
     return "ld<atomic_ldop>al<atomic_sfx>\t%<w>1, %<w>3, %0";
  }
)

(define_insn_and_split "atomic_nand<mode>"
  [(set (match_operand:ALLI 0 "aarch64_sync_memory_operand" "+Q")
    (unspec_volatile:ALLI
      [(not:ALLI
	(and:ALLI (match_dup 0)
	  (match_operand:ALLI 1 "aarch64_logical_operand" "r<lconst_atomic>")))
       (match_operand:SI 2 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:ALLI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
     aarch64_split_atomic_op (NOT, NULL_RTX, operands[3], operands[0],
			     operands[1], operands[2], NULL_RTX, operands[4]);
     DONE;
  }
)

(define_insn_and_split "atomic_nandcadi"
  [(replace_address_value:CADI (match_operand:CADI 0 "aarch64_sync_memory_operand" "+Q")
    (unspec_volatile:DI
      [(match_dup 0)
       (match_operand:DI 1 "aarch64_logical_operand" "rL")
       (match_operand:SI 2 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_CAPABILITY_NAND))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:CADI 3 "=&r"))
   (clobber (match_scratch:DI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))
   ]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
     aarch64_split_atomic_op (NOT, NULL_RTX, operands[3], operands[0],
			     operands[1], operands[2], operands[4],
			     operands[5]);
     DONE;
  }
)

;; Load-operate-store, returning the original memory data.

(define_expand "atomic_fetch_<atomic_optab><mode>"
 [(match_operand:ALLI 0 "register_operand")
  (match_operand:ALLI 1 "aarch64_sync_memory_operand")
  (atomic_op:ALLI
   (match_operand:ALLI 2 "<atomic_op_operand>")
   (match_operand:SI 3 "const_int_operand"))]
 ""
{
  rtx (*gen) (rtx, rtx, rtx, rtx);

  /* Use an atomic load-operate instruction when possible.  */
  if (TARGET_LSE)
    {
      switch (<CODE>)
        {
	case MINUS:
	  operands[2] = expand_simple_unop (<MODE>mode, NEG, operands[2],
					    NULL, 1);
	  /* fallthru */
	case PLUS:
	  gen = gen_aarch64_atomic_fetch_add<mode>_lse;
	  break;
	case IOR:
	  gen = gen_aarch64_atomic_fetch_ior<mode>_lse;
	  break;
	case XOR:
	  gen = gen_aarch64_atomic_fetch_xor<mode>_lse;
	  break;
	case AND:
	  operands[2] = expand_simple_unop (<MODE>mode, NOT, operands[2],
					    NULL, 1);
	  gen = gen_aarch64_atomic_fetch_bic<mode>_lse;
	  break;
	default:
	  gcc_unreachable ();
	}
      operands[2] = force_reg (<MODE>mode, operands[2]);
    }
  else if (TARGET_OUTLINE_ATOMICS)
    {
      const atomic_ool_names *names;
      switch (<CODE>)
	{
	case MINUS:
	  operands[2] = expand_simple_unop (<MODE>mode, NEG, operands[2],
					    NULL, 1);
	  /* fallthru */
	case PLUS:
	  names = &aarch64_ool_ldadd_names;
	  break;
	case IOR:
	  names = &aarch64_ool_ldset_names;
	  break;
	case XOR:
	  names = &aarch64_ool_ldeor_names;
	  break;
	case AND:
	  operands[2] = expand_simple_unop (<MODE>mode, NOT, operands[2],
					    NULL, 1);
	  names = &aarch64_ool_ldclr_names;
	  break;
	default:
	  gcc_unreachable ();
	}
      machine_mode mode = <MODE>mode;
      rtx func = aarch64_atomic_ool_func (mode, operands[3], names);
      rtx rval = emit_library_call_value (func, operands[0], LCT_NORMAL, mode,
					  operands[2], mode,
					  XEXP (operands[1], 0), Pmode);
      emit_move_insn (operands[0], rval);
      DONE;
    }
  else
    gen = gen_aarch64_atomic_fetch_<atomic_optab><mode>;

  emit_insn (gen (operands[0], operands[1], operands[2], operands[3]));
  DONE;
})

(define_expand "atomic_fetch_<atomic_optab>cadi"
 [(match_operand:CADI 0 "register_operand")
  (match_operand:CADI 1 "aarch64_sync_memory_operand")
  (atomic_op:CADI
   (match_operand:DI 2 "<atomic_op_operand>")
   (match_operand:SI 3 "const_int_operand"))]
 ""
{
  rtx (*gen) (rtx, rtx, rtx, rtx);

  /* Use an atomic load-operate instruction when possible.  */
  if (TARGET_OUTLINE_ATOMICS && TARGET_CAPABILITY_FAKE)
    {
      const atomic_ool_names *names;
      switch (<CODE>)
	{
	case MINUS:
	  operands[2] = expand_simple_unop (DImode, NEG, operands[2],
					    NULL_RTX, 1);
	  /* fallthru */
	case PLUS:
	  names = &aarch64_ool_ldadd_names;
	  break;
	case IOR:
	  names = &aarch64_ool_ldset_names;
	  break;
	case XOR:
	  names = &aarch64_ool_ldeor_names;
	  break;
	case AND:
	  operands[2] = expand_simple_unop (DImode, NOT, operands[2],
					    NULL_RTX, 1);
	  names = &aarch64_ool_ldclr_names;
	  break;
	default:
	  gcc_unreachable ();
	}
      rtx func = aarch64_atomic_ool_func (CADImode, operands[3], names);
      rtx rval = emit_library_call_value (func, operands[0], LCT_NORMAL,
					  CADImode, operands[2], DImode,
					  XEXP (operands[1], 0), Pmode);
      emit_move_insn (operands[0], rval);
      DONE;
    }
  else
    gen = gen_aarch64_atomic_fetch_<atomic_optab>cadi;
  emit_insn (gen (operands[0], operands[1], operands[2], operands[3]));
  DONE;
})

(define_insn_and_split "aarch64_atomic_fetch_<atomic_optab><mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q"))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(atomic_op:ALLI (match_dup 1)
	(match_operand:ALLI 2 "<atomic_op_operand>" "r<const_atomic>"))
       (match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:ALLI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (<CODE>, operands[0], operands[4], operands[1],
			     operands[2], operands[3], NULL_RTX, operands[5]);
    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_fetch_<atomic_capoptab>cadi"

 [(set (match_operand:CADI 0 "register_operand" "=&r")
       (match_operand:CADI 1 "aarch64_sync_memory_operand" "+Q"))
   (replace_address_value:CADI
    (match_dup 1)
    (unspec_volatile:DI
      [(match_dup 1)
       (match_operand:DI 2 "<atomic_capop_operand>" "r<atomic_capopconst_operand>")
       (match_operand:SI 3 "const_int_operand")]		;; model
      ATOMIC_CAPOP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:CADI 4 "=&r"))
   (clobber (match_scratch:DI 5 "=&r"))
   (clobber (match_scratch:SI 6 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (<atomic_capopcode>, operands[0], operands[4],
			     operands[1], operands[2], operands[3],
			     operands[5], operands[6]);
    DONE;
  }
)

(define_insn "aarch64_atomic_fetch_<atomic_ldoptab><mode>_lse"
  [(set (match_operand:ALLI 0 "register_operand" "=r")
	(match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q"))
   (set (match_dup 1)
	(unspec_volatile:ALLI
	  [(match_dup 1)
	   (match_operand:ALLI 2 "register_operand" "r")
	   (match_operand:SI 3 "const_int_operand")]
	  ATOMIC_LDOP))]
  "TARGET_LSE"
  {
   enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
   if (is_mm_relaxed (model))
     return "ld<atomic_ldop><atomic_sfx>\t%<w>2, %<w>0, %1";
   else if (is_mm_acquire (model) || is_mm_consume (model))
     return "ld<atomic_ldop>a<atomic_sfx>\t%<w>2, %<w>0, %1";
   else if (is_mm_release (model))
     return "ld<atomic_ldop>l<atomic_sfx>\t%<w>2, %<w>0, %1";
   else
     return "ld<atomic_ldop>al<atomic_sfx>\t%<w>2, %<w>0, %1";
  }
)

(define_insn_and_split "atomic_fetch_nand<mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q"))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(not:ALLI
	 (and:ALLI (match_dup 1)
	   (match_operand:ALLI 2 "aarch64_logical_operand" "r<lconst_atomic>")))
       (match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:ALLI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (NOT, operands[0], operands[4], operands[1],
			    operands[2], operands[3], NULL_RTX, operands[5]);
    DONE;
  }
)

(define_insn_and_split "atomic_fetch_nandcadi"
  [(set (match_operand:CADI 0 "register_operand" "=&r")
        (match_operand:CADI 1 "aarch64_sync_memory_operand" "+Q"))
   (replace_address_value:CADI (match_dup 1)
     (unspec_volatile:DI
       [(match_dup 1)
	(match_operand:DI 2 "aarch64_logical_operand" "rL")
	(match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_CAPABILITY_NAND))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:CADI 4 "=&r"))
   (clobber (match_scratch:DI 5 "=&r"))
   (clobber (match_scratch:SI 6 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (NOT, operands[0], operands[4], operands[1],
			    operands[2], operands[3], operands[5],
			    operands[6]);
    DONE;
  }
)

;; Load-operate-store, returning the updated memory data.

(define_expand "atomic_<atomic_optab>_fetch<mode>"
 [(match_operand:ALLIC 0 "register_operand")
  (atomic_op:ALLIC
   (match_operand:ALLIC 1 "aarch64_sync_memory_operand")
   (match_operand:<PTR_OFF> 2 "<atomic_op_operand>"))
  (match_operand:SI 3 "const_int_operand")]
 ""
{
  /* Use an atomic load-operate instruction when possible.  In this case
     we will re-compute the result from the original mem value. */
  if ((TARGET_LSE || TARGET_OUTLINE_ATOMICS)
       && !(TARGET_MORELLO && CAPABILITY_MODE_P (<MODE>mode)))
    {
      rtx tmp = gen_reg_rtx (<MODE>mode);
      operands[2] = force_reg (<PTR_OFF>mode, operands[2]);
      emit_insn (gen_atomic_fetch_<atomic_optab><mode>
                 (tmp, operands[1], operands[2], operands[3]));

      if (CAPABILITY_MODE_P (<MODE>mode))
	{
	  switch (<CODE>)
	    {
	    case PLUS:
	      tmp = expand_pointer_plus (<MODE>mode, tmp, operands[2],
					 operands[0], 1, OPTAB_WIDEN);
	      break;
	    case MINUS:
	      tmp = expand_pointer_minus (<MODE>mode, tmp, operands[2],
					  operands[0], 1, OPTAB_WIDEN);
	      break;
	    default:
	      tmp = expand_simple_binop (noncapability_mode (<MODE>mode),
					 <CODE>, drop_capability (tmp),
					 operands[2], tmp, 1, OPTAB_WIDEN);
	      tmp = expand_replace_address_value (as_a <scalar_addr_mode>
						    (<MODE>mode),
						  operands[0], tmp,
						  operands[0]);
	    }
	}
      else
	{
	  tmp = expand_simple_binop (<MODE>mode, <CODE>, tmp, operands[2],
				     operands[0], 1, OPTAB_WIDEN);
	}
      emit_move_insn (operands[0], tmp);
    }
  else
    {
      emit_insn (gen_aarch64_atomic_<atomic_optab>_fetch<mode>
                 (operands[0], operands[1], operands[2], operands[3]));
    }
  DONE;
})

(define_insn_and_split "aarch64_atomic_<atomic_optab>_fetch<mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (atomic_op:ALLI
      (match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q")
      (match_operand:ALLI 2 "<atomic_op_operand>" "r<const_atomic>")))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(match_dup 1) (match_dup 2)
       (match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_OP))
    (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 4 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (<CODE>, NULL_RTX, operands[0], operands[1],
			     operands[2], operands[3], NULL_RTX, operands[4]);
    DONE;
  }
)

(define_insn_and_split "aarch64_atomic_<atomic_capoptab>_fetchcadi"
  [(replace_address_value:CADI (match_operand:CADI 0 "register_operand" "=&r")
    (unspec_volatile:DI
      [(match_operand:CADI 1 "aarch64_sync_memory_operand" "+Q")
      (match_operand:DI 2 "<atomic_capop_operand>" "r<atomic_capopconst_operand>")]
      ATOMIC_CAPOP))
   (set (match_dup 1)
    (unspec_volatile:CADI
      [(match_dup 1) (match_dup 2)
       (match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_CAPABILITY_SET))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:DI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (<atomic_capopcode>, NULL_RTX, operands[0],
			     operands[1], operands[2], operands[3],
			     operands[4], operands[5]);
    DONE;
  }
)

(define_insn_and_split "atomic_nand_fetch<mode>"
  [(set (match_operand:ALLI 0 "register_operand" "=&r")
    (not:ALLI
      (and:ALLI
	(match_operand:ALLI 1 "aarch64_sync_memory_operand" "+Q")
	(match_operand:ALLI 2 "aarch64_logical_operand" "r<lconst_atomic>"))))
   (set (match_dup 1)
    (unspec_volatile:ALLI
      [(match_dup 1) (match_dup 2)
       (match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_OP))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:SI 4 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (NOT, NULL_RTX, operands[0], operands[1],
			    operands[2], operands[3], NULL_RTX, operands[4]);
    DONE;
  }
)

(define_insn_and_split "atomic_nand_fetchcadi"
  [(replace_address_value:CADI (match_operand:CADI 0 "register_operand" "=&r")
    (unspec_volatile:DI
	[(match_operand:CADI 1 "aarch64_sync_memory_operand" "+Q")
	 (match_operand:DI 2 "aarch64_logical_operand" "rL")]
	UNSPECV_ATOMIC_CAPABILITY_NAND))
   (set (match_dup 1)
    (unspec_volatile:CADI
      [(match_dup 1) (match_dup 2)
       (match_operand:SI 3 "const_int_operand")]		;; model
      UNSPECV_ATOMIC_CAPABILITY_SET))
   (clobber (reg:CC CC_REGNUM))
   (clobber (match_scratch:DI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))]
  ""
  "#"
  "&& epilogue_completed"
  [(const_int 0)]
  {
    aarch64_split_atomic_op (NOT, NULL_RTX, operands[0], operands[1],
			    operands[2], operands[3], operands[4],
			    operands[5]);
    DONE;
  }
)

(define_expand "atomic_load<mode>"
  [(set (match_operand:ALLIC 0 "register_operand")
    (unspec_volatile:ALLIC
      [(match_operand:ALLIC 1 "aarch64_atomic_load_memory_operand")
       (match_operand:SI 2 "const_int_operand")]			;; model
      UNSPECV_LDA))]
  ""
  {
    if (!aarch64_atomic_load_store_ok_p (operands[1], operands[2]))
      FAIL;
  }
)

(define_insn "*atomic_load<mode>"
  [(set (match_operand:ALLIC 0 "register_operand" "=r")
    (unspec_volatile:ALLIC
      [(match_operand:ALLIC 1 "aarch64_atomic_load_memory_operand" "Q")
       (match_operand:SI 2 "const_int_operand")]			;; model
      UNSPECV_LDA))]
  "aarch64_atomic_load_store_ok_p (operands[1], operands[2])"
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[2]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_release (model))
      return "ldr<atomic_sfx>\t%<w>0, %1";
    else
      return "ldar<atomic_sfx>\t%<w>0, %1";
  }
)

(define_expand "atomic_store<mode>"
  [(set (match_operand:ALLIC 0 "aarch64_rcpc_memory_operand")
    (unspec_volatile:ALLIC
      [(match_operand:ALLIC 1 "general_operand")
       (match_operand:SI 2 "const_int_operand")]			;; model
      UNSPECV_STL))]
  ""
  {
    if (!aarch64_atomic_load_store_ok_p (operands[0], operands[2]))
      FAIL;
  }
)

(define_expand "atomic_storeti"
  [(set (match_operand:TI 0 "aarch64_rcpc_memory_operand")
	(unspec_volatile:TI
	  [(match_operand:TI 1 "const0_operand")
	   (match_operand:SI 2 "const_int_operand")]		;; model
	  UNSPECV_STL))]
  "TARGET_MORELLO"
  {
    operands[0] = adjust_address (operands[0], CADImode, 0);
    emit_insn (gen_atomic_storecadi (operands[0], CONST0_RTX (CADImode),
				     operands[2]));
    DONE;
  }
)

(define_insn "*atomic_store<mode>"
  [(set (match_operand:ALLIC 0 "aarch64_rcpc_memory_operand" "=Q,Ust")
    (unspec_volatile:ALLIC
      [(match_operand:ALLIC 1 "general_operand" "rZ,rZ")
       (match_operand:SI 2 "const_int_operand")]			;; model
      UNSPECV_STL))]
  "aarch64_atomic_load_store_ok_p (operands[0], operands[2])"
  {
    gcc_assert (which_alternative == 0 || <MODE>mode != CADImode
		|| TARGET_CAPABILITY_FAKE);
    enum memmodel model = memmodel_from_int (INTVAL (operands[2]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_acquire (model))
      return "str<atomic_sfx>\t%<w>1, %0";
    else if (which_alternative == 0)
      return "stlr<atomic_sfx>\t%<w>1, %0";
    else
      return "stlur<atomic_sfx>\t%<w>1, %0";
  }
  [(set_attr "arch" "*,rcpc8_4")]
)

(define_insn "@aarch64_load_exclusive<mode>"
  [(set (match_operand:SI 0 "register_operand" "=r")
    (zero_extend:SI
      (unspec_volatile:SHORT
	[(match_operand:SHORT 1 "aarch64_sync_memory_operand" "Q")
	 (match_operand:SI 2 "const_int_operand")]
	UNSPECV_LX)))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[2]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_release (model))
      return "ldxr<atomic_sfx>\t%w0, %1";
    else
      return "ldaxr<atomic_sfx>\t%w0, %1";
  }
)

(define_insn "@aarch64_load_exclusive<mode>"
  [(set (match_operand:GPIC 0 "register_operand" "=r")
    (unspec_volatile:GPIC
      [(match_operand:GPIC 1 "aarch64_sync_memory_operand" "Q")
       (match_operand:SI 2 "const_int_operand")]
      UNSPECV_LX))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[2]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_release (model))
      return "ldxr\t%<w>0, %1";
    else
      return "ldaxr\t%<w>0, %1";
  }
)

(define_insn "aarch64_load_exclusive_pair"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec_volatile:DI
	  [(match_operand:TI 2 "aarch64_sync_memory_operand" "Q")
	   (match_operand:SI 3 "const_int_operand")]
	  UNSPECV_LX))
   (set (match_operand:DI 1 "register_operand" "=r")
	(unspec_volatile:DI [(match_dup 2) (match_dup 3)] UNSPECV_LX))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_release (model))
      return "ldxp\t%0, %1, %2";
    else
      return "ldaxp\t%0, %1, %2";
  }
)

(define_insn "@aarch64_store_exclusive<mode>"
  [(set (match_operand:SI 0 "register_operand" "=&r")
    (unspec_volatile:SI [(const_int 0)] UNSPECV_SX))
   (set (match_operand:ALLIC 1 "aarch64_sync_memory_operand" "=Q")
    (unspec_volatile:ALLIC
      [(match_operand:ALLIC 2 "aarch64_reg_or_zero" "rZ")
       (match_operand:SI 3 "const_int_operand")]
      UNSPECV_SX))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[3]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_acquire (model))
      return "stxr<atomic_sfx>\t%w0, %<w>2, %1";
    else
      return "stlxr<atomic_sfx>\t%w0, %<w>2, %1";
  }
)

(define_insn "aarch64_store_exclusive_pair"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(unspec_volatile:SI [(const_int 0)] UNSPECV_SX))
   (set (match_operand:TI 1 "aarch64_sync_memory_operand" "=Q")
	(unspec_volatile:TI
	  [(match_operand:DI 2 "aarch64_reg_or_zero" "rZ")
	   (match_operand:DI 3 "aarch64_reg_or_zero" "rZ")
	   (match_operand:SI 4 "const_int_operand")]
	  UNSPECV_SX))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[4]));
    if (is_mm_relaxed (model) || is_mm_consume (model) || is_mm_acquire (model))
      return "stxp\t%w0, %x2, %x3, %1";
    else
      return "stlxp\t%w0, %x2, %x3, %1";
  }
)

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand")]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[0]));
    if (!(is_mm_relaxed (model) || is_mm_consume (model)))
      emit_insn (gen_dmb (operands[0]));
    DONE;
  }
)

(define_expand "dmb"
  [(set (match_dup 1)
    (unspec:BLK [(match_dup 1) (match_operand:SI 0 "const_int_operand")]
     UNSPEC_MB))]
   ""
   {
    operands[1] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
    MEM_VOLATILE_P (operands[1]) = 1;
  }
)

(define_insn "*dmb"
  [(set (match_operand:BLK 0 "" "")
    (unspec:BLK [(match_dup 0) (match_operand:SI 1 "const_int_operand")]
     UNSPEC_MB))]
  ""
  {
    enum memmodel model = memmodel_from_int (INTVAL (operands[1]));
    if (is_mm_acquire (model))
      return "dmb\\tishld";
    else
      return "dmb\\tish";
  }
)
