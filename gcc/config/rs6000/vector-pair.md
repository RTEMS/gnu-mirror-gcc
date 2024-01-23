;; Vector pair arithmetic support.
;; Copyright (C) 2020-2023 Free Software Foundation, Inc.
;; Contributed by Peter Bergner <bergner@linux.ibm.com> and
;;		  Michael Meissner <meissner@linux.ibm.com>
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.
;;
;; This file adds support for doing vector operations on pairs of vector
;; registers.  Most of the instructions use vector pair instructions to load
;; and possibly store registers, but splitting the operation after register
;; allocation to do 2 separate operations.  The second scheduler pass can
;; interleave other instructions between these pairs of instructions if
;; possible.

;; We use UNSPEC to identify the representation for the operation rather than
;; SUBREG, because SUBREG tends to generate extra moves.

(define_c_enum "unspec"
  [UNSPEC_VPAIR_ABS
   UNSPEC_VPAIR_DIV
   UNSPEC_VPAIR_FMA
   UNSPEC_VPAIR_MINUS
   UNSPEC_VPAIR_MULT
   UNSPEC_VPAIR_NEG
   UNSPEC_VPAIR_PLUS
   UNSPEC_VPAIR_SMAX
   UNSPEC_VPAIR_SMIN
   UNSPEC_VPAIR_ZERO
   UNSPEC_VPAIR_SPLAT])

;; Vector pair element ID that defines the scaler element within the vector pair.
(define_c_enum "vpair_element"
  [VPAIR_ELEMENT_FLOAT
   VPAIR_ELEMENT_DOUBLE])

(define_int_iterator VPAIR_FP_ELEMENT [VPAIR_ELEMENT_FLOAT
				       VPAIR_ELEMENT_DOUBLE])

;; Map vector pair element ID to the vector mode after the vector pair has been
;; split.
(define_int_attr VPAIR_VMODE [(VPAIR_ELEMENT_FLOAT  "V4SF")
			      (VPAIR_ELEMENT_DOUBLE "V2DF")])

;; Map vector pair element ID to the name used on the define_insn (in lower
;; case).
(define_int_attr vpair_modename [(VPAIR_ELEMENT_FLOAT  "v8sf")
				 (VPAIR_ELEMENT_DOUBLE "v4df")])

;; Unary/binary arithmetic iterator on vector pairs.
(define_int_iterator VPAIR_FP_UNARY  [UNSPEC_VPAIR_ABS
				      UNSPEC_VPAIR_NEG])

(define_int_iterator VPAIR_FP_BINARY [UNSPEC_VPAIR_DIV
				      UNSPEC_VPAIR_MINUS
				      UNSPEC_VPAIR_MULT
				      UNSPEC_VPAIR_PLUS
				      UNSPEC_VPAIR_SMAX
				      UNSPEC_VPAIR_SMIN])

;; Map the vpair operator unspec number to the standard name.
(define_int_attr vpair_stdname [(UNSPEC_VPAIR_ABS    "abs")
				(UNSPEC_VPAIR_DIV    "div")
				(UNSPEC_VPAIR_FMA    "fma")
				(UNSPEC_VPAIR_MINUS  "sub")
				(UNSPEC_VPAIR_MULT   "mul")
				(UNSPEC_VPAIR_NEG    "neg")
				(UNSPEC_VPAIR_PLUS   "add")
				(UNSPEC_VPAIR_SMAX   "smax")
				(UNSPEC_VPAIR_SMIN   "smin")])

;; Map the vpair operator unspec number to the RTL operator.
(define_int_attr VPAIR_OP [(UNSPEC_VPAIR_ABS    "ABS")
			   (UNSPEC_VPAIR_DIV    "DIV")
			   (UNSPEC_VPAIR_FMA    "FMA")
			   (UNSPEC_VPAIR_MINUS  "MINUS")
			   (UNSPEC_VPAIR_MULT   "MULT")
			   (UNSPEC_VPAIR_NEG    "NEG")
			   (UNSPEC_VPAIR_PLUS   "PLUS")
			   (UNSPEC_VPAIR_SMAX   "SMAX")
			   (UNSPEC_VPAIR_SMIN   "SMIN")])

;; Map the scalar element ID into the appropriate insn type.
(define_int_attr vpair_type [(VPAIR_ELEMENT_FLOAT  "vecfloat")
			     (VPAIR_ELEMENT_DOUBLE "vecdouble")])

;; Map the scalar element ID into the appropriate insn type for divide.
(define_int_attr vpair_divtype [(VPAIR_ELEMENT_FLOAT  "vecfdiv")
				(VPAIR_ELEMENT_DOUBLE "vecdiv")])

;; Mode iterator for the vector modes that we provide splat operations for.
(define_mode_iterator VPAIR_SPLAT_VMODE [V4SF V2DF])

;; Map element mode to 128-bit vector mode for splat operations
(define_mode_attr VPAIR_SPLAT_ELEMENT_TO_VMODE [(SF "V4SF")
						(DF "V2DF")])

;; Map either element mode or vector mode into the name for the splat insn.
(define_mode_attr vpair_splat_name [(SF   "v8sf")
				    (DF   "v4df")
				    (V4SF "v8sf")
				    (V2DF "v4df")])

;; Initialize a vector pair to 0
(define_insn_and_split "vpair_zero"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(const_int 0)] UNSPEC_VPAIR_ZERO))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 1) (match_dup 3))
   (set (match_dup 2) (match_dup 3))]
{
  rtx op0 = operands[0];

  operands[1] = simplify_gen_subreg (V2DFmode, op0, OOmode, 0);
  operands[2] = simplify_gen_subreg (V2DFmode, op0, OOmode, 16);
  operands[3] = CONST0_RTX (V2DFmode);
}
  [(set_attr "length" "8")
   (set_attr "type" "vecperm")])

;; Create a vector pair with a value splat'ed (duplicated) to all of the
;; elements.
(define_expand "vpair_splat_<vpair_splat_name>"
  [(use (match_operand:OO 0 "vsx_register_operand"))
   (use (match_operand:SFDF 1 "input_operand"))]
  "TARGET_MMA"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  machine_mode element_mode = <MODE>mode;

  if (op1 == CONST0_RTX (element_mode))
    {
      emit_insn (gen_vpair_zero (op0));
      DONE;
    }

  machine_mode vector_mode = <VPAIR_SPLAT_ELEMENT_TO_VMODE>mode;
  rtx vec = gen_reg_rtx (vector_mode);
  unsigned num_elements = GET_MODE_NUNITS (vector_mode);
  rtvec elements = rtvec_alloc (num_elements);
  for (size_t i = 0; i < num_elements; i++)
    RTVEC_ELT (elements, i) = copy_rtx (op1);

  rs6000_expand_vector_init (vec, gen_rtx_PARALLEL (vector_mode, elements));
  emit_insn (gen_vpair_splat_<vpair_splat_name>_internal (op0, vec));
  DONE;
})

;; Inner splat support.  Operand1 is the vector splat created above.  Allow
;; operand 1 to overlap with the output registers to eliminate one move
;; instruction.
(define_insn_and_split "vpair_splat_<vpair_splat_name>_internal"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(match_operand:VPAIR_SPLAT_VMODE 1 "vsx_register_operand" "0,wa")]
	 UNSPEC_VPAIR_SPLAT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op0 = operands[0];
  rtx op0_a = simplify_gen_subreg (<MODE>mode, op0, OOmode, 0);
  rtx op0_b = simplify_gen_subreg (<MODE>mode, op0, OOmode, 16);
  rtx op1 = operands[1];
  unsigned op1_regno = reg_or_subregno (op1);

  /* Check if the input is one of the output registers.  */
  if (op1_regno == reg_or_subregno (op0_a))
    emit_move_insn (op0_b, op1);

  else if (op1_regno == reg_or_subregno (op0_b))
    emit_move_insn (op0_a, op1);

  else
    {
      emit_move_insn (op0_a, op1);
      emit_move_insn (op0_b, op1);
    }

  DONE;
}
  [(set_attr "length" "*,8")
   (set_attr "type" "vecmove")])

;; Vector pair unary operations.  The last argument in the UNSPEC is a
;; CONST_INT which identifies what the scalar element is.
(define_insn_and_split "vpair_<vpair_stdname>_<vpair_modename>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "wa")
	  (const_int VPAIR_FP_ELEMENT)]
	 VPAIR_FP_UNARY))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_unary (operands, <VPAIR_VMODE>mode, <VPAIR_OP>,
		     VPAIR_SPLIT_NORMAL);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Optimize vector pair (neg (abs)).
(define_insn_and_split "vpair_nabs_<vpair_modename>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "wa")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_ABS)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_NEG))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_unary (operands, <VPAIR_VMODE>mode, ABS, VPAIR_SPLIT_NEGATE);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Vector pair binary operations.  The last argument in the UNSPEC is a
;; CONST_INT which identifies what the scalar element is.
(define_insn_and_split "vpair_<vpair_stdname>_<vpair_modename>3"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "wa")
	  (match_operand:OO 2 "vsx_register_operand" "wa")
	  (const_int VPAIR_FP_ELEMENT)]
	 VPAIR_FP_BINARY))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_binary (operands, <VPAIR_VMODE>mode, <VPAIR_OP>);
  DONE;
}
  [(set_attr "length" "8")
   (set (attr "type") (if_then_else (match_test "<VPAIR_OP> == DIV")
				    (const_string "<vpair_divtype>")
				    (const_string "<vpair_type>")))])

;; Optimize vector pair add of a negative value into a subtract.
(define_insn_and_split "*vpair_add_neg_<vpair_modename>3"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "wa")
	  (unspec:OO
	   [(match_operand:OO 2 "vsx_register_operand" "wa")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_NEG)
	  (const_int VPAIR_FP_ELEMENT)]
	 VPAIR_FP_BINARY))]
  "TARGET_MMA"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(match_dup 1)
	  (match_dup 2)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_MINUS))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Vector pair fused-multiply (FMA) operations.  The last argument in the
;; UNSPEC is a CONST_INT which identifies what the scalar element is.
(define_insn_and_split "vpair_fma_<vpair_modename>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:OO 2 "vsx_register_operand" "wa,0")
	  (match_operand:OO 3 "vsx_register_operand" "0,wa")
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_FMA))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_fma (operands, <VPAIR_VMODE>mode, VPAIR_SPLIT_FMA);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Vector pair fused multiply-subtract
(define_insn_and_split "vpair_fms_<vpair_modename>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:OO 2 "vsx_register_operand" "wa,0")
	  (unspec:OO
	   [(match_operand:OO 3 "vsx_register_operand" "0,wa")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_NEG)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_FMA))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_fma (operands, <VPAIR_VMODE>mode, VPAIR_SPLIT_FMS);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Vector pair negate fused multiply-add
(define_insn_and_split "vpair_nfma_<vpair_modename>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	    (match_operand:OO 2 "vsx_register_operand" "wa,0")
	    (match_operand:OO 3 "vsx_register_operand" "0,wa")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_FMA)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_NEG))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_fma (operands, <VPAIR_VMODE>mode, VPAIR_SPLIT_NFMA);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Vector pair fused multiply-subtract
(define_insn_and_split "vpair_nfms_<vpair_modename>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	    (match_operand:OO 2 "vsx_register_operand" "wa,0")
	    (unspec:OO
	     [(match_operand:OO 3 "vsx_register_operand" "0,wa")
	      (const_int VPAIR_FP_ELEMENT)]
	     UNSPEC_VPAIR_NEG)
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_FMA)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_NEG))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_fma (operands, <VPAIR_VMODE>mode, VPAIR_SPLIT_NFMS);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Optimize vector pair multiply and vector pair add into vector pair fma,
;; providing the compiler would do this optimization for scalar and vectors.
;; Unlike most of the define_insn_and_splits, this can be done before register
;; allocation.
(define_insn_and_split "*vpair_fma_<vpair_modename>_merge"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	    (match_operand:OO 2 "vsx_register_operand" "wa,0")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_MULT)
	  (match_operand:OO 3 "vsx_register_operand" "0,wa")
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_PLUS))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(match_dup 1)
	  (match_dup 2)
	  (match_dup 3)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_FMA))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Merge multiply and subtract.
(define_insn_and_split "*vpair_fma_<vpair_modename>_merge"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	    (match_operand:OO 2 "vsx_register_operand" "wa,0")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_MULT)
	  (match_operand:OO 3 "vsx_register_operand" "0,wa")
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_MINUS))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(match_dup 1)
	  (match_dup 2)
	  (unspec:OO
	   [(match_dup 3)
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_NEG)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_FMA))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

(define_insn_and_split "*vpair_fma_<vpair_modename>_merge2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	    (match_operand:OO 2 "vsx_register_operand" "wa,0")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_MULT)
	  (unspec:OO
	   [(match_operand:OO 3 "vsx_register_operand" "0,wa")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_NEG)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_PLUS))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(match_dup 1)
	  (match_dup 2)
	  (unspec:OO
	   [(match_dup 3)
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_NEG)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_FMA))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Merge negate, multiply, and add.
(define_insn_and_split "*vpair_nfma_<vpair_modename>_merge"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(unspec:OO
	     [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")
	      (const_int VPAIR_FP_ELEMENT)]
	     UNSPEC_VPAIR_MULT)
	    (match_operand:OO 3 "vsx_register_operand" "0,wa")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_PLUS)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_NEG))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(unspec:OO
	   [(match_dup 1)
	    (match_dup 2)
	    (match_dup 3)
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_FMA)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_NEG))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Merge negate, multiply, and subtract.
(define_insn_and_split "*vpair_nfms_<vpair_modename>_merge"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(unspec:OO
	     [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")
	      (const_int VPAIR_FP_ELEMENT)]
	     UNSPEC_VPAIR_MULT)
	    (match_operand:OO 3 "vsx_register_operand" "0,wa")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_MINUS)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_NEG))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(unspec:OO
	   [(match_dup 1)
	    (match_dup 2)
	    (unspec:OO
	     [(match_dup 3)
	      (const_int VPAIR_FP_ELEMENT)]
	     UNSPEC_VPAIR_NEG)
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_FMA)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_NEG))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

(define_insn_and_split "*vpair_nfms_<vpair_modename>_merge2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(unspec:OO
	     [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")
	      (const_int VPAIR_FP_ELEMENT)]
	     UNSPEC_VPAIR_MULT)
	    (unspec:OO
	     [(match_operand:OO 3 "vsx_register_operand" "0,wa")
	      (const_int VPAIR_FP_ELEMENT)]
	     UNSPEC_VPAIR_NEG)
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_PLUS)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_NEG))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(unspec:OO
	   [(match_dup 1)
	    (match_dup 2)
	    (unspec:OO
	     [(match_dup 3)
	      (const_int VPAIR_FP_ELEMENT)]
	     UNSPEC_VPAIR_NEG)
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_FMA)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_NEG))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])
