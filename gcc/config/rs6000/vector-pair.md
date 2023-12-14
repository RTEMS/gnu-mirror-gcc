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
  [UNSPEC_VPAIR_ZERO
   UNSPEC_VPAIR_ASSEMBLE
   UNSPEC_VPAIR_SPLAT
   UNSPEC_VPAIR_EXTRACT_VECTOR
   UNSPEC_VPAIR_ABS_V4DF
   UNSPEC_VPAIR_ABS_V8SF
   UNSPEC_VPAIR_FMA_V4DF
   UNSPEC_VPAIR_FMA_V8SF
   UNSPEC_VPAIR_MINUS_V4DF
   UNSPEC_VPAIR_MINUS_V8SF
   UNSPEC_VPAIR_MULT_V4DF
   UNSPEC_VPAIR_MULT_V8SF
   UNSPEC_VPAIR_NEG_V4DF
   UNSPEC_VPAIR_NEG_V8SF
   UNSPEC_VPAIR_PLUS_V4DF
   UNSPEC_VPAIR_PLUS_V8SF
   UNSPEC_VPAIR_SMAX_V4DF
   UNSPEC_VPAIR_SMAX_V8SF
   UNSPEC_VPAIR_SMIN_V4DF
   UNSPEC_VPAIR_SMIN_V8SF])

;; Unary/binary arithmetic iterator on vector pairs.
(define_int_iterator VPAIR_FP_UNARY  [UNSPEC_VPAIR_ABS_V4DF
				      UNSPEC_VPAIR_ABS_V8SF
				      UNSPEC_VPAIR_NEG_V4DF
				      UNSPEC_VPAIR_NEG_V8SF])

(define_int_iterator VPAIR_FP_BINARY [UNSPEC_VPAIR_MINUS_V4DF
				      UNSPEC_VPAIR_MINUS_V8SF
				      UNSPEC_VPAIR_MULT_V4DF
				      UNSPEC_VPAIR_MULT_V8SF
				      UNSPEC_VPAIR_PLUS_V4DF
				      UNSPEC_VPAIR_PLUS_V8SF
				      UNSPEC_VPAIR_SMAX_V4DF
				      UNSPEC_VPAIR_SMAX_V8SF
				      UNSPEC_VPAIR_SMIN_V4DF
				      UNSPEC_VPAIR_SMIN_V8SF])

(define_int_iterator VPAIR_FP_FMA [UNSPEC_VPAIR_FMA_V4DF
				   UNSPEC_VPAIR_FMA_V8SF])

;; Map vector pair operator to the vector mode
(define_int_attr VPAIR_VEC_MODE [(UNSPEC_VPAIR_ABS_V4DF	  "V2DF")
				 (UNSPEC_VPAIR_ABS_V8SF	  "V4SF")
				 (UNSPEC_VPAIR_FMA_V4DF   "V2DF")
				 (UNSPEC_VPAIR_FMA_V8SF   "V4SF")
				 (UNSPEC_VPAIR_MINUS_V4DF "V2DF")
				 (UNSPEC_VPAIR_MINUS_V8SF "V4SF")
				 (UNSPEC_VPAIR_MULT_V4DF  "V2DF")
				 (UNSPEC_VPAIR_MULT_V8SF  "V4SF")
				 (UNSPEC_VPAIR_NEG_V4DF   "V2DF")
				 (UNSPEC_VPAIR_NEG_V8SF   "V4SF")
				 (UNSPEC_VPAIR_PLUS_V4DF  "V2DF")
				 (UNSPEC_VPAIR_PLUS_V8SF  "V4SF")
				 (UNSPEC_VPAIR_SMAX_V4DF  "V2DF")
				 (UNSPEC_VPAIR_SMAX_V8SF  "V4SF")
				 (UNSPEC_VPAIR_SMIN_V4DF  "V2DF")
				 (UNSPEC_VPAIR_SMIN_V8SF  "V4SF")])

;; Map vector pair operator to the lower case vector mode that fits in the
;; vector pair
(define_int_attr vpair_vp_mode_lc [(UNSPEC_VPAIR_ABS_V4DF   "v4df")
				   (UNSPEC_VPAIR_ABS_V8SF   "v8sf")
				   (UNSPEC_VPAIR_FMA_V4DF   "v4df")
				   (UNSPEC_VPAIR_FMA_V8SF   "v8sf")
				   (UNSPEC_VPAIR_MINUS_V4DF "v4df")
				   (UNSPEC_VPAIR_MINUS_V8SF "v8sf")
				   (UNSPEC_VPAIR_MULT_V4DF  "v4df")
				   (UNSPEC_VPAIR_MULT_V8SF  "v8sf")
				   (UNSPEC_VPAIR_NEG_V4DF   "v4df")
				   (UNSPEC_VPAIR_NEG_V8SF   "v8sf")
				   (UNSPEC_VPAIR_PLUS_V4DF  "v4df")
				   (UNSPEC_VPAIR_PLUS_V8SF  "v8sf")
				   (UNSPEC_VPAIR_SMAX_V4DF  "v4df")
				   (UNSPEC_VPAIR_SMAX_V8SF  "v8sf")
				   (UNSPEC_VPAIR_SMIN_V4DF  "v4df")
				   (UNSPEC_VPAIR_SMIN_V8SF  "v8sf")])

;; Map vector pair operator to the type attribute
(define_int_attr vpair_type [(UNSPEC_VPAIR_ABS_V4DF   "vecdouble")
			     (UNSPEC_VPAIR_ABS_V8SF   "vecfloat")
			     (UNSPEC_VPAIR_FMA_V4DF   "vecdouble")
			     (UNSPEC_VPAIR_FMA_V8SF   "vecfloat")
			     (UNSPEC_VPAIR_MINUS_V4DF "vecdouble")
			     (UNSPEC_VPAIR_MINUS_V8SF "vecfloat")
			     (UNSPEC_VPAIR_MULT_V4DF  "vecdouble")
			     (UNSPEC_VPAIR_MULT_V8SF  "vecfloat")
			     (UNSPEC_VPAIR_NEG_V4DF   "vecdouble")
			     (UNSPEC_VPAIR_NEG_V8SF   "vecfloat")
			     (UNSPEC_VPAIR_PLUS_V4DF  "vecdouble")
			     (UNSPEC_VPAIR_PLUS_V8SF  "vecfloat")
			     (UNSPEC_VPAIR_SMAX_V4DF  "vecdouble")
			     (UNSPEC_VPAIR_SMAX_V8SF  "vecfloat")
			     (UNSPEC_VPAIR_SMIN_V4DF  "vecdouble")
			     (UNSPEC_VPAIR_SMIN_V8SF  "vecfloat")])

;; Map vector pair operator to the operation
(define_int_attr VPAIR_OP [(UNSPEC_VPAIR_ABS_V4DF   "ABS")
			   (UNSPEC_VPAIR_ABS_V8SF   "ABS")
			   (UNSPEC_VPAIR_FMA_V4DF   "FMA")
			   (UNSPEC_VPAIR_FMA_V8SF   "FMA")
			   (UNSPEC_VPAIR_MINUS_V4DF "MINUS")
			   (UNSPEC_VPAIR_MINUS_V8SF "MINUS")
			   (UNSPEC_VPAIR_MULT_V4DF  "MULT")
			   (UNSPEC_VPAIR_MULT_V8SF  "MULT")
			   (UNSPEC_VPAIR_NEG_V4DF   "NEG")
			   (UNSPEC_VPAIR_NEG_V8SF   "NEG")
			   (UNSPEC_VPAIR_PLUS_V4DF  "PLUS")
			   (UNSPEC_VPAIR_PLUS_V8SF  "PLUS")
			   (UNSPEC_VPAIR_SMAX_V4DF  "SMAX")
			   (UNSPEC_VPAIR_SMAX_V8SF  "SMAX")
			   (UNSPEC_VPAIR_SMIN_V4DF  "SMIN")
			   (UNSPEC_VPAIR_SMIN_V8SF  "SMIN")])

;; Map vector pair operator to the standard name
(define_int_attr vpair_stdname [(UNSPEC_VPAIR_ABS_V4DF   "abs")
				(UNSPEC_VPAIR_ABS_V8SF   "abs")
				(UNSPEC_VPAIR_FMA_V4DF   "fma")
				(UNSPEC_VPAIR_FMA_V8SF   "fma")
				(UNSPEC_VPAIR_MINUS_V4DF "sub")
				(UNSPEC_VPAIR_MINUS_V8SF "sub")
				(UNSPEC_VPAIR_MULT_V4DF  "mul")
				(UNSPEC_VPAIR_MULT_V8SF  "mul")
				(UNSPEC_VPAIR_NEG_V4DF   "neg")
				(UNSPEC_VPAIR_NEG_V8SF   "neg")
				(UNSPEC_VPAIR_PLUS_V4DF  "add")
				(UNSPEC_VPAIR_PLUS_V8SF  "add")
				(UNSPEC_VPAIR_SMAX_V4DF  "smax")
				(UNSPEC_VPAIR_SMAX_V8SF  "smax")
				(UNSPEC_VPAIR_SMIN_V4DF  "smin")
				(UNSPEC_VPAIR_SMIN_V8SF  "smin")])

;; Vector pair absolute value operators
(define_int_iterator VPAIR_FP_ABS [UNSPEC_VPAIR_ABS_V4DF
				   UNSPEC_VPAIR_ABS_V8SF])

;; Vector pair negate operations that pair with absolute value
(define_int_attr VPAIR_FP_ABS_NEG
		 [(UNSPEC_VPAIR_ABS_V4DF "UNSPEC_VPAIR_NEG_V4DF")
		  (UNSPEC_VPAIR_ABS_V8SF "UNSPEC_VPAIR_NEG_V8SF")])

;; Vector pair plus operators
(define_int_iterator VPAIR_FP_PLUS [UNSPEC_VPAIR_PLUS_V4DF
				    UNSPEC_VPAIR_PLUS_V8SF])

;; Vector pair negate operations that pair with addition to become subtraction.
(define_int_attr VPAIR_FP_PLUS_NEG
		 [(UNSPEC_VPAIR_PLUS_V4DF "UNSPEC_VPAIR_NEG_V4DF")
		  (UNSPEC_VPAIR_PLUS_V8SF "UNSPEC_VPAIR_NEG_V8SF")])

;; Vector pair minus operation that is formed from plus of a negative value.
(define_int_attr VPAIR_FP_PLUS_TO_MINUS
		 [(UNSPEC_VPAIR_PLUS_V4DF "UNSPEC_VPAIR_MINUS_V4DF")
		  (UNSPEC_VPAIR_PLUS_V8SF "UNSPEC_VPAIR_MINUS_V8SF")])

;; Vector pair negate operations that pair with fma
(define_int_attr VPAIR_FP_FMA_NEG
		 [(UNSPEC_VPAIR_FMA_V4DF "UNSPEC_VPAIR_NEG_V4DF")
		  (UNSPEC_VPAIR_FMA_V8SF "UNSPEC_VPAIR_NEG_V8SF")])

;; Vector pair multiply operators
(define_int_iterator VPAIR_FP_MULT [UNSPEC_VPAIR_MULT_V4DF
				    UNSPEC_VPAIR_MULT_V8SF])

;; Vector pair add operators that merge with multiply to make FMA.
(define_int_attr VPAIR_FP_MULT_PLUS
		 [(UNSPEC_VPAIR_MULT_V4DF "UNSPEC_VPAIR_PLUS_V4DF")
		  (UNSPEC_VPAIR_MULT_V8SF "UNSPEC_VPAIR_PLUS_V8SF")])

;; Vector pair FMA operation after merging multiply and add/subtract
(define_int_attr VPAIR_FP_MULT_TO_FMA
		 [(UNSPEC_VPAIR_MULT_V4DF "UNSPEC_VPAIR_FMA_V4DF")
		  (UNSPEC_VPAIR_MULT_V8SF "UNSPEC_VPAIR_FMA_V8SF")])

;; Vector pair subtract operators that merge with multiply to make FMA.
(define_int_attr VPAIR_FP_MULT_MINUS
		 [(UNSPEC_VPAIR_MULT_V4DF "UNSPEC_VPAIR_MINUS_V4DF")
		  (UNSPEC_VPAIR_MULT_V8SF "UNSPEC_VPAIR_MINUS_V8SF")])

;; Vector pair negate operators that merge with multiply to make FMA.
(define_int_attr VPAIR_FP_MULT_NEG
		 [(UNSPEC_VPAIR_MULT_V4DF "UNSPEC_VPAIR_NEG_V4DF")
		  (UNSPEC_VPAIR_MULT_V8SF "UNSPEC_VPAIR_NEG_V8SF")])

;; Vector pair NEG operation after merging multiply and subtract
(define_int_attr VPAIR_FP_MULT_TO_NEG
		 [(UNSPEC_VPAIR_MULT_V4DF "UNSPEC_VPAIR_NEG_V4DF")
		  (UNSPEC_VPAIR_MULT_V8SF "UNSPEC_VPAIR_NEG_V8SF")])

;; Iterator for base element for supported vector pair operations.
(define_mode_iterator VPAIR_ELEMENT [DF SF])

;; Iterator for vector element for supported vector pair operations.
(define_mode_iterator VPAIR_VECTOR [V2DF V4SF])

;; Map element mode to vector mode
(define_mode_attr VPAIR_ELE_TO_VEC [(DF "V2DF")
				    (SF "V4SF")])

;; Map element mode to vector pair mode in lower case
(define_mode_attr vpair_ele_to_vpair_lc [(DF "v4df")
					 (SF "v8sf")])

;; Map vector mode to vector pair mode in lower case
(define_mode_attr vpair_vec_to_vpair_lc [(V2DF "v4df")
					 (V4SF "v8sf")])


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

  operands[1] = simplify_gen_subreg (V2DImode, op0, OOmode, 0);
  operands[2] = simplify_gen_subreg (V2DImode, op0, OOmode, 16);
  operands[3] = CONST0_RTX (V2DImode);
}
  [(set_attr "length" "8")
   (set_attr "type" "vecperm")])

;; Assemble a vector pair from two vectors.  Unlike
;; __builtin_mma_assemble_pair, this function produces a vector pair output
;; directly and uses the vectors as inputs, instead of taking an array.  The
;; first argument is the high vector (which on little endian systems is stored
;; in the second vector of the vector pair).  We have two variants, based on
;; the endian to favor getting one of the results in the correct register.
;;
;; We issue a clobber of the vector pair so this insn can be split before
;; register allocation.

(define_insn_and_split "vpair_assemble_<vpair_vec_to_vpair_lc>"
  [(set (match_operand:OO 0 "vsx_register_operand" "=&wa")
	(unspec:OO
	 [(match_operand:VPAIR_VECTOR 1 "input_operand" "mwaeP")
	  (match_operand:VPAIR_VECTOR 2 "input_operand" "mwaeP")]
	 UNSPEC_VPAIR_ASSEMBLE))]
  "TARGET_MMA"
  "#"
  "&& 1"
  [(clobber (match_dup 0))
   (set (match_dup 3) (match_dup 1))
   (set (match_dup 4) (match_dup 2))]
{
  rtx dest = operands[0];
  unsigned hi_offset = (WORDS_BIG_ENDIAN) ? 0 : 16;
  unsigned lo_offset = 16 - hi_offset;

  operands[3] = simplify_gen_subreg (<MODE>mode, dest, OOmode, hi_offset);
  operands[4] = simplify_gen_subreg (<MODE>mode, dest, OOmode, lo_offset);
}
  [(set_attr "length" "8")])

;; Create a vector pair with a value splat'ed (duplicated) to all of the
;; elements.
(define_expand "vpair_splat_<vpair_ele_to_vpair_lc>"
  [(use (match_operand:OO 0 "vsx_register_operand"))
   (use (match_operand:VPAIR_ELEMENT 1 "input_operand"))]
  "TARGET_MMA"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  machine_mode element_mode = <MODE>mode;
  machine_mode vector_mode = <VPAIR_ELE_TO_VEC>mode;

  if (op1 == CONST0_RTX (element_mode))
    {
      emit_insn (gen_vpair_zero (op0));
      DONE;
    }

  rtx vec = gen_reg_rtx (vector_mode);
  unsigned num_elements = GET_MODE_NUNITS (vector_mode);
  rtvec elements = rtvec_alloc (num_elements);
  for (size_t i = 0; i < num_elements; i++)
    RTVEC_ELT (elements, i) = copy_rtx (op1);

  rs6000_expand_vector_init (vec, gen_rtx_PARALLEL (vector_mode, elements));
  emit_insn (gen_vpair_splat_<vpair_ele_to_vpair_lc>_internal (op0, vec));
  DONE;
})

;; Inner splat support.  Operand1 is the vector splat created above.  Allow
;; operand 1 to overlap with the output registers to eliminate one move
;; instruction.
(define_insn_and_split "vpair_splat_<vpair_vec_to_vpair_lc>_internal"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(match_operand:VPAIR_VECTOR 1 "vsx_register_operand" "0,wa")]
	 UNSPEC_VPAIR_SPLAT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op0_a = simplify_gen_subreg (<MODE>mode, op0, OOmode, 0);
  rtx op0_b = simplify_gen_subreg (<MODE>mode, op0, OOmode, 16);

  /* Check if the input is one of the output registers.  */
  if (rtx_equal_p (op0_a, op1))
    emit_move_insn (op0_b, op1);

  else if (rtx_equal_p (op0_b, op1))
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

;; Extract one of the two 128-bit vectors from a vector pair.
(define_insn_and_split "vpair_extract_vector_<vpair_vec_to_vpair_lc>"
  [(set (match_operand:VPAIR_VECTOR 0 "vsx_register_operand" "=wa")
	(unspec:VPAIR_VECTOR
	 [(match_operand:OO 1 "vsx_register_operand" "wa")
	  (match_operand 2 "const_0_to_1_operand" "n")]
	 UNSPEC_VPAIR_EXTRACT_VECTOR))]
  "TARGET_MMA"
  "#"
  "&& 1"
  [(set (match_dup 0) (match_dup 3))]
{
  rtx src = operands[1];
  machine_mode mode = <MODE>mode;
  unsigned reg_num = UINTVAL (operands[2]);
  if (!WORDS_BIG_ENDIAN)
    reg_num = 1 - reg_num;
	   
  operands[3] = simplify_gen_subreg (mode, src, OOmode, reg_num * 16);
}
  [(set_attr "type" "vecmove")])

;; Vector pair floating point unary operations
(define_insn_and_split "vpair_<vpair_stdname>_<vpair_vp_mode_lc>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "wa")]
	 VPAIR_FP_UNARY))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_unary (operands, <VPAIR_VEC_MODE>mode, <VPAIR_OP>,
		     VPAIR_SPLIT_NO_EXTRA_ACTION);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])
	 
;; Combine neg and abs for vector pair
(define_insn_and_split "*vpair_nabs_<vpair_vp_mode_lc>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "wa")]
	   VPAIR_FP_ABS)]
	 <VPAIR_FP_ABS_NEG>))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_unary (operands, <VPAIR_VEC_MODE>mode, ABS, VPAIR_SPLIT_NABS);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Vector pair floating point binary operations
(define_insn_and_split "vpair_<vpair_stdname>_<vpair_vp_mode_lc>3"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "wa")
	  (match_operand:OO 2 "vsx_register_operand" "wa")]
	 VPAIR_FP_BINARY))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_binary (operands, <VPAIR_VEC_MODE>mode, <VPAIR_OP>);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Optimize vector pair add and negation into minus
(define_insn_and_split "*vpair_plus_negate_<vpair_vp_mode_lc>3"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "wa")
	  (unspec:OO
	   [(match_operand:OO 2 "vsx_register_operand" "wa")]
	   <VPAIR_FP_PLUS_NEG>)]
	 VPAIR_FP_PLUS))]
  "TARGET_MMA"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(match_dup 1)
	  (match_dup 2)]
	 <VPAIR_FP_PLUS_TO_MINUS>))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Vector pair floating point FMA operations
(define_insn_and_split "vpair_fma_<vpair_vp_mode_lc>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:OO 2 "vsx_register_operand" "wa,0")
	  (match_operand:OO 3 "vsx_register_operand" "0,wa")]
	 VPAIR_FP_FMA))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_fma (operands, <VPAIR_VEC_MODE>mode, FMA, VPAIR_SPLIT_FMA);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

(define_insn_and_split "*vpair_fms_<vpair_vp_mode_lc>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:OO 2 "vsx_register_operand" "wa,0")
	  (unspec:OO
	   [(match_operand:OO 3 "vsx_register_operand" "0,wa")]
	   <VPAIR_FP_FMA_NEG>)]
	 VPAIR_FP_FMA))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_fma (operands, <VPAIR_VEC_MODE>mode, FMA, VPAIR_SPLIT_FMS);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

(define_insn_and_split "*vpair_nfma_<vpair_vp_mode_lc>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	    (match_operand:OO 2 "vsx_register_operand" "wa,0")
	    (match_operand:OO 3 "vsx_register_operand" "0,wa")]
	   VPAIR_FP_FMA)]
	 <VPAIR_FP_FMA_NEG>))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_fma (operands, <VPAIR_VEC_MODE>mode, FMA, VPAIR_SPLIT_NFMA);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

(define_insn_and_split "*vpair_nfms_<vpair_vp_mode_lc>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	    (match_operand:OO 2 "vsx_register_operand" "wa,0")
	    (unspec:OO
	     [(match_operand:OO 3 "vsx_register_operand" "0,wa")]
	     <VPAIR_FP_FMA_NEG>)]
	   VPAIR_FP_FMA)]
	 <VPAIR_FP_FMA_NEG>))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_fma (operands, <VPAIR_VEC_MODE>mode, FMA, VPAIR_SPLIT_NFMS);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Optimize vector pair (a * b) + c into vector pair fma (a, b, c).
(define_insn_and_split "*vpair_fma_merge_<vpair_vp_mode_lc>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	    (match_operand:OO 2 "vsx_register_operand" "wa,0")]
	   VPAIR_FP_MULT)
	  (match_operand:OO 3 "vsx_register_operand" "0,wa")]
	 <VPAIR_FP_MULT_PLUS>))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(match_dup 1)
	  (match_dup 2)
	  (match_dup 3)]
	 <VPAIR_FP_MULT_TO_FMA>))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Optimize vector pair (a * b) - c into vector pair fma (a, b, -c).
(define_insn_and_split "*vpair_fms_merge_<vpair_vp_mode_lc>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	    (match_operand:OO 2 "vsx_register_operand" "wa,0")]
	   VPAIR_FP_MULT)
	  (match_operand:OO 3 "vsx_register_operand" "0,wa")]
	 <VPAIR_FP_MULT_MINUS>))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(match_dup 1)
	  (match_dup 2)
	  (unspec:OO
	   [(match_dup 3)]
	   <VPAIR_FP_MULT_TO_NEG>)]
	 <VPAIR_FP_MULT_TO_FMA>))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

(define_insn_and_split "*vpair_fms_merge2_<vpair_vp_mode_lc>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	    (match_operand:OO 2 "vsx_register_operand" "wa,0")]
	   VPAIR_FP_MULT)
	  (unspec:OO
	   [(match_operand:OO 3 "vsx_register_operand" "0,wa")]
	   <VPAIR_FP_MULT_NEG>)]
	 <VPAIR_FP_MULT_PLUS>))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(match_dup 1)
	  (match_dup 2)
	  (unspec:OO
	   [(match_dup 3)]
	   <VPAIR_FP_MULT_TO_NEG>)]
	 <VPAIR_FP_MULT_TO_FMA>))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Optimize vector pair -((a * b) + c) into vector pair -fma (a, b, c).
(define_insn_and_split "*vpair_nfma_merge_<vpair_vp_mode_lc>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(unspec:OO
	     [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")]
	     VPAIR_FP_MULT)
	    (match_operand:OO 3 "vsx_register_operand" "0,wa")]
	   <VPAIR_FP_MULT_PLUS>)]
	 <VPAIR_FP_MULT_NEG>))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(unspec:OO
	   [(match_dup 1)
	    (match_dup 2)
	    (match_dup 3)]
	   <VPAIR_FP_MULT_TO_FMA>)]
	 <VPAIR_FP_MULT_TO_NEG>))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Optimize vector pair -((a * b) - c) into vector pair -fma (a, b, -c).
(define_insn_and_split "*vpair_nfms_merge_<vpair_vp_mode_lc>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(unspec:OO
	     [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")]
	     VPAIR_FP_MULT)
	    (match_operand:OO 3 "vsx_register_operand" "0,wa")]
	   <VPAIR_FP_MULT_MINUS>)]
	 <VPAIR_FP_MULT_NEG>))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(unspec:OO
	   [(match_dup 1)
	    (match_dup 2)
	    (unspec:OO
	     [(match_dup 3)]
	     <VPAIR_FP_MULT_TO_NEG>)]
	   <VPAIR_FP_MULT_TO_FMA>)]
	 <VPAIR_FP_MULT_TO_NEG>))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

(define_insn_and_split "*vpair_nfms_merge2_<vpair_vp_mode_lc>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(unspec:OO
	   [(unspec:OO
	     [(match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")]
	     VPAIR_FP_MULT)
	    (unspec:OO
	     [(match_operand:OO 3 "vsx_register_operand" "0,wa")]
	     <VPAIR_FP_MULT_NEG>)]
	   <VPAIR_FP_MULT_PLUS>)]
	 <VPAIR_FP_MULT_NEG>))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(unspec:OO
	   [(match_dup 1)
	    (match_dup 2)
	    (unspec:OO
	     [(match_dup 3)]
	     <VPAIR_FP_MULT_TO_NEG>)]
	   <VPAIR_FP_MULT_TO_FMA>)]
	 <VPAIR_FP_MULT_TO_NEG>))]
{
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])
