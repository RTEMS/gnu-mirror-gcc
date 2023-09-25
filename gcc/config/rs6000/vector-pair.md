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

;; Support for vector pair built-in functions

(define_c_enum "unspec"
  [UNSPEC_VPAIR_ZERO
   UNSPEC_VPAIR_V4DF
   UNSPEC_VPAIR_V8SF
   UNSPEC_VPAIR_V32QI
   UNSPEC_VPAIR_V16HI
   UNSPEC_VPAIR_V8SI
   UNSPEC_VPAIR_V4DI
   UNSPEC_VPAIR_REDUCE_PLUS_F32
   UNSPEC_VPAIR_REDUCE_PLUS_F64
   UNSPEC_VPAIR_REDUCE_PLUS_I64
   ])

;; Iterator doing unary/binary arithmetic on vector pairs
(define_code_iterator VP_FP_UNARY  [abs neg])
(define_code_iterator VP_FP_BINARY [minus mult plus smin smax])

(define_code_iterator VP_INT_BINARY  [and ior minus plus smax smin umax umin xor])

;; Return the insn name from the VP_* code iterator
(define_code_attr vp_insn [(abs      "abs")
			   (and      "and")
			   (ior      "ior")
			   (minus    "sub")
			   (mult     "mul")
			   (not      "one_cmpl")
			   (neg      "neg")
			   (plus     "add")
			   (smin     "smin")
			   (smax     "smax")
			   (umin     "umin")
			   (umax     "umax")
			   (xor      "xor")])

;; Return the register constraint ("v" or "wa") for the integer code iterator
;; used.  For arithmetic operations, we need to use "v" in order to use the
;; Altivec instruction.  For logical operations, we can use wa.
(define_code_attr vp_ireg [(and   "wa")
			   (ior   "wa")
			   (minus "v")
			   (not   "wa")
			   (neg   "v")
			   (plus  "v")
			   (smax  "v")
			   (smin  "v")
			   (umax  "v")
			   (umin  "v")
			   (xor   "wa")])

;; Return the register previdcate for the integer code iterator used
(define_code_attr vp_ipredicate [(and   "vsx_register_operand")
				 (ior   "vsx_register_operand")
				 (minus "altivec_register_operand")
				 (not   "vsx_register_operand")
				 (neg   "altivec_register_operand")
				 (plus  "altivec_register_operand")
				 (smax  "altivec_register_operand")
				 (smin  "altivec_register_operand")
				 (umax  "altivec_register_operand")
				 (umin  "altivec_register_operand")
				 (xor   "vsx_register_operand")])

;; Iterator for creating the unspecs for vector pair built-ins
(define_int_iterator VP_FP [UNSPEC_VPAIR_V4DF
			    UNSPEC_VPAIR_V8SF])

(define_int_iterator VP_INT [UNSPEC_VPAIR_V4DI
			     UNSPEC_VPAIR_V8SI
			     UNSPEC_VPAIR_V16HI
			     UNSPEC_VPAIR_V32QI])

(define_int_iterator VP_ALL [UNSPEC_VPAIR_V4DF
			     UNSPEC_VPAIR_V8SF
			     UNSPEC_VPAIR_V4DI
			     UNSPEC_VPAIR_V8SI
			     UNSPEC_VPAIR_V16HI
			     UNSPEC_VPAIR_V32QI])

;; Map VP_{INT,FP,ALL} to vector mode of the arguments after they are split
(define_int_attr VP_VEC_MODE [(UNSPEC_VPAIR_V4DF  "V2DF")
			      (UNSPEC_VPAIR_V8SF  "V4SF")
			      (UNSPEC_VPAIR_V32QI "V16QI")
			      (UNSPEC_VPAIR_V16HI "V8HI")
			      (UNSPEC_VPAIR_V8SI  "V4SI")
			      (UNSPEC_VPAIR_V4DI  "V2DI")])

;; Map VP_{INT,FP,ALL} to a lower case name to identify the vector pair.
(define_int_attr vp_pmode [(UNSPEC_VPAIR_V4DF  "v4df")
			   (UNSPEC_VPAIR_V8SF  "v8sf")
			   (UNSPEC_VPAIR_V32QI "v32qi")
			   (UNSPEC_VPAIR_V16HI "v16hi")
			   (UNSPEC_VPAIR_V8SI  "v8si")
			   (UNSPEC_VPAIR_V4DI  "v4di")])

;; Map VP_{INT,FP,ALL} to a lower case name to identify the vector after the
;; vector pair has been split.
(define_int_attr vp_vmode [(UNSPEC_VPAIR_V4DF  "v2df")
			   (UNSPEC_VPAIR_V8SF  "v4sf")
			   (UNSPEC_VPAIR_V32QI "v16qi")
			   (UNSPEC_VPAIR_V16HI "v8hi")
			   (UNSPEC_VPAIR_V8SI  "v4si")
			   (UNSPEC_VPAIR_V4DI  "v2di")])

;; Map VP_INT to constraints used for the negate scratch register.  For vectors
;; of QI and HI, we need to change -a into 0 - a since we don't have a negate
;; operation.  We do have a vnegw/vnegd operation for SI and DI modes.
(define_int_attr vp_neg_reg [(UNSPEC_VPAIR_V32QI "&v")
			     (UNSPEC_VPAIR_V16HI "&v")
			     (UNSPEC_VPAIR_V8SI  "X")
			     (UNSPEC_VPAIR_V4DI  "X")])

;; Moddes of the vector element to splat to vector pair
(define_mode_iterator VP_SPLAT [DF SF DI SI HI QI])

;; MAP VP_SPLAT to the mode of the vector pair in the assemble operation
(define_mode_attr vp_splat_pmode [(DF "v4df")
				  (SF "v8sf")
				  (DI "v4di")
				  (SI "v8si")
				  (HI "v16hi")
				  (QI "v32qi")])

;; MAP VP_SPLAT to the mode of the vector containing the element
(define_mode_attr VP_SPLAT_VMODE [(DF "V2DF")
				  (SF "V4SF")
				  (DI "V2DI")
				  (SI "V4SI")
				  (HI "V8HI")
				  (QI "V16QI")])

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
  [(set_attr "length" "8")])

;; Assemble a vector pair from two vectors.  Unlike
;; __builtin_mma_assemble_pair, this function produces a vector pair output
;; directly and it takes all of the vector types.
;;
;; We cannot update the two output registers atomically, so mark the output as
;; an early clobber so we don't accidentally clobber the input operands.  */

(define_insn_and_split "vpair_assemble_<vp_pmode>"
  [(set (match_operand:OO 0 "vsx_register_operand" "=&wa")
	(unspec:OO
	 [(match_operand:<VP_VEC_MODE> 1 "mma_assemble_input_operand" "mwa")
	  (match_operand:<VP_VEC_MODE> 2 "mma_assemble_input_operand" "mwa")]
	 VP_ALL))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx src = gen_rtx_UNSPEC (OOmode,
			    gen_rtvec (2, operands[1], operands[2]),
			    UNSPEC_VSX_ASSEMBLE);
  rs6000_split_multireg_move (operands[0], src);
  DONE;
}
  [(set_attr "length" "8")])

;; Extract one of the two 128-bit vectors from a vector pair.
(define_insn_and_split "vpair_get_vector_<vp_pmode>"
  [(set (match_operand:<VP_VEC_MODE> 0 "vsx_register_operand" "=wa")
	(unspec:<VP_VEC_MODE>
	 [(match_operand:OO 1 "vsx_register_operand" "wa")
	  (match_operand 2 "const_0_to_1_operand" "n")]
	 VP_ALL))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 3))]
{
  machine_mode vmode = <VP_VEC_MODE>mode;
  unsigned reg_num = UINTVAL (operands[2]);
  if (!WORDS_BIG_ENDIAN)
    reg_num = 1 - reg_num;
	   
  operands[3] = simplify_gen_subreg (vmode, operands[0], OOmode, reg_num * 16);
})

;; Optimize extracting an 128-bit vector from a vector pair in memory.
(define_insn_and_split "*vpair_get_vector_<vp_pmode>_mem"
  [(set (match_operand:<VP_VEC_MODE> 0 "vsx_register_operand" "=wa")
	(unspec:<VP_VEC_MODE>
	 [(match_operand:OO 1 "memory_operand" "o")
	  (match_operand 2 "const_0_to_1_operand" "n")]
	 VP_ALL))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 3))]
{
  operands[3] = adjust_address (operands[1], <VP_VEC_MODE>mode,
				16 * INTVAL (operands[2]));
}
  [(set_attr "type" "vecload")])

;; Create a vector pair with a value splat'ed (duplicated) to all of the
;; elements.
(define_expand "vpair_splat_<vp_splat_pmode>"
  [(use (match_operand:OO 0 "vsx_register_operand"))
   (use (match_operand:VP_SPLAT 1 "input_operand"))]
  "TARGET_MMA"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  machine_mode element_mode = <MODE>mode;
  machine_mode vector_mode = <VP_SPLAT_VMODE>mode;

  if (op1 == CONST0_RTX (element_mode))
    {
      emit_insn (gen_vpair_zero (op0));
      DONE;
    }

  rtx vec = gen_reg_rtx (vector_mode);
  unsigned num_elements = GET_MODE_NUNITS (vector_mode);
  rtvec elements = rtvec_alloc (num_elements);
  for (unsigned i = 0; i < num_elements; i++)
    RTVEC_ELT (elements, i) = copy_rtx (op1);

  rs6000_expand_vector_init (vec, gen_rtx_PARALLEL (vector_mode, elements));
  emit_insn (gen_vpair_assemble_<vp_splat_pmode> (op0, vec, vec));
  DONE;
})


;; Vector pair floating point unary operations
(define_insn_and_split "vpair_<vp_insn>_<vp_pmode>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(VP_FP_UNARY:OO
		     (match_operand:OO 1 "vsx_register_operand" "wa"))]
		   VP_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VP_VEC_MODE>mode, operands,
			   gen_<vp_insn><vp_vmode>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize vector pair negate of absolute value
(define_insn_and_split "vpair_nabs_<vp_pmode>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(abs:OO (match_operand:OO 1 "vsx_register_operand" "ww"))]
	    VP_FP))]
	 VP_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VP_VEC_MODE>mode, operands,
			   gen_vsx_nabs<vp_vmode>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating binary operations
(define_insn_and_split "vpair_<vp_insn>_<vp_pmode>3"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(VP_FP_BINARY:OO
		     (match_operand:OO 1 "vsx_register_operand" "wa")
		     (match_operand:OO 2 "vsx_register_operand" "wa"))]
		   VP_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VP_VEC_MODE>mode, operands,
			    gen_<vp_insn><vp_vmode>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair fused multiply-add floating point operations
(define_insn_and_split "vpair_fma_<vp_pmode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(fma:OO
	   (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:OO 2 "vsx_register_operand" "wa,0")
	   (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	 VP_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VP_VEC_MODE>mode, operands,
			 gen_fma<vp_vmode>4);
  DONE;
}
  [(set_attr "length" "8")])

(define_insn_and_split "vpair_fms_<vp_pmode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(fma:OO
	   (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:OO 2 "vsx_register_operand" "wa,0")
	   (unspec:OO
	    [(neg:OO (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	     VP_FP))]
	 VP_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VP_VEC_MODE>mode, operands,
			 gen_fms<vp_vmode>4);
  DONE;
}
  [(set_attr "length" "8")])

(define_insn_and_split "vpair_nfma_<vp_pmode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(fma:OO
	      (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")
	      (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	    VP_FP))]
	 VP_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VP_VEC_MODE>mode, operands,
			 gen_nfma<vp_vmode>4);
  DONE;
}
  [(set_attr "length" "8")])

(define_insn_and_split "vpair_nfms_<vp_pmode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(fma:OO
	      (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")
	      (unspec:OO
	       [(neg:OO (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	       VP_FP))]
	   VP_FP))]
	 VP_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VP_VEC_MODE>mode, operands,
			 gen_nfms<vp_vmode>4);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize vector pair (a * b) + c into vector pair fma (a, b, c).
(define_insn_and_split "*vpair_fma_fpcontract_<vp_pmode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(plus:OO
	   (unspec:OO
	    [(mult:OO
	      (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0"))]
	    VP_FP)
	   (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	 VP_FP))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(fma:OO
	   (match_dup 1)
	   (match_dup 2)
	   (match_dup 3))]
	 VP_FP))]
{
}
  [(set_attr "length" "8")])

;; Optimize vector pair (a * b) - c into vector pair fma (a, b, -c)
(define_insn_and_split "*vpair_fms_fpcontract_<vp_pmode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(minus:OO
	   (unspec:OO
	    [(mult:OO
	      (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0"))]
	    VP_FP)
	   (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	 VP_FP))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(fma:OO
	   (match_dup 1)
	   (match_dup 2)
	   (unspec:OO
	    [(neg:OO
	      (match_dup 3))]
	    VP_FP))]
	 VP_FP))]
{
}
  [(set_attr "length" "8")])


;; Optimize vector pair -((a * b) + c) into vector pair -fma (a, b, c).
(define_insn_and_split "*vpair_nfma_fpcontract_<vp_pmode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(plus:OO
	      (unspec:OO
	       [(mult:OO
		 (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
		 (match_operand:OO 2 "vsx_register_operand" "wa,0"))]
	       VP_FP)
	      (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	    VP_FP))]
	 VP_FP))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(fma:OO
	      (match_dup 1)
	      (match_dup 2)
	      (match_dup 3))]
	    VP_FP))]
	 VP_FP))]
{
}
  [(set_attr "length" "8")])

;; Optimize vector pair -((a * b) - c) into vector pair -fma (a, b, -c)
(define_insn_and_split "*vpair_nfms_fpcontract_<vp_pmode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(minus:OO
	      (unspec:OO
	       [(mult:OO
		 (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
		 (match_operand:OO 2 "vsx_register_operand" "wa,0"))]
	       VP_FP)
	      (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	    VP_FP))]
	 VP_FP))]
  "TARGET_MMA && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(fma:OO
	      (match_dup 1)
	      (match_dup 2)
	      (unspec:OO
	       [(neg:OO
		 (match_dup 3))]
	       VP_FP))]
	    VP_FP))]
	 VP_FP))]
{
}
  [(set_attr "length" "8")])


;; Add all elements in a pair of V4SF vectors.
(define_insn_and_split "vpair_reduc_plus_scale_v8sf"
  [(set (match_operand:SF 0 "vsx_register_operand" "=wa")
	(unspec:SF [(match_operand:OO 1 "vsx_register_operand" "v")]
		   UNSPEC_VPAIR_REDUCE_PLUS_F32))
   (clobber (match_scratch:V4SF 2 "=&v"))
   (clobber (match_scratch:V4SF 3 "=&v"))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(pc)]
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx tmp1 = operands[2];
  rtx tmp2 = operands[3];
  unsigned r = reg_or_subregno (op1);
  rtx op1_hi = gen_rtx_REG (V4SFmode, r);
  rtx op1_lo = gen_rtx_REG (V4SFmode, r + 1);

  emit_insn (gen_addv4sf3 (tmp1, op1_hi, op1_lo));
  emit_insn (gen_altivec_vsldoi_v4sf (tmp2, tmp1, tmp1, GEN_INT (8)));
  emit_insn (gen_addv4sf3 (tmp2, tmp1, tmp2));
  emit_insn (gen_altivec_vsldoi_v4sf (tmp1, tmp2, tmp2, GEN_INT (4)));
  emit_insn (gen_addv4sf3 (tmp2, tmp1, tmp2));
  emit_insn (gen_vsx_xscvspdp_scalar2 (op0, tmp2));
  DONE;
}
  [(set_attr "length" "24")])

;; Add all elements in a pair of V2DF vectors
(define_insn_and_split "vpair_reduc_plus_scale_v4df"
  [(set (match_operand:DF 0 "vsx_register_operand" "=&wa")
	(unspec:DF [(match_operand:OO 1 "vsx_register_operand" "wa")]
		   UNSPEC_VPAIR_REDUCE_PLUS_F64))
   (clobber (match_scratch:DF 2 "=&wa"))
   (clobber (match_scratch:V2DF 3 "=&wa"))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(plus:V2DF (match_dup 4)
		   (match_dup 5)))
   (set (match_dup 2)
	(vec_select:DF (match_dup 3)
		       (parallel [(match_dup 6)])))
   (set (match_dup 0)
	(plus:DF (match_dup 7)
		 (match_dup 2)))]
{
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg3 = reg_or_subregno (operands[3]);

  operands[4] = gen_rtx_REG (V2DFmode, reg1);
  operands[5] = gen_rtx_REG (V2DFmode, reg1 + 1);
  operands[6] = GEN_INT (BYTES_BIG_ENDIAN ? 1 : 0);
  operands[7] = gen_rtx_REG (DFmode, reg3);
})


;; Vector pair integer negate support.
(define_insn_and_split "vpair_neg_<vp_pmode>2"
  [(set (match_operand:OO 0 "altivec_register_operand" "=v")
	(unspec:OO [(neg:OO
		     (match_operand:OO 1 "altivec_register_operand" "v"))]
		   VP_INT))
   (clobber (match_scratch:<VP_VEC_MODE> 2 "=<vp_neg_reg>"))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (minus:<VP_VEC_MODE> (match_dup 2)
					   (match_dup 5)))
   (set (match_dup 6) (minus:<VP_VEC_MODE> (match_dup 2)
					   (match_dup 7)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  machine_mode vmode = <VP_VEC_MODE>mode;

  operands[3] = CONST0_RTX (vmode);

  operands[4] = gen_rtx_REG (vmode, reg0);
  operands[5] = gen_rtx_REG (vmode, reg1);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);

  /* If the vector integer size is 32 or 64 bits, we can use the vneg{w,d}
     instructions.  */
  if (vmode == V4SImode)
    {
      emit_insn (gen_negv4si2 (operands[4], operands[5]));
      emit_insn (gen_negv4si2 (operands[6], operands[7]));
      DONE;
    }
  else if (vmode == V2DImode)
    {
      emit_insn (gen_negv2di2 (operands[4], operands[5]));
      emit_insn (gen_negv2di2 (operands[6], operands[7]));
      DONE;
    }
}
  [(set_attr "length" "8")])

;; Vector pair integer not support.
(define_insn_and_split "vpair_not_<vp_pmode>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(not:OO (match_operand:OO 1 "vsx_register_operand" "wa"))]
		   VP_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VP_VEC_MODE>mode, operands,
			   gen_one_cmpl<vp_vmode>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair integer binary operations.
(define_insn_and_split "vpair_<vp_insn>_<vp_pmode>3"
  [(set (match_operand:OO 0 "<vp_ipredicate>" "=<vp_ireg>")
	(unspec:OO [(VP_INT_BINARY:OO
		     (match_operand:OO 1 "<vp_ipredicate>" "<vp_ireg>")
		     (match_operand:OO 2 "<vp_ipredicate>" "<vp_ireg>"))]
		   VP_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VP_VEC_MODE>mode, operands,
			    gen_<vp_insn><vp_vmode>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize vector pair a & ~b
(define_insn_and_split "*vpair_andc_<vp_pmode>"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(and:OO
		     (unspec:OO
		      [(not:OO
			(match_operand:OO 1 "vsx_register_operand" "wa"))]
		      VP_INT)
		     (match_operand:OO 2 "vsx_register_operand" "wa"))]
		   VP_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VP_VEC_MODE>mode, operands,
			    gen_andc<vp_vmode>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize vector pair a | ~b
(define_insn_and_split "*vpair_iorc_<vp_pmode>"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(ior:OO
		     (unspec:OO
		      [(not:OO
			(match_operand:OO 1 "vsx_register_operand" "wa"))]
		      VP_INT)
		     (match_operand:OO 2 "vsx_register_operand" "wa"))]
		   VP_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VP_VEC_MODE>mode, operands,
			    gen_orc<vp_vmode>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Optiomize vector pair ~(a & b) or ((~a) | (~b))
(define_insn_and_split "*vpair_nand_<vp_pmode>_1"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(not:OO
	   (unspec:OO [(and:OO
			(match_operand:OO 1 "vsx_register_operand" "wa")
			(match_operand:OO 2 "vsx_register_operand" "wa"))]
		      VP_INT))]
	 VP_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VP_VEC_MODE>mode, operands,
			    gen_nand<vp_vmode>3);
  DONE;
}
  [(set_attr "length" "8")])

(define_insn_and_split "*vpair_nand_<vp_pmode>_2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(ior:OO
	   (unspec:OO
	    [(not:OO
	      (match_operand:OO 1 "vsx_register_operand" "wa"))]
	    VP_INT)
	   (unspec:OO
	    [(not:OO
	      (match_operand:OO 2 "vsx_register_operand" "wa"))]
	    VP_INT))]
	 VP_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VP_VEC_MODE>mode, operands,
			    gen_nand<vp_vmode>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Optiomize vector pair ~(a | b)  or ((~a) & (~b))
(define_insn_and_split "*vpair_nor_<vp_pmode>_1"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(not:OO
	   (unspec:OO [(ior:OO
			(match_operand:OO 1 "vsx_register_operand" "wa")
			(match_operand:OO 2 "vsx_register_operand" "wa"))]
		      VP_INT))]
	 VP_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VP_VEC_MODE>mode, operands,
			    gen_nor<vp_vmode>3);
  DONE;
}
  [(set_attr "length" "8")])

(define_insn_and_split "*vpair_nor_<vp_pmode>_2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(ior:OO
	   (unspec:OO
	    [(not:OO (match_operand:OO 1 "vsx_register_operand" "wa"))]
	    VP_INT)
	   (unspec:OO
	    [(not:OO (match_operand:OO 2 "vsx_register_operand" "wa"))]
	    VP_INT))]
	 VP_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VP_VEC_MODE>mode, operands,
			    gen_nor<vp_vmode>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Add all elements in a pair of V2DI vectors
(define_insn_and_split "vpair_reduc_plus_scale_v4di"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=&r")
	(unspec:DI [(match_operand:OO 1 "altivec_register_operand" "v")]
		   UNSPEC_VPAIR_REDUCE_PLUS_I64))
   (clobber (match_scratch:V2DI 2 "=&v"))
   (clobber (match_scratch:DI 3 "=&r"))]
  "TARGET_MMA && TARGET_POWERPC64"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(plus:V2DI (match_dup 4)
		   (match_dup 5)))
   (set (match_dup 3)
	(vec_select:DI (match_dup 2)
		       (parallel [(const_int 0)])))
   (set (match_dup 0)
	(vec_select:DI (match_dup 2)
		       (parallel [(const_int 1)])))
   (set (match_dup 0)
	(plus:DI (match_dup 0)
		 (match_dup 3)))]
{
  unsigned reg1 = reg_or_subregno (operands[1]);

  operands[4] = gen_rtx_REG (V2DImode, reg1);
  operands[5] = gen_rtx_REG (V2DImode, reg1 + 1);
}
  [(set_attr "length" "16")])


;; Support for __attribute__((__vector_size__(32)))

;; Iterator for all vector pair modes
(define_mode_iterator VPAIR [V32QI V16HI V8SI V4DI V8SF V4DF])

;; Iterator for the integer vector pair modes
(define_mode_iterator VPAIR_INT [V32QI V16HI V8SI V4DI])

;; Special iterators for NEG (V4SI and V2DI have vneg{w,d}), while V16QI and
;; V8HI have to use a subtract from 0.
(define_mode_iterator VPAIR_NEG_VNEG [V4DI V8SI])
(define_mode_iterator VPAIR_NEG_SUB [V32QI V16HI])

;; Iterator for the floating point vector pair modes
(define_mode_iterator VPAIR_FP [V8SF V4DF])

;; Iterator doing unary/binary arithmetic on vector pairs.  Split it into
;; integer and floating point operations.
(define_code_iterator VPAIR_INT_UNARY   [not])
(define_code_iterator VPAIR_INT_BINARY  [plus minus smin smax])
(define_code_iterator VPAIR_INT_LOGICAL [and ior xor])

(define_code_iterator VPAIR_FP_UNARY  [abs neg])
(define_code_iterator VPAIR_FP_BINARY [plus minus mult smin smax])

;; Give the insn name from the opertion
(define_code_attr vpair_op [(abs      "abs")
			    (and      "and")
			    (fma      "fma")
			    (ior      "ior")
			    (minus    "sub")
			    (mult     "mul")
			    (not      "one_cmpl")
			    (neg      "neg")
			    (plus     "add")
			    (smin     "smin")
			    (smax     "smax")
			    (umin     "umin")
			    (umax     "umax")
			    (xor      "xor")])

;; Vector pair move support.
(define_expand "mov<mode>"
  [(set (match_operand:VPAIR 0 "nonimmediate_operand")
	(match_operand:VPAIR 1 "input_operand"))]
  "TARGET_VECTOR_PAIR"
{
  rs6000_emit_move (operands[0], operands[1], <MODE>mode);
  DONE;
})

(define_insn_and_split "*mov<mode>"
  [(set (match_operand:VPAIR 0 "nonimmediate_operand" "=wa,wa,m,Qo,wa,wa,wa")
	(match_operand:VPAIR 1 "input_operand" "m,Qo,wa,wa,wa,j,eP"))]
  "TARGET_VECTOR_PAIR
   && (gpc_reg_operand (operands[0], <MODE>mode)
       || gpc_reg_operand (operands[1], <MODE>mode))"
  "@
   lxvp%X1 %x0,%1
   #
   stxvp%X0 %x1,%0
   #
   #
   #
   #"
  "&& reload_completed
   && !(MEM_P (operands[0]) && TARGET_STXVP)
   && !(MEM_P (operands[1]) && TARGET_LXVP)"
  [(const_int 0)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecload,vecstore,vecstore,veclogical,
                     vecperm,vecperm")
   (set_attr "size" "256")
   (set_attr "isa" "lxvp,*,stxvp,*,*,*,*")
   (set_attr "length" "*,8,*,8,8,8,40")])


;; Vector pair floating point arithmetic unary operations
(define_insn_and_split "<vpair_op><mode>2"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa")
	(VPAIR_FP_UNARY:VPAIR_FP
	 (match_operand:VPAIR_FP 1 "vsx_register_operand" "wa")))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VPAIR_VECTOR>mode, operands,
			   gen_<vpair_op><vpair_vector>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize negative absolute value (both floating point and integer)
(define_insn_and_split "nabs<mode>2"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa")
	(neg:VPAIR_FP
	 (abs:VPAIR_FP
	  (match_operand:VPAIR_FP 1 "vsx_register_operand" "wa"))))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VPAIR_VECTOR>mode, operands,
			   gen_vsx_nabs<vpair_vector>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating point arithmetic binary operations
(define_insn_and_split "<vpair_op><mode>3"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa")
	(VPAIR_FP_BINARY:VPAIR_FP
	 (match_operand:VPAIR_FP 1 "vsx_register_operand" "wa")
	 (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa")))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_<vpair_op><vpair_vector>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating point fused multiply-add
(define_insn_and_split "fma<mode>3"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(fma:VPAIR_FP
	 (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	 (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0")
	 (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa")))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VPAIR_VECTOR>mode, operands,
			 gen_fma<vpair_vector>4);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating point fused multiply-subtract
(define_insn_and_split "fms<mode>3"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(fma:VPAIR_FP
	 (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	 (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0")
	 (neg:VPAIR_FP
	  (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa"))))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VPAIR_VECTOR>mode, operands,
			 gen_fms<vpair_vector>4);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating point negative fused multiply-add
(define_insn_and_split "nfma<mode>3"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR_FP
	 (fma:VPAIR_FP
	  (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0")
	  (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa"))))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VPAIR_VECTOR>mode, operands,
			 gen_nfma<vpair_vector>4);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating point fused negative multiply-subtract
(define_insn_and_split "nfms<mode>3"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR_FP
	 (fma:VPAIR_FP
	  (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0")
	  (neg:VPAIR_FP
	   (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa")))))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VPAIR_VECTOR>mode, operands,
			 gen_nfms<vpair_vector>4);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize vector pair (a * b) + c into fma (a, b, c)
(define_insn_and_split "*fma_fpcontract_<mode>3"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(plus:VPAIR_FP
	 (mult:VPAIR_FP
	  (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0"))
	 (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa")))]
  "TARGET_VECTOR_PAIR && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(fma:VPAIR_FP (match_dup 1)
		      (match_dup 2)
		      (match_dup 3)))]
{
}
  [(set_attr "length" "8")])

;; Optimize vector pair (a * b) - c into fma (a, b, -c)
(define_insn_and_split "*fms_fpcontract_<mode>3"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(minus:VPAIR_FP
	 (mult:VPAIR_FP
	  (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0"))
	 (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa")))]
  "TARGET_VECTOR_PAIR && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(fma:VPAIR_FP (match_dup 1)
		      (match_dup 2)
		      (neg:VPAIR_FP
		       (match_dup 3))))]
{
}
  [(set_attr "length" "8")])

;; Optimize vector pair -((a * b) + c) into -fma (a, b, c)
(define_insn_and_split "*nfma_fpcontract_<mode>3"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR_FP
	 (plus:VPAIR_FP
	  (mult:VPAIR_FP
	   (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0"))
	  (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa"))))]
  "TARGET_VECTOR_PAIR && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(neg:VPAIR_FP
	 (fma:VPAIR_FP (match_dup 1)
		       (match_dup 2)
		       (match_dup 3))))]
{
}
  [(set_attr "length" "8")])

;; Optimize vector pair -((a * b) - c) into -fma (a, b, -c)
(define_insn_and_split "*nfms_fpcontract_<mode>3"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR_FP
	 (minus:VPAIR_FP
	  (mult:VPAIR_FP
	   (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0"))
	  (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa"))))]
  "TARGET_VECTOR_PAIR && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(neg:VPAIR_FP
	 (fma:VPAIR_FP (match_dup 1)
		       (match_dup 2)
		       (neg:VPAIR_FP
			(match_dup 3)))))]
{
}
  [(set_attr "length" "8")])


;; Vector pair integer arithmetic unary operations
(define_insn_and_split "<vpair_op><mode>2"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(VPAIR_INT_UNARY:VPAIR_INT
	 (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa")))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VPAIR_VECTOR>mode, operands,
			   gen_<vpair_op><vpair_vector>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair negate if we have the VNEGx instruction.
(define_insn_and_split "neg<mode>2"
  [(set (match_operand:VPAIR_NEG_VNEG 0 "vsx_register_operand" "=v")
	(neg:VPAIR_NEG_VNEG
	 (match_operand:VPAIR_NEG_VNEG 1 "vsx_register_operand" "v")))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VPAIR_VECTOR>mode, operands,
			   gen_neg<vpair_vector>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair negate if we have to do a subtract from 0
(define_insn_and_split "neg<mode>2"
  [(set (match_operand:VPAIR_NEG_SUB 0 "vsx_register_operand" "=v")
	(neg:VPAIR_NEG_SUB
	 (match_operand:VPAIR_NEG_SUB 1 "vsx_register_operand" "v")))
   (clobber (match_scratch:<VPAIR_VECTOR> 2 "=&v"))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  enum machine_mode mode = <VPAIR_VECTOR>mode;
  rtx tmp = operands[2];
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);

  emit_move_insn (tmp, CONST0_RTX (mode));
  emit_insn (gen_sub<vpair_vector>3 (gen_rtx_REG (mode, reg0),
				     tmp,
				     gen_rtx_REG (mode, reg1)));

  emit_insn (gen_sub<vpair_vector>3 (gen_rtx_REG (mode, reg0 + 1),
				     tmp,
				     gen_rtx_REG (mode, reg1 + 1)));

  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair integer arithmetic binary operations
(define_insn_and_split "<vpair_op><mode>3"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=v")
	(VPAIR_INT_BINARY:VPAIR_INT
	 (match_operand:VPAIR_INT 1 "vsx_register_operand" "v")
	 (match_operand:VPAIR_INT 2 "vsx_register_operand" "v")))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_<vpair_op><vpair_vector>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair integer arithmetic logical operations
(define_insn_and_split "<vpair_op><mode>3"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(VPAIR_INT_LOGICAL:VPAIR_INT
	 (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa")
	 (match_operand:VPAIR_INT 2 "vsx_register_operand" "wa")))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_<vpair_op><vpair_vector>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Optiomize vector pair ~(a | b)  or ((~a) & (~b)) to produce xxlnor
(define_insn_and_split "*nor<mode>3_1"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(not:VPAIR_INT
	 (ior:VPAIR_INT
	  (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa")
	  (match_operand:VPAIR_INT 2 "vsx_register_operand" "wa"))))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_nor<vpair_vector>3);
  DONE;
}
  [(set_attr "length" "8")])

(define_insn_and_split "*nor<mode>3_2"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(and:VPAIR_INT
	 (not:VPAIR_INT
	  (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa"))
	 (not:VPAIR_INT
	  (match_operand:VPAIR_INT 2 "vsx_register_operand" "wa"))))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_nor<vpair_vector>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize vector pair (~a) & b to use xxlandc
(define_insn_and_split "*andc<mode>3"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(and:VPAIR_INT
	 (not:VPAIR_INT
	  (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa"))
	 (match_operand:VPAIR_INT 2 "vsx_register_operand" "wa")))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_andc<vpair_vector>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize vector pair ~(a ^ b) to produce xxleqv
(define_insn_and_split "*eqv<mode>3"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(not:VPAIR_INT
	 (xor:VPAIR_INT
	  (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa")
	  (match_operand:VPAIR_INT 2 "vsx_register_operand" "wa"))))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_nor<vpair_vector>3);
  DONE;
}
[(set_attr "length" "8")])


;; Optiomize vector pair ~(a & b) or ((~a) | (~b)) to produce xxlnand
(define_insn_and_split "*nand<mode>3_1"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(not:VPAIR_INT
	 (and:VPAIR_INT
	  (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa")
	  (match_operand:VPAIR_INT 2 "vsx_register_operand" "wa"))))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_nand<vpair_vector>3);
  DONE;
}
  [(set_attr "length" "8")])

(define_insn_and_split "*nand<mode>3_2"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(ior:VPAIR_INT
	 (not:VPAIR_INT
	  (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa"))
	 (not:VPAIR_INT
	  (match_operand:VPAIR_INT 2 "vsx_register_operand" "wa"))))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_nand<vpair_vector>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize vector pair (~a) | b to produce xxlorc
(define_insn_and_split "*orc<mode>3"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(ior:VPAIR_INT
	 (not:VPAIR_INT
	  (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa"))
	 (match_operand:VPAIR_INT 2 "vsx_register_operand" "wa")))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_orc<vpair_vector>3);
  DONE;
}
  [(set_attr "length" "8")])
