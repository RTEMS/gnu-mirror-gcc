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
(define_code_iterator VPAIR_FP_UNARY  [abs neg])
(define_code_iterator VPAIR_FP_BINARY [minus mult plus smin smax])

(define_code_iterator VPAIR_INT_BINARY  [and ior minus plus smax smin umax umin xor])

;; Return the insn name from the VPAIR_* code iterator
(define_code_attr vpair_op [(abs      "abs")
			    (and      "and")
			    (ior      "ior")
			    (minus    "sub")
			    (mult     "mul")
			    (not      "not")
			    (neg      "neg")
			    (plus     "add")
			    (smin     "smin")
			    (smax     "smax")
			    (umin     "umin")
			    (umax     "umax")
			    (xor      "xor")])

;; Return the register constraint ("v" or "wa") for the integer code iterator
;; used
(define_code_attr vpair_ireg [(and   "wa")
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
(define_code_attr vpair_ipred [(and   "vsx_register_operand")
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
(define_int_iterator VPAIR_FP [UNSPEC_VPAIR_V4DF
			       UNSPEC_VPAIR_V8SF])

(define_int_iterator VPAIR_INT [UNSPEC_VPAIR_V4DI
				UNSPEC_VPAIR_V8SI
				UNSPEC_VPAIR_V16HI
				UNSPEC_VPAIR_V32QI])

(define_int_iterator VPAIR_ALL [UNSPEC_VPAIR_V4DF
				UNSPEC_VPAIR_V8SF
				UNSPEC_VPAIR_V4DI
				UNSPEC_VPAIR_V8SI
				UNSPEC_VPAIR_V16HI
				UNSPEC_VPAIR_V32QI])

;; Map VPAIR_{INT,FP,ALL} to vector type of the arguments after they are split
(define_int_attr VPAIR_VECTOR [(UNSPEC_VPAIR_V4DF  "V2DF")
			       (UNSPEC_VPAIR_V8SF  "V4SF")
			       (UNSPEC_VPAIR_V32QI "V16QI")
			       (UNSPEC_VPAIR_V16HI "V8HI")
			       (UNSPEC_VPAIR_V8SI  "V4SI")
			       (UNSPEC_VPAIR_V4DI  "V2DI")])

;; Map VPAIR_{INT,FP,ALL} to a lower case name to identify the vector pair.
(define_int_attr vpair_mode [(UNSPEC_VPAIR_V4DF  "v4df")
			     (UNSPEC_VPAIR_V8SF  "v8sf")
			     (UNSPEC_VPAIR_V32QI "v32qi")
			     (UNSPEC_VPAIR_V16HI "v16hi")
			     (UNSPEC_VPAIR_V8SI  "v8si")
			     (UNSPEC_VPAIR_V4DI  "v4di")])

;; Map VPAIR_INT to constraints used for the negate scratch register.
(define_int_attr vpair_neg_reg [(UNSPEC_VPAIR_V32QI "&v")
				(UNSPEC_VPAIR_V16HI "&v")
				(UNSPEC_VPAIR_V8SI  "X")
				(UNSPEC_VPAIR_V4DI  "X")])

;; Moddes of the vector element to splat to vector pair
(define_mode_iterator VPAIR_SPLAT [DF SF DI SI HI QI])

;; MAP VPAIR_SPLAT to the name in the assemble operation
(define_mode_attr vpair_splat_mode [(DF "v4df")
				    (SF "v8sf")
				    (DI "v4di")
				    (SI "v8si")
				    (HI "v16hi")
				    (QI "v32qi")])

;; MAP VPAIR_SPLAT to the mode of the vector containing the element
(define_mode_attr VPAIR_SPLAT_VMODE [(DF "V2DF")
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
  unsigned reg0 = reg_or_subregno (operands[0]);
  rtvec vec_zero = gen_rtvec (2, const0_rtx, const0_rtx);

  operands[1] = gen_rtx_REG (V2DImode, reg0);
  operands[2] = gen_rtx_REG (V2DImode, reg0 + 1);
  operands[3] = gen_rtx_CONST_VECTOR (V2DImode, vec_zero);
}
  [(set_attr "length" "8")])

;; Assemble a vector pair from two vectors.  Unlike
;; __builtin_mma_assemble_pair, this function produces a vector pair output
;; directly and it takes all of the vector types.
;;
;; We cannot update the two output registers atomically, so mark the output as
;; an early clobber so we don't accidentally clobber the input operands.  */

(define_insn_and_split "vpair_assemble_<vpair_mode>"
  [(set (match_operand:OO 0 "vsx_register_operand" "=&wa")
	(unspec:OO
	 [(match_operand:<VPAIR_VECTOR> 1 "mma_assemble_input_operand" "mwa")
	  (match_operand:<VPAIR_VECTOR> 2 "mma_assemble_input_operand" "mwa")]
	 VPAIR_ALL))]
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

;; Extract one of the two 128-bitvectors from a vector pair.
(define_insn_and_split "vpair_get_vector_<vpair_mode>"
  [(set (match_operand:<VPAIR_VECTOR> 0 "vsx_register_operand" "=wa")
	(unspec:<VPAIR_VECTOR>
	 [(match_operand:OO 1 "vsx_register_operand" "wa")
	  (match_operand 2 "const_0_to_1_operand" "n")]
	 VPAIR_ALL))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 3))]
{
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg_num = UINTVAL (operands[2]);
  if (!WORDS_BIG_ENDIAN)
    reg_num = 1 - reg_num;
	   
  operands[3] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1 + reg_num);
})

;; Create a vector pair with a value splat'ed (duplicated) to all of the
;; elements.
(define_expand "vpair_splat_<vpair_splat_mode>"
  [(use (match_operand:OO 0 "vsx_register_operand"))
   (use (match_operand:VPAIR_SPLAT 1 "input_operand"))]
  "TARGET_MMA"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  machine_mode element_mode = <MODE>mode;
  machine_mode vector_mode = <VPAIR_SPLAT_VMODE>mode;

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
  emit_insn (gen_vpair_assemble_<vpair_splat_mode> (op0, vec, vec));
  DONE;
})


;; Vector pair floating point unary operations
(define_insn_and_split "vpair_<vpair_op>_<vpair_mode>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(VPAIR_FP_UNARY:OO
		     (match_operand:OO 1 "vsx_register_operand" "wa"))]
		   VPAIR_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 2) (VPAIR_FP_UNARY:<VPAIR_VECTOR> (match_dup 3)))
   (set (match_dup 4) (VPAIR_FP_UNARY:<VPAIR_VECTOR> (match_dup 5)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[2] = gen_rtx_REG (vmode, reg0);
  operands[3] = gen_rtx_REG (vmode, reg1);
  operands[4] = gen_rtx_REG (vmode, reg0 + 1);
  operands[5] = gen_rtx_REG (vmode, reg1 + 1);
}
  [(set_attr "length" "8")])

;; Optimize vector pair negate of absolute value
(define_insn_and_split "vpair_nabs_<vpair_mode>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(abs:OO (match_operand:OO 1 "vsx_register_operand" "ww"))]
	    VPAIR_FP))]
	 VPAIR_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(neg:<VPAIR_VECTOR>
	 (abs:<VPAIR_VECTOR> (match_dup 3))))
   (set (match_dup 4)
	(neg:<VPAIR_VECTOR>
	 (abs:<VPAIR_VECTOR> (match_dup 5))))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[2] = gen_rtx_REG (vmode, reg0);
  operands[3] = gen_rtx_REG (vmode, reg1);
  operands[4] = gen_rtx_REG (vmode, reg0 + 1);
  operands[5] = gen_rtx_REG (vmode, reg1 + 1);
}
  [(set_attr "length" "8")])

;; Vector pair floating binary operations
(define_insn_and_split "vpair_<vpair_op>_<vpair_mode>3"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(VPAIR_FP_BINARY:OO
		     (match_operand:OO 1 "vsx_register_operand" "wa")
		     (match_operand:OO 2 "vsx_register_operand" "wa"))]
		   VPAIR_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(VPAIR_FP_BINARY:<VPAIR_VECTOR> (match_dup 4)
					(match_dup 5)))
   (set (match_dup 6)
	(VPAIR_FP_BINARY:<VPAIR_VECTOR> (match_dup 7)
					(match_dup 8)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
}
  [(set_attr "length" "8")])

;; Vector pair fused multiply-add floating point operations
(define_insn_and_split "vpair_fma_<vpair_mode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(fma:OO
	   (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:OO 2 "vsx_register_operand" "wa,0")
	   (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	 VPAIR_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
	(fma:<VPAIR_VECTOR> (match_dup 5)
			    (match_dup 6)
			    (match_dup 7)))
   (set (match_dup 8)
	(fma:<VPAIR_VECTOR> (match_dup 9)
			    (match_dup 10)
			    (match_dup 11)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  unsigned reg3 = reg_or_subregno (operands[3]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[4] = gen_rtx_REG (vmode, reg0);
  operands[5] = gen_rtx_REG (vmode, reg1);
  operands[6] = gen_rtx_REG (vmode, reg2);
  operands[7] = gen_rtx_REG (vmode, reg3);

  operands[8] = gen_rtx_REG (vmode, reg0 + 1);
  operands[9] = gen_rtx_REG (vmode, reg1 + 1);
  operands[10] = gen_rtx_REG (vmode, reg2 + 1);
  operands[11] = gen_rtx_REG (vmode, reg3 + 1);
}
  [(set_attr "length" "8")])

(define_insn_and_split "vpair_fms_<vpair_mode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(fma:OO
	   (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:OO 2 "vsx_register_operand" "wa,0")
	   (unspec:OO
	    [(neg:OO (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	     VPAIR_FP))]
	 VPAIR_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
	(fma:<VPAIR_VECTOR> (match_dup 5)
			    (match_dup 6)
			    (neg:<VPAIR_VECTOR> (match_dup 7))))
   (set (match_dup 8)
	(fma:<VPAIR_VECTOR> (match_dup 9)
			    (match_dup 10)
			    (neg:<VPAIR_VECTOR> (match_dup 11))))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  unsigned reg3 = reg_or_subregno (operands[3]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[4] = gen_rtx_REG (vmode, reg0);
  operands[5] = gen_rtx_REG (vmode, reg1);
  operands[6] = gen_rtx_REG (vmode, reg2);
  operands[7] = gen_rtx_REG (vmode, reg3);

  operands[8] = gen_rtx_REG (vmode, reg0 + 1);
  operands[9] = gen_rtx_REG (vmode, reg1 + 1);
  operands[10] = gen_rtx_REG (vmode, reg2 + 1);
  operands[11] = gen_rtx_REG (vmode, reg3 + 1);
}
  [(set_attr "length" "8")])

(define_insn_and_split "vpair_nfma_<vpair_mode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(fma:OO
	      (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")
	      (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	    VPAIR_FP))]
	 VPAIR_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
	(neg:<VPAIR_VECTOR>
	 (fma:<VPAIR_VECTOR> (match_dup 5)
			     (match_dup 6)
			     (match_dup 7))))
   (set (match_dup 8)
	(neg:<VPAIR_VECTOR>
	 (fma:<VPAIR_VECTOR> (match_dup 9)
			     (match_dup 10)
			     (match_dup 11))))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  unsigned reg3 = reg_or_subregno (operands[3]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[4] = gen_rtx_REG (vmode, reg0);
  operands[5] = gen_rtx_REG (vmode, reg1);
  operands[6] = gen_rtx_REG (vmode, reg2);
  operands[7] = gen_rtx_REG (vmode, reg3);

  operands[8] = gen_rtx_REG (vmode, reg0 + 1);
  operands[9] = gen_rtx_REG (vmode, reg1 + 1);
  operands[10] = gen_rtx_REG (vmode, reg2 + 1);
  operands[11] = gen_rtx_REG (vmode, reg3 + 1);
}
  [(set_attr "length" "8")])

(define_insn_and_split "vpair_nfms_<vpair_mode>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(fma:OO
	      (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")
	      (unspec:OO
	       [(neg:OO (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	       VPAIR_FP))]
	   VPAIR_FP))]
	 VPAIR_FP))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 4)
	(neg:<VPAIR_VECTOR>
	 (fma:<VPAIR_VECTOR> (match_dup 5)
			     (match_dup 6)
			     (neg:<VPAIR_VECTOR> (match_dup 7)))))
   (set (match_dup 8)
	(neg:<VPAIR_VECTOR>
	 (fma:<VPAIR_VECTOR> (match_dup 9)
			     (match_dup 10)
			     (neg:<VPAIR_VECTOR> (match_dup 11)))))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  unsigned reg3 = reg_or_subregno (operands[3]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[4] = gen_rtx_REG (vmode, reg0);
  operands[5] = gen_rtx_REG (vmode, reg1);
  operands[6] = gen_rtx_REG (vmode, reg2);
  operands[7] = gen_rtx_REG (vmode, reg3);

  operands[8] = gen_rtx_REG (vmode, reg0 + 1);
  operands[9] = gen_rtx_REG (vmode, reg1 + 1);
  operands[10] = gen_rtx_REG (vmode, reg2 + 1);
  operands[11] = gen_rtx_REG (vmode, reg3 + 1);
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
(define_insn_and_split "vpair_neg_<vpair_mode>2"
  [(set (match_operand:OO 0 "altivec_register_operand" "=v")
	(unspec:OO [(neg:OO
		     (match_operand:OO 1 "altivec_register_operand" "v"))]
		   VPAIR_INT))
   (clobber (match_scratch:<VPAIR_VECTOR> 2 "=<vpair_neg_reg>"))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (minus:<VPAIR_VECTOR> (match_dup 2)
					    (match_dup 5)))
   (set (match_dup 6) (minus:<VPAIR_VECTOR> (match_dup 2)
					    (match_dup 7)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

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
(define_insn_and_split "vpair_not_<vpair_mode>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(not:OO (match_operand:OO 1 "vsx_register_operand" "wa"))]
		   VPAIR_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 2) (not:<VPAIR_VECTOR> (match_dup 3)))
   (set (match_dup 4) (not:<VPAIR_VECTOR> (match_dup 5)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[2] = gen_rtx_REG (vmode, reg0);
  operands[3] = gen_rtx_REG (vmode, reg1);

  operands[4] = gen_rtx_REG (vmode, reg0 + 1);
  operands[5] = gen_rtx_REG (vmode, reg1 + 1);
}
  [(set_attr "length" "8")])

;; Vector pair integer binary operations.
(define_insn_and_split "vpair_<vpair_op>_<vpair_mode>3"
  [(set (match_operand:OO 0 "<vpair_ipred>" "=<vpair_ireg>")
	(unspec:OO [(VPAIR_INT_BINARY:OO
		     (match_operand:OO 1 "<vpair_ipred>" "<vpair_ireg>")
		     (match_operand:OO 2 "<vpair_ipred>" "<vpair_ireg>"))]
		   VPAIR_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(VPAIR_INT_BINARY:<VPAIR_VECTOR> (match_dup 4)
					 (match_dup 5)))
   (set (match_dup 6)
	(VPAIR_INT_BINARY:<VPAIR_VECTOR> (match_dup 7)
					 (match_dup 8)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
}
  [(set_attr "length" "8")])

;; Optimize vector pair a & ~b
(define_insn_and_split "*vpair_andc_<vpair_mode>"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(and:OO
		     (unspec:OO
		      [(not:OO
			(match_operand:OO 1 "vsx_register_operand" "wa"))]
		     VPAIR_INT)
		     (match_operand:OO 2 "vsx_register_operand" "wa"))]
		   VPAIR_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(and:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 4))
			    (match_dup 5)))
   (set (match_dup 6)
	(and:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 7))
			    (match_dup 8)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
}
  [(set_attr "length" "8")])

;; Optimize vector pair a | ~b
(define_insn_and_split "*vpair_iorc_<vpair_mode>"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(ior:OO
		     (unspec:OO
		      [(not:OO
			(match_operand:OO 1 "vsx_register_operand" "wa"))]
		     VPAIR_INT)
		     (match_operand:OO 2 "vsx_register_operand" "wa"))]
		   VPAIR_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(ior:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 4))
			    (match_dup 5)))
   (set (match_dup 6)
	(ior:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 7))
			    (match_dup 8)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
}
  [(set_attr "length" "8")])

;; Optiomize vector pair ~(a & b) or ((~a) | (~b))
(define_insn_and_split "*vpair_nand_<vpair_mode>_1"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(not:OO
	   (unspec:OO [(and:OO
			(match_operand:OO 1 "vsx_register_operand" "wa")
			(match_operand:OO 2 "vsx_register_operand" "wa"))]
		      VPAIR_INT))]
	 VPAIR_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(ior:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 4))
			    (not:<VPAIR_VECTOR> (match_dup 5))))
   (set (match_dup 6)
	(ior:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 7))
			    (not:<VPAIR_VECTOR> (match_dup 8))))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
}
  [(set_attr "length" "8")])

(define_insn_and_split "*vpair_nand_<vpair_mode>_2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(ior:OO
	   (unspec:OO [(not:OO
			(match_operand:OO 1 "vsx_register_operand" "wa"))]
		      VPAIR_INT)
	   (unspec:OO [(not:OO
			(match_operand:OO 2 "vsx_register_operand" "wa"))]
		      VPAIR_INT))]
	 VPAIR_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(ior:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 4))
			    (not:<VPAIR_VECTOR> (match_dup 5))))
   (set (match_dup 6)
	(ior:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 7))
			    (not:<VPAIR_VECTOR> (match_dup 8))))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
}
  [(set_attr "length" "8")])

;; Optiomize vector pair ~(a | b)  or ((~a) & (~b))
(define_insn_and_split "*vpair_nor_<vpair_mode>_1"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(not:OO
	   (unspec:OO [(ior:OO
			(match_operand:OO 1 "vsx_register_operand" "wa")
			(match_operand:OO 2 "vsx_register_operand" "wa"))]
		      VPAIR_INT))]
	 VPAIR_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(and:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 4))
			    (not:<VPAIR_VECTOR> (match_dup 5))))
   (set (match_dup 6)
	(and:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 7))
			    (not:<VPAIR_VECTOR> (match_dup 8))))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
}
  [(set_attr "length" "8")])

(define_insn_and_split "*vpair_nor_<vpair_mode>_2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(ior:OO
	   (unspec:OO [(not:OO
			(match_operand:OO 1 "vsx_register_operand" "wa"))]
		      VPAIR_INT)
	   (unspec:OO [(not:OO
			(match_operand:OO 2 "vsx_register_operand" "wa"))]
		      VPAIR_INT))]
	 VPAIR_INT))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(and:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 4))
			    (not:<VPAIR_VECTOR> (match_dup 5))))
   (set (match_dup 6)
	(and:<VPAIR_VECTOR> (not:<VPAIR_VECTOR> (match_dup 7))
			    (not:<VPAIR_VECTOR> (match_dup 8))))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  machine_mode vmode = <VPAIR_VECTOR>mode;

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
}
  [(set_attr "length" "8")])

;; Add all elements in a pair of V2DI vectors
(define_insn_and_split "vpair_reduc_plus_scale_v4di"
  [(set (match_operand:DI 0 "gpc_reg_operand" "=&r")
	(unspec:DI [(match_operand:OO 1 "altivec_register_operand" "v")]
		   UNSPEC_VPAIR_REDUCE_PLUS_I64))
   (clobber (match_scratch:DI 2 "=&r"))
   (clobber (match_scratch:V2DI 3 "=&v"))]
  "TARGET_MMA && TARGET_POWERPC64"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(plus:V2DI (match_dup 4)
		   (match_dup 5)))
   (set (match_dup 2)
	(vec_select:DI (match_dup 3)
		       (parallel [(const_int 0)])))
   (set (match_dup 0)
	(vec_select:DI (match_dup 3)
		       (parallel [(const_int 1)])))
   (set (match_dup 0)
	(plus:DI (match_dup 0)
		 (match_dup 2)))]
{
  unsigned reg1 = reg_or_subregno (operands[1]);

  operands[4] = gen_rtx_REG (V2DImode, reg1);
  operands[5] = gen_rtx_REG (V2DImode, reg1 + 1);
}
  [(set_attr "length" "16")])
