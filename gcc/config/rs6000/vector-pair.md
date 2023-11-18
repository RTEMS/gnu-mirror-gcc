;; Vector pair arithmetic and logical instruction support.
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

;; This function adds support for doing vector operations on pairs of vector
;; registers.  Most of the instructions use vector pair instructions to load
;; and possibly store registers, but splitting the operation after register
;; allocation to do 2 separate operations.  The second scheduler pass can
;; interleave other instructions between these pairs of instructions if
;; possible.

;; Iterator for all vector pair modes.  Even though we do not provide integer
;; vector pair operations at this time, we need to support loading and storing
;; integer vector pairs for perumte operations (and eventually compare).
(define_mode_iterator VPAIR [V32QI V16HI V8SI V4DI V8SF V4DF])

;; Floating point vector pair ops
(define_mode_iterator VPAIR_FP [V8SF V4DF])

;; Iterator for floating point unary/binary operations.
(define_code_iterator VPAIR_FP_UNARY  [abs neg])
(define_code_iterator VPAIR_FP_BINARY [plus minus mult smin smax])

;; Integer vector pair ops.  We need the basic logical opts to support
;; permution on little endian systems.
(define_mode_iterator VPAIR_INT [V32QI V16HI V8SI V4DI])

;; Special iterators for NEG (V4SI and V2DI have vneg{w,d}), while V16QI and
;; V8HI have to use a subtract from 0.
(define_mode_iterator VPAIR_NEG_VNEG [V4DI V8SI])
(define_mode_iterator VPAIR_NEG_SUB [V32QI V16HI])

;; Iterator integer unary/binary operations.  Logical operations can be done on
;; all VSX registers, while the binary int operators need Altivec registers.
(define_code_iterator VPAIR_LOGICAL_UNARY  [not])
(define_code_iterator VPAIR_LOGICAL_BINARY [and ior xor])

(define_code_iterator VPAIR_INT_BINARY     [plus minus smin smax])

;; Give the insn name from the opertion
(define_code_attr vpair_op [(abs   "abs")
			    (div   "div")
			    (and   "and")
			    (fma   "fma")
			    (ior   "ior")
			    (minus "sub")
			    (mult  "mul")
			    (neg   "neg")
			    (not   "one_cmpl")
			    (plus  "add")
			    (smin  "smin")
			    (smax  "smax")
			    (xor   "xor")])

;; Map vector pair mode to vector mode in upper case after the vector pair is
;; split to two vectors.
(define_mode_attr VPAIR_VECTOR [(V32QI "V16QI")
				(V16HI "V8HI")
				(V8SI  "V4SI")
				(V4DI  "V2DI")
				(V8SF  "V4SF")
                                (V4DF  "V2DF")])

;; Map vector pair mode to vector mode in lower case after the vector pair is
;; split to two vectors.
(define_mode_attr vpair_vector_l [(V32QI "v16qi")
				  (V16HI "v8hi")
				  (V8SI  "v4si")
				  (V4DI  "v2di")
				  (V8SF  "v4sf")
				  (V4DF  "v2df")])

;; Map vector pair mode to the base element mode.
(define_mode_attr VPAIR_ELEMENT [(V32QI "QI")
				 (V16HI "HI")
				 (V8SI  "SI")
				 (V4DI  "DI")
				 (V8SF  "SF")
				 (V4DF  "DF")])

;; Map vector pair mode to the base element mode in lower case.
(define_mode_attr vpair_element_l [(V32QI "qi")
				   (V16HI "hi")
				   (V8SI  "si")
				   (V4DI  "di")
				   (V8SF  "sf")
				   (V4DF  "df")])

;; Vector pair move support.
(define_expand "mov<mode>"
  [(set (match_operand:VPAIR 0 "nonimmediate_operand")
	(match_operand:VPAIR 1 "input_operand"))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
{
  rs6000_emit_move (operands[0], operands[1], <MODE>mode);
  DONE;
})

(define_insn_and_split "*mov<mode>"
  [(set (match_operand:VPAIR 0 "nonimmediate_operand" "=wa,wa,ZwO,QwO,wa,wa")
	(match_operand:VPAIR 1 "input_operand" "ZwO,QwO,wa,wa,wa,j"))]
  "TARGET_MMA
   && (gpc_reg_operand (operands[0], <MODE>mode)
       || gpc_reg_operand (operands[1], <MODE>mode))"
  "@
   lxvp%X1 %x0,%1
   #
   stxvp%X0 %x1,%0
   #
   #
   #"
  "&& reload_completed
   && ((MEM_P (operands[0]) && !TARGET_STORE_VECTOR_PAIR)
       || (MEM_P (operands[1]) && !TARGET_LOAD_VECTOR_PAIR)
       || (!MEM_P (operands[0]) && !MEM_P (operands[1])))"
  [(const_int 0)]
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];

  if (op1 == CONST0_RTX (<MODE>mode))
    {
      machine_mode vmode = <VPAIR_VECTOR>mode;
      rtx op0_reg0 = simplify_gen_subreg (vmode, op0, <MODE>mode, 0);
      rtx op0_reg1 = simplify_gen_subreg (vmode, op0, <MODE>mode, 16);
      rtx zero = CONST0_RTX (vmode);
      emit_move_insn (op0_reg0, zero);
      emit_move_insn (op0_reg1, zero);
      DONE;
    }

  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecload,vecstore,vecstore,veclogical,vecperm")
   (set_attr "size" "256")
   (set_attr "length" "*,8,*,8,8,8")
   (set_attr "isa" "lxvp,*,stxvp,*,*,*")])

;; Vector pair initialization
(define_expand "vec_init<mode><vpair_element_l>"
  [(match_operand:VPAIR 0 "vsx_register_operand")
   (match_operand:VPAIR 1 "")]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
{
  rs6000_expand_vector_pair_init (operands[0], operands[1]);
  DONE;
})

;; Vector pair set element
(define_expand "vec_set<mode>"
  [(match_operand:VPAIR 0 "vsx_register_operand")
   (match_operand:<VPAIR_ELEMENT> 1 "register_operand")
   (match_operand 2 "vec_set_index_operand")]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
{
  rs6000_expand_vector_pair_set (operands[0], operands[1], operands[2]);
  DONE;
})

;; Vector pair extraction
(define_insn_and_split "vec_extract<mode><vpair_element_l>"
  [(set (match_operand:<VPAIR_ELEMENT> 0 "vsx_register_operand" "=wa")
	(vec_select:<VPAIR_ELEMENT>
	 (match_operand:VPAIR 1 "vsx_register_operand" "wa")
	 (parallel [(match_operand:QI 2 "const_int_operand" "n")])))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  HOST_WIDE_INT elt = INTVAL (operands[2]);
  machine_mode mode = <MODE>mode;
  machine_mode vmode = <VPAIR_VECTOR>mode;
  unsigned vsize = GET_MODE_SIZE (<VPAIR_VECTOR>mode);
  unsigned reg_num = ((WORDS_BIG_ENDIAN && elt >= vsize)
		      || (!WORDS_BIG_ENDIAN && elt < vsize));
	   
  rtx vreg = simplify_gen_subreg (vmode, op1, mode, reg_num * 16);
  emit_insn (gen_vsx_extract_<vpair_vector_l> (op0, vreg,
					       GEN_INT (elt % vsize)));
  DONE;
})

;; Assemble a vector pair from two vectors.
;;
;; We have both endian versions to change which input register will be moved
;; the the first register in the vector pair.
(define_expand "vpair_concat_<mode>"
  [(set (match_operand:VPAIR 0 "vsx_register_operand")
	(vec_concat:VPAIR
	 (match_operand:<VPAIR_VECTOR> 1 "input_operand")
	 (match_operand:<VPAIR_VECTOR> 2 "input_operand")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32")

(define_insn_and_split "*vpair_concat_<mode>_be"
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa,&wa")
	(vec_concat:VPAIR
	 (match_operand:<VPAIR_VECTOR> 1 "input_operand" "0,mwajeP")
	 (match_operand:<VPAIR_VECTOR> 2 "input_operand" "mwajeP,mwajeP")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32 && WORDS_BIG_ENDIAN"
  "#"
  "&& reload_completed"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 4) (match_dup 2))]
{
  machine_mode vmode = <VPAIR_VECTOR>mode;
  rtx op0 = operands[0];
  operands[3] = simplify_gen_subreg (vmode, op0, <MODE>mode, 0);
  operands[4] = simplify_gen_subreg (vmode, op0, <MODE>mode, 16);
}
  [(set_attr "length" "8")])

(define_insn_and_split "*vpair_concat_<mode>_le"
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=&wa,wa")
	(vec_concat:VPAIR
	 (match_operand:<VPAIR_VECTOR> 1 "input_operand" "mwajeP,0")
	 (match_operand:<VPAIR_VECTOR> 2 "input_operand" "mwajeP,mwajeP")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32 && !WORDS_BIG_ENDIAN"
  "#"
  "&& reload_completed"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 4) (match_dup 2))]
{
  machine_mode vmode = <VPAIR_VECTOR>mode;
  rtx op0 = operands[0];
  operands[3] = simplify_gen_subreg (vmode, op0, <MODE>mode, 0);
  operands[4] = simplify_gen_subreg (vmode, op0, <MODE>mode, 16);
}
  [(set_attr "length" "8")])

;; Zero a vector pair
(define_expand "vpair_zero_<mode>"
  [(set (match_operand:VPAIR 0 "vsx_register_operand") (match_dup 1))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
{
  operands[1] = CONST0_RTX (<MODE>mode);
})

;; Create a vector pair with a value splat'ed (duplicated) to all of the
;; elements.
(define_expand "vpair_splat_<mode>"
  [(use (match_operand:VPAIR 0 "vsx_register_operand"))
   (use (match_operand:<VPAIR_ELEMENT> 1 "input_operand"))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
{
  machine_mode vmode = <VPAIR_VECTOR>mode;
  rtx op0 = operands[0];
  rtx op1 = operands[1];

  if (op1 == CONST0_RTX (vmode))
    {
      emit_insn (gen_vpair_zero_<mode> (op0));
      DONE;
    }

  rtx tmp = gen_reg_rtx (vmode);

  unsigned num_elements = GET_MODE_NUNITS (vmode);
  rtvec elements = rtvec_alloc (num_elements);
  for (size_t i = 0; i < num_elements; i++)
    RTVEC_ELT (elements, i) = copy_rtx (op1);

  rtx vec_elements = gen_rtx_PARALLEL (vmode, elements);
  rs6000_expand_vector_init (tmp, vec_elements);
  emit_insn (gen_vpair_concat_<mode> (op0, tmp, tmp));
  DONE;
})

;; Vector pair floating point arithmetic unary operations
(define_insn_and_split "<vpair_op><mode>2"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa")
	(VPAIR_FP_UNARY:VPAIR_FP
	 (match_operand:VPAIR_FP 1 "vsx_register_operand" "wa")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VPAIR_VECTOR>mode, operands,
			   gen_<vpair_op><vpair_vector_l>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize negative absolute value (both floating point and integer)
(define_insn_and_split "nabs<mode>2"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa")
	(neg:VPAIR_FP
	 (abs:VPAIR_FP
	  (match_operand:VPAIR_FP 1 "vsx_register_operand" "wa"))))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VPAIR_VECTOR>mode, operands,
			   gen_vsx_nabs<vpair_vector_l>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating point arithmetic binary operations
(define_insn_and_split "<vpair_op><mode>3"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa")
	(VPAIR_FP_BINARY:VPAIR_FP
	 (match_operand:VPAIR_FP 1 "vsx_register_operand" "wa")
	 (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_<vpair_op><vpair_vector_l>3);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating point fused multiply-add
(define_insn_and_split "fma<mode>4"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(fma:VPAIR_FP
	 (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	 (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0")
	 (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VPAIR_VECTOR>mode, operands,
			 gen_fma<vpair_vector_l>4);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating point fused multiply-subtract
(define_insn_and_split "fms<mode>4"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(fma:VPAIR_FP
	 (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	 (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0")
	 (neg:VPAIR_FP
	  (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa"))))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VPAIR_VECTOR>mode, operands,
			 gen_fms<vpair_vector_l>4);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating point negative fused multiply-add
(define_insn_and_split "nfma<mode>4"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR_FP
	 (fma:VPAIR_FP
	  (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0")
	  (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa"))))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VPAIR_VECTOR>mode, operands,
			 gen_nfma<vpair_vector_l>4);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair floating point fused negative multiply-subtract
(define_insn_and_split "nfms<mode>4"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR_FP
	 (fma:VPAIR_FP
	  (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0")
	  (neg:VPAIR_FP
	   (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa")))))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_fma_vector_pair (<VPAIR_VECTOR>mode, operands,
			 gen_nfms<vpair_vector_l>4);
  DONE;
}
  [(set_attr "length" "8")])

;; Optimize vector pair (a * b) + c into fma (a, b, c)
(define_insn_and_split "*fma_fpcontract_<mode>4"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(plus:VPAIR_FP
	 (mult:VPAIR_FP
	  (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0"))
	 (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
   && flag_fp_contract_mode == FP_CONTRACT_FAST"
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
(define_insn_and_split "*fms_fpcontract_<mode>4"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(minus:VPAIR_FP
	 (mult:VPAIR_FP
	  (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0"))
	 (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
   && flag_fp_contract_mode == FP_CONTRACT_FAST"
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
(define_insn_and_split "*nfma_fpcontract_<mode>4"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR_FP
	 (plus:VPAIR_FP
	  (mult:VPAIR_FP
	   (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0"))
	  (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa"))))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
   && flag_fp_contract_mode == FP_CONTRACT_FAST"
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
(define_insn_and_split "*nfms_fpcontract_<mode>4"
  [(set (match_operand:VPAIR_FP 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR_FP
	 (minus:VPAIR_FP
	  (mult:VPAIR_FP
	   (match_operand:VPAIR_FP 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:VPAIR_FP 2 "vsx_register_operand" "wa,0"))
	  (match_operand:VPAIR_FP 3 "vsx_register_operand" "0,wa"))))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
   && flag_fp_contract_mode == FP_CONTRACT_FAST"
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

;; Vector pair negate if we have the VNEGx instruction.
(define_insn_and_split "neg<mode>2"
  [(set (match_operand:VPAIR_NEG_VNEG 0 "vsx_register_operand" "=v")
	(neg:VPAIR_NEG_VNEG
	 (match_operand:VPAIR_NEG_VNEG 1 "vsx_register_operand" "v")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VPAIR_VECTOR>mode, operands,
			   gen_neg<vpair_vector_l>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair negate if we have to do a subtract from 0
(define_insn_and_split "neg<mode>2"
  [(set (match_operand:VPAIR_NEG_SUB 0 "vsx_register_operand" "=v")
	(neg:VPAIR_NEG_SUB
	 (match_operand:VPAIR_NEG_SUB 1 "vsx_register_operand" "v")))
   (clobber (match_scratch:<VPAIR_VECTOR> 2 "=&v"))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  enum machine_mode mode = <VPAIR_VECTOR>mode;
  rtx tmp = operands[2];
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);

  emit_move_insn (tmp, CONST0_RTX (mode));
  emit_insn (gen_sub<vpair_vector_l>3 (gen_rtx_REG (mode, reg0),
				       tmp,
				       gen_rtx_REG (mode, reg1)));

  emit_insn (gen_sub<vpair_vector_l>3 (gen_rtx_REG (mode, reg0 + 1),
				       tmp,
				       gen_rtx_REG (mode, reg1 + 1)));

  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair logical unary operations
(define_insn_and_split "<vpair_op><mode>2"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(VPAIR_LOGICAL_UNARY:VPAIR_INT
	 (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_unary_vector_pair (<VPAIR_VECTOR>mode, operands,
			   gen_<vpair_op><vpair_vector_l>2);
  DONE;
}
  [(set_attr "length" "8")])

;; Vector pair logical binary operations
(define_insn_and_split "<vpair_op><mode>3"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(VPAIR_LOGICAL_BINARY:VPAIR_INT
	 (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa")
	 (match_operand:VPAIR_INT 2 "vsx_register_operand" "wa")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_<vpair_op><vpair_vector_l>3);
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_nor<vpair_vector_l>3);
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_nor<vpair_vector_l>3);
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_andc<vpair_vector_l>3);
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_nor<vpair_vector_l>3);
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_nand<vpair_vector_l>3);
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_nand<vpair_vector_l>3);
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_orc<vpair_vector_l>3);
  DONE;
}
  [(set_attr "length" "8")])
