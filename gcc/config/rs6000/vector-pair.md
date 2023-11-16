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

(define_c_enum "unspec"
  [UNSPEC_VPAIR_ASSEMBLE
   UNSPEC_VPAIR_SPLAT])

;; Iterator for all vector pair modes
(define_mode_iterator VPAIR [V8SF V4DF])

;; Iterator for floating point unary/binary operations.
(define_code_iterator VPAIR_UNARY  [abs neg])
(define_code_iterator VPAIR_BINARY [plus minus mult smin smax])

;; Give the insn name from the opertion
(define_code_attr vpair_op [(abs   "abs")
			    (div   "div")
			    (fma   "fma")
			    (minus "sub")
			    (mult  "mul")
			    (neg   "neg")
			    (plus  "add")
			    (smin  "smin")
			    (smax  "smax")])

;; Map vector pair mode to vector mode in upper case after the vector pair is
;; split to two vectors.
(define_mode_attr VPAIR_VECTOR [(V8SF  "V4SF")
                                (V4DF  "V2DF")])

;; Map vector pair mode to vector mode in lower case after the vector pair is
;; split to two vectors.
(define_mode_attr vpair_vector_l [(V8SF  "v4sf")
				  (V4DF  "v2df")])

;; Map vector pair mode to the base element mode.
(define_mode_attr VPAIR_ELEMENT [(V8SF  "SF")
				 (V4DF  "DF")])

;; Map vector pair mode to the base element mode in lower case.
(define_mode_attr vpair_element_l [(V8SF  "sf")
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
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

;; Vector initialization, set, extract
(define_expand "vec_init<mode><vpair_element_l>"
  [(match_operand:VPAIR 0 "vlogical_operand")
   (match_operand:VPAIR 1 "")]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
{
  rs6000_expand_vector_pair_init (operands[0], operands[1]);
  DONE;
})

(define_expand "vec_set<mode>"
  [(match_operand:VPAIR 0 "vlogical_operand")
   (match_operand:<VPAIR_ELEMENT> 1 "register_operand")
   (match_operand 2 "vec_set_index_operand")]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
{
  rs6000_expand_vector_pair_set (operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "vec_extract<mode><vpair_element_l>"
  [(match_operand:<VPAIR_ELEMENT> 0 "register_operand")
   (match_operand:VPAIR 1 "vlogical_operand")
   (match_operand 2 "const_int_operand")]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
{
  rs6000_expand_vector_pair_extract (operands[0], operands[1], operands[2]);
  DONE;
})

;; Assemble a vector pair from two vectors.  Unlike
;; __builtin_mma_assemble_pair, this function produces a vector pair output
;; directly and it takes all of the vector types.
;;
;; We cannot update the two output registers atomically, so mark the output as
;; an early clobber so we don't accidentally clobber the input operands.  */

(define_insn_and_split "vpair_assemble_<mode>"
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=&wa")
	(unspec:VPAIR
	 [(match_operand:<VPAIR_VECTOR> 1 "mma_assemble_input_operand" "mwajeP")
	  (match_operand:<VPAIR_VECTOR> 2 "mma_assemble_input_operand" "mwajeP")]
	 UNSPEC_VPAIR_ASSEMBLE))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  emit_insn (gen_vpair_assemble_<mode> (op0, tmp, tmp));
  DONE;
})
	     

;; Vector pair floating point arithmetic unary operations
(define_insn_and_split "<vpair_op><mode>2"
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa")
	(VPAIR_UNARY:VPAIR
	 (match_operand:VPAIR 1 "vsx_register_operand" "wa")))]
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
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa")
	(neg:VPAIR
	 (abs:VPAIR
	  (match_operand:VPAIR 1 "vsx_register_operand" "wa"))))]
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
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa")
	(VPAIR_BINARY:VPAIR
	 (match_operand:VPAIR 1 "vsx_register_operand" "wa")
	 (match_operand:VPAIR 2 "vsx_register_operand" "wa")))]
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
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa,wa")
	(fma:VPAIR
	 (match_operand:VPAIR 1 "vsx_register_operand" "%wa,wa")
	 (match_operand:VPAIR 2 "vsx_register_operand" "wa,0")
	 (match_operand:VPAIR 3 "vsx_register_operand" "0,wa")))]
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
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa,wa")
	(fma:VPAIR
	 (match_operand:VPAIR 1 "vsx_register_operand" "%wa,wa")
	 (match_operand:VPAIR 2 "vsx_register_operand" "wa,0")
	 (neg:VPAIR
	  (match_operand:VPAIR 3 "vsx_register_operand" "0,wa"))))]
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
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR
	 (fma:VPAIR
	  (match_operand:VPAIR 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR 2 "vsx_register_operand" "wa,0")
	  (match_operand:VPAIR 3 "vsx_register_operand" "0,wa"))))]
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
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR
	 (fma:VPAIR
	  (match_operand:VPAIR 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR 2 "vsx_register_operand" "wa,0")
	  (neg:VPAIR
	   (match_operand:VPAIR 3 "vsx_register_operand" "0,wa")))))]
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
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa,wa")
	(plus:VPAIR
	 (mult:VPAIR
	  (match_operand:VPAIR 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR 2 "vsx_register_operand" "wa,0"))
	 (match_operand:VPAIR 3 "vsx_register_operand" "0,wa")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
   && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(fma:VPAIR (match_dup 1)
		   (match_dup 2)
		   (match_dup 3)))]
{
}
  [(set_attr "length" "8")])

;; Optimize vector pair (a * b) - c into fma (a, b, -c)
(define_insn_and_split "*fms_fpcontract_<mode>4"
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa,wa")
	(minus:VPAIR
	 (mult:VPAIR
	  (match_operand:VPAIR 1 "vsx_register_operand" "%wa,wa")
	  (match_operand:VPAIR 2 "vsx_register_operand" "wa,0"))
	 (match_operand:VPAIR 3 "vsx_register_operand" "0,wa")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
   && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(fma:VPAIR (match_dup 1)
		   (match_dup 2)
		   (neg:VPAIR (match_dup 3))))]
{
}
  [(set_attr "length" "8")])

;; Optimize vector pair -((a * b) + c) into -fma (a, b, c)
(define_insn_and_split "*nfma_fpcontract_<mode>4"
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR
	 (plus:VPAIR
	  (mult:VPAIR
	   (match_operand:VPAIR 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:VPAIR 2 "vsx_register_operand" "wa,0"))
	  (match_operand:VPAIR 3 "vsx_register_operand" "0,wa"))))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
   && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(neg:VPAIR
	 (fma:VPAIR (match_dup 1)
		    (match_dup 2)
		    (match_dup 3))))]
{
}
  [(set_attr "length" "8")])

;; Optimize vector pair -((a * b) - c) into -fma (a, b, -c)
(define_insn_and_split "*nfms_fpcontract_<mode>4"
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa,wa")
	(neg:VPAIR
	 (minus:VPAIR
	  (mult:VPAIR
	   (match_operand:VPAIR 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:VPAIR 2 "vsx_register_operand" "wa,0"))
	  (match_operand:VPAIR 3 "vsx_register_operand" "0,wa"))))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
   && flag_fp_contract_mode == FP_CONTRACT_FAST"
  "#"
  "&& 1"
  [(set (match_dup 0)
	(neg:VPAIR
	 (fma:VPAIR (match_dup 1)
		    (match_dup 2)
		    (neg:VPAIR (match_dup 3)))))]
{
}
  [(set_attr "length" "8")])
