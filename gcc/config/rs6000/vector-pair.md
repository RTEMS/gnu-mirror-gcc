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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
{
  rs6000_emit_move (operands[0], operands[1], <MODE>mode);
  DONE;
})

(define_insn_and_split "*mov<mode>"
  [(set (match_operand:VPAIR 0 "nonimmediate_operand" "=wa,m,wa,wa,wa")
	(match_operand:VPAIR 1 "input_operand" "m,wa,wa,j,eP"))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
   && (gpc_reg_operand (operands[0], <MODE>mode)
       || gpc_reg_operand (operands[1], <MODE>mode))"
{
  if (MEM_P (operands[0]))
    return TARGET_STORE_VECTOR_PAIR ? "stxvp%X0 %x1,%0" : "#";

  if (MEM_P (operands[1]))
    return TARGET_LOAD_VECTOR_PAIR ? "lxvp%X1 %x0,%1" : "#";

  return "#";
}
  "&& reload_completed
   && ((MEM_P (operands[0]) && !TARGET_STORE_VECTOR_PAIR)
       || (MEM_P (operands[1]) && !TARGET_LOAD_VECTOR_PAIR)
       || (!MEM_P (operands[0]) && !MEM_P (operands[1])))"
  [(const_int 0)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecstore,veclogical,vecperm,vecperm")
   (set_attr "size" "256")
   (set_attr "length" "*,*,8,8,40")])


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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
(define_insn_and_split "*fms_fpcontract_<mode>3"
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
(define_insn_and_split "*nfma_fpcontract_<mode>3"
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
(define_insn_and_split "*nfms_fpcontract_<mode>3"
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


;; Vector pair integer arithmetic unary operations
(define_insn_and_split "<vpair_op><mode>2"
  [(set (match_operand:VPAIR_INT 0 "vsx_register_operand" "=wa")
	(VPAIR_INT_UNARY:VPAIR_INT
	 (match_operand:VPAIR_INT 1 "vsx_register_operand" "wa")))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
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
  "TARGET_MMA && TARGET_VECTOR_SIZE_32"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_binary_vector_pair (<VPAIR_VECTOR>mode, operands,
			    gen_orc<vpair_vector>3);
  DONE;
}
  [(set_attr "length" "8")])
