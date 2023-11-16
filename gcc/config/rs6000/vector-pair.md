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
(define_mode_attr vpair_vector [(V8SF  "v4sf")
				(V4DF  "v2df")])

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
  [(set (match_operand:VPAIR 0 "nonimmediate_operand" "=wa,wa,ZwO,QwO,wa")
	(match_operand:VPAIR 1 "input_operand" "ZwO,QwO,wa,wa,wa"))]
  "TARGET_MMA && TARGET_VECTOR_SIZE_32
   && (gpc_reg_operand (operands[0], <MODE>mode)
       || gpc_reg_operand (operands[1], <MODE>mode))"
  "@
   lxvp%X1 %x0,%1
   #
   stxvp%X0 %x1,%0
   #
   #"
  "&& reload_completed
   && ((MEM_P (operands[0]) && !TARGET_STORE_VECTOR_PAIR)
       || (MEM_P (operands[1]) && !TARGET_LOAD_VECTOR_PAIR)
       || (!MEM_P (operands[0]) && !MEM_P (operands[1])))"
  [(const_int 0)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecload,vecstore,vecstore,veclogical")
   (set_attr "size" "256")
   (set_attr "length" "*,8,*,8,8")
   (set_attr "isa" "lxvp,*,stxvp,*,*")])


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
			   gen_<vpair_op><vpair_vector>2);
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
			   gen_vsx_nabs<vpair_vector>2);
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
			    gen_<vpair_op><vpair_vector>3);
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
			 gen_fma<vpair_vector>4);
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
			 gen_fms<vpair_vector>4);
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
			 gen_nfma<vpair_vector>4);
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
			 gen_nfms<vpair_vector>4);
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
