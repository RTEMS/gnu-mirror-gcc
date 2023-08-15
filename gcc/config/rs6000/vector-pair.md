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

;; Iterator for the floating point vector pair modes
(define_mode_iterator VPAIR_FP [V8SF V4DF])

;; Iterator doing unary/binary arithmetic on vector pairs.  Split it into
;; integer and floating point operations.
(define_code_iterator VPAIR_INT_UNARY   [neg abs not])
(define_code_iterator VPAIR_INT_BINARY  [plus minus smin smax])
(define_code_iterator VPAIR_INT_LOGICAL [and ior xor])

(define_code_iterator VPAIR_FP_UNARY  [neg abs sqrt])
(define_code_iterator VPAIR_FP_BINARY [plus minus mult div copysign smin smax])

;; Give the insn name from the opertion
(define_code_attr vpair_op [(abs      "abs")
			    (and      "and")
			    (copysign "copysign")
			    (div      "div")
			    (fma      "fma")
			    (ior      "ior")
			    (minus    "sub")
			    (mult     "mul")
			    (not      "not")
			    (neg      "neg")
			    (plus     "add")
			    (smin     "smin")
			    (smax     "smax")
			    (sqrt     "sqrt")
			    (umin     "umin")
			    (umax     "umax")
			    (xor      "xor")])

;; Map vector pair to vector
(define_mode_attr VPAIR_VECT [(V32QI "V16QI")
			      (V16HI "V8HI")
			      (V8SI  "V4SI")
			      (V4DI  "V2DI")
			      (V8SF  "V4SF")
			      (V4DF  "V2DF")])


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
  [(set (match_dup 2) (VPAIR_FP_UNARY:<VPAIR_VECT> (match_dup 3)))
   (set (match_dup 4) (VPAIR_FP_UNARY:<VPAIR_VECT> (match_dup 5)))]
{
  machine_mode vmode = <VPAIR_VECT>mode;
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);

  operands[2] = gen_rtx_REG (vmode, reg0);
  operands[3] = gen_rtx_REG (vmode, reg1);
  operands[4] = gen_rtx_REG (vmode, reg0 + 1);
  operands[5] = gen_rtx_REG (vmode, reg1 + 1);
}
  [(set_attr "length" "8")])

;; Optimize negative absolute value (both floating point and integer)
(define_insn_and_split "nabs<mode>2"
  [(set (match_operand:VPAIR 0 "vsx_register_operand" "=wa")
	(neg:VPAIR
	 (abs:VPAIR
	  (match_operand:VPAIR 1 "vsx_register_operand" "wa"))))]
  "TARGET_VECTOR_PAIR"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(neg:<VPAIR_VECT>
	 (abs:<VPAIR_VECT> (match_dup 3))))
   (set (match_dup 4)
	(neg:<VPAIR_VECT>
	 (abs:<VPAIR_VECT> (match_dup 5))))]
{
  machine_mode vmode = <VPAIR_VECT>mode;
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);

  operands[2] = gen_rtx_REG (vmode, reg0);
  operands[3] = gen_rtx_REG (vmode, reg1);
  operands[4] = gen_rtx_REG (vmode, reg0 + 1);
  operands[5] = gen_rtx_REG (vmode, reg1 + 1);
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
  [(set (match_dup 3)
	(VPAIR_FP_BINARY:<VPAIR_VECT> (match_dup 4)
				      (match_dup 5)))
   (set (match_dup 6)
	(VPAIR_FP_BINARY:<VPAIR_VECT> (match_dup 7)
				      (match_dup 8)))]
{
  machine_mode vmode = <VPAIR_VECT>mode;
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
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
  [(set (match_dup 4)
	(fma:<VPAIR_VECT> (match_dup 5)
			  (match_dup 6)
			  (match_dup 7)))
   (set (match_dup 8)
	(fma:<VPAIR_VECT> (match_dup 9)
			  (match_dup 10)
			  (match_dup 11)))]
{
  machine_mode vmode = <VPAIR_VECT>mode;
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  unsigned reg3 = reg_or_subregno (operands[3]);

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
  [(set (match_dup 4)
	(fma:<VPAIR_VECT> (match_dup 5)
			  (match_dup 6)
			  (neg:VPAIR_FP
			   (match_dup 7))))
   (set (match_dup 8)
	(fma:<VPAIR_VECT> (match_dup 9)
			  (match_dup 10)
			  (neg:VPAIR_FP
			   (match_dup 11))))]
{
  machine_mode vmode = <VPAIR_VECT>mode;
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  unsigned reg3 = reg_or_subregno (operands[3]);

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
  [(set (match_dup 4)
	(neg:<VPAIR_VECT>
	 (fma:<VPAIR_VECT> (match_dup 5)
			   (match_dup 6)
			   (match_dup 7))))
   (set (match_dup 8)
	(neg:<VPAIR_VECT>
	 (fma:<VPAIR_VECT> (match_dup 9)
			   (match_dup 10)
			   (match_dup 11))))]
{
  machine_mode vmode = <VPAIR_VECT>mode;
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  unsigned reg3 = reg_or_subregno (operands[3]);

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
  [(set (match_dup 4)
	(neg:<VPAIR_VECT>
	 (fma:<VPAIR_VECT> (match_dup 5)
			   (match_dup 6)
			   (neg:VPAIR_FP
			    (match_dup 7)))))
   (set (match_dup 8)
	(neg:<VPAIR_VECT>
	 (fma:<VPAIR_VECT> (match_dup 9)
			   (match_dup 10)
			   (neg:VPAIR_FP
			    (match_dup 11)))))]
{
  machine_mode vmode = <VPAIR_VECT>mode;
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);
  unsigned reg3 = reg_or_subregno (operands[3]);

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
  [(set (match_dup 2) (VPAIR_INT_UNARY:<VPAIR_VECT> (match_dup 3)))
   (set (match_dup 4) (VPAIR_INT_UNARY:<VPAIR_VECT> (match_dup 5)))]
{
  machine_mode vmode = <VPAIR_VECT>mode;
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);

  operands[2] = gen_rtx_REG (vmode, reg0);
  operands[3] = gen_rtx_REG (vmode, reg1);
  operands[4] = gen_rtx_REG (vmode, reg0 + 1);
  operands[5] = gen_rtx_REG (vmode, reg1 + 1);
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
  [(set (match_dup 3)
	(VPAIR_INT_BINARY:<VPAIR_VECT> (match_dup 4)
				      (match_dup 5)))
   (set (match_dup 6)
	(VPAIR_INT_BINARY:<VPAIR_VECT> (match_dup 7)
				      (match_dup 8)))]
{
  machine_mode vmode = <VPAIR_VECT>mode;
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
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
  [(set (match_dup 3)
	(VPAIR_INT_LOGICAL:<VPAIR_VECT> (match_dup 4)
					(match_dup 5)))
   (set (match_dup 6)
	(VPAIR_INT_LOGICAL:<VPAIR_VECT> (match_dup 7)
					(match_dup 8)))]
{
  machine_mode vmode = <VPAIR_VECT>mode;
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);

  operands[3] = gen_rtx_REG (vmode, reg0);
  operands[4] = gen_rtx_REG (vmode, reg1);
  operands[5] = gen_rtx_REG (vmode, reg2);

  operands[6] = gen_rtx_REG (vmode, reg0 + 1);
  operands[7] = gen_rtx_REG (vmode, reg1 + 1);
  operands[8] = gen_rtx_REG (vmode, reg2 + 1);
}
  [(set_attr "length" "8")])
