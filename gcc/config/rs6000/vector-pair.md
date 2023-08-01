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
  [UNSPEC_VPAIR_V4DF
   UNSPEC_VPAIR_V8SF
   UNSPEC_VPAIR_V32QI
   UNSPEC_VPAIR_V16HI
   UNSPEC_VPAIR_V8SI
   UNSPEC_VPAIR_V4DI
   UNSPEC_REDUCE_F32
   UNSPEC_REDUCE_F64
   ])

;; Iterator doing unary/binary arithmetic on vector pairs
(define_code_iterator VPAIR_UNARY  [neg abs sqrt])
(define_code_iterator VPAIR_BINARY [plus minus mult div copysign smin smax])

;; Give the insn name from the opertion
(define_code_attr vpair_op [(abs      "abs")
			    (copysign "copysign")
			    (div      "div")
			    (minus    "sub")
			    (mult     "mul")
			    (neg      "neg")
			    (plus     "add")
			    (smin     "smin")
			    (smax     "smax")
			    (sqrt     "sqrt")])

;; Iterator for creating the wrapper for vector pair built-ins
(define_int_iterator VPAIR_WRAPPER [UNSPEC_VPAIR_V4DF UNSPEC_VPAIR_V8SF])

;; Map VPAIR_WRAPPER to vector type (i.e. V2DF or V4SF)
(define_int_attr VPAIR_VECTOR [(UNSPEC_VPAIR_V4DF "V2DF")
			       (UNSPEC_VPAIR_V8SF "V4SF")])

(define_int_attr vpair_type [(UNSPEC_VPAIR_V4DF "v4df")
			     (UNSPEC_VPAIR_V8SF "v8sf")])


;; Vector pair floating point unary operations
(define_insn_and_split "vpair_<vpair_op><vpair_type>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(VPAIR_UNARY:OO
		     (match_operand:OO 1 "vsx_register_operand" "wa"))]
		   VPAIR_WRAPPER))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 2) (VPAIR_UNARY:<VPAIR_VECTOR> (match_dup 3)))
   (set (match_dup 4) (VPAIR_UNARY:<VPAIR_VECTOR> (match_dup 5)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);

  operands[2] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0);
  operands[3] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1);
  operands[4] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0 + 1);
  operands[5] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1 + 1);
}
  [(set_attr "length" "8")])

;; Optimize vector pair negate of absolute value
(define_insn_and_split "vpair_nabs<vpair_type>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(abs:OO (match_operand:OO 1 "vsx_register_operand" "ww"))]
	    VPAIR_WRAPPER))]
	 VPAIR_WRAPPER))]
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

  operands[2] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0);
  operands[3] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1);
  operands[4] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0 + 1);
  operands[5] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1 + 1);
}
  [(set_attr "length" "8")])

;; Vector pair floating binary operations
(define_insn_and_split "vpair_<vpair_op><vpair_type>3"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO [(VPAIR_BINARY:OO
		     (match_operand:OO 1 "vsx_register_operand" "wa")
		     (match_operand:OO 2 "vsx_register_operand" "wa"))]
		   VPAIR_WRAPPER))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 3)
	(VPAIR_BINARY:<VPAIR_VECTOR> (match_dup 4)
				      (match_dup 5)))
   (set (match_dup 6)
	(VPAIR_BINARY:<VPAIR_VECTOR> (match_dup 7)
				      (match_dup 8)))]
{
  unsigned reg0 = reg_or_subregno (operands[0]);
  unsigned reg1 = reg_or_subregno (operands[1]);
  unsigned reg2 = reg_or_subregno (operands[2]);

  operands[3] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0);
  operands[4] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1);
  operands[5] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg2);

  operands[6] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0 + 1);
  operands[7] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1 + 1);
  operands[8] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg2 + 1);
}
  [(set_attr "length" "8")])

;; Vector pair fused multiply-add floating point operations
(define_insn_and_split "vpair_fma<vpair_type>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(fma:OO
	   (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:OO 2 "vsx_register_operand" "wa,0")
	   (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	 VPAIR_WRAPPER))]
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

  operands[4] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0);
  operands[5] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1);
  operands[6] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg2);
  operands[7] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg3);

  operands[8] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0 + 1);
  operands[9] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1 + 1);
  operands[10] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg2 + 1);
  operands[11] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg3 + 1);
}
  [(set_attr "length" "8")])

(define_insn_and_split "vpair_fms<vpair_type>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(fma:OO
	   (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	   (match_operand:OO 2 "vsx_register_operand" "wa,0")
	   (unspec:OO
	    [(neg:OO (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	     VPAIR_WRAPPER))]
	 VPAIR_WRAPPER))]
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

  operands[4] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0);
  operands[5] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1);
  operands[6] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg2);
  operands[7] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg3);

  operands[8] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0 + 1);
  operands[9] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1 + 1);
  operands[10] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg2 + 1);
  operands[11] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg3 + 1);
}
  [(set_attr "length" "8")])

(define_insn_and_split "vpair_nfma<vpair_type>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(fma:OO
	      (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")
	      (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	    VPAIR_WRAPPER))]
	 VPAIR_WRAPPER))]
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

  operands[4] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0);
  operands[5] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1);
  operands[6] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg2);
  operands[7] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg3);

  operands[8] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0 + 1);
  operands[9] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1 + 1);
  operands[10] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg2 + 1);
  operands[11] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg3 + 1);
}
  [(set_attr "length" "8")])

(define_insn_and_split "vpair_nfms<vpair_type>4"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa,wa")
	(unspec:OO
	 [(neg:OO
	   (unspec:OO
	    [(fma:OO
	      (match_operand:OO 1 "vsx_register_operand" "%wa,wa")
	      (match_operand:OO 2 "vsx_register_operand" "wa,0")
	      (unspec:OO
	       [(neg:OO (match_operand:OO 3 "vsx_register_operand" "0,wa"))]
	       VPAIR_WRAPPER))]
	   VPAIR_WRAPPER))]
	 VPAIR_WRAPPER))]
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

  operands[4] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0);
  operands[5] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1);
  operands[6] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg2);
  operands[7] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg3);

  operands[8] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg0 + 1);
  operands[9] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg1 + 1);
  operands[10] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg2 + 1);
  operands[11] = gen_rtx_REG (<VPAIR_VECTOR>mode, reg3 + 1);
}
  [(set_attr "length" "8")])

;; Reduction for a V4SF vector
(define_insn_and_split "reduce_v4sf"
  [(set (match_operand:SF 0 "vsx_register_operand" "=wa")
	(unspec:SF [(match_operand:V4SF 1 "vsx_register_operand" "v")]
		   UNSPEC_REDUCE_F32))
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

  emit_insn (gen_altivec_vsldoi_v4sf (tmp1, op1, op1, GEN_INT (8)));
  emit_insn (gen_addv4sf3 (tmp1, op1, tmp1));
  emit_insn (gen_altivec_vsldoi_v4sf (tmp2, tmp1, tmp1, GEN_INT (4)));
  emit_insn (gen_addv4sf3 (tmp2, tmp1, tmp2));
  emit_insn (gen_vsx_xscvspdp_scalar2 (op0, tmp2));
  DONE;
}
  [(set_attr "length" "24")])

;; Reduction for a pair of V4SF vectors
(define_insn_and_split "reduce_v8sf"
  [(set (match_operand:SF 0 "vsx_register_operand" "=wa")
	(unspec:SF [(match_operand:OO 1 "vsx_register_operand" "v")]
		   UNSPEC_REDUCE_F32))
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

;; Reduction for a V2DF vector
(define_insn_and_split "reduce_v2df"
  [(set (match_operand:DF 0 "vsx_register_operand" "=&wa")
	(unspec:DF [(match_operand:V2DF 1 "vsx_register_operand" "wa")]
		   UNSPEC_REDUCE_F64))
   (clobber (match_scratch:DF 2 "=&wa"))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(vec_select:DF (match_dup 1)
		       (parallel [(match_dup 3)])))
   (set (match_dup 0)
	(plus:DF (match_dup 4)
		 (match_dup 2)))]
{
  unsigned reg1 = reg_or_subregno (operands[1]);

  operands[3] = GEN_INT (BYTES_BIG_ENDIAN ? 1 : 0);
  operands[4] = gen_rtx_REG (DFmode, reg1);
})

;; Reduction for a pair of V2DF vectors
(define_insn_and_split "reduce_v4df"
  [(set (match_operand:DF 0 "vsx_register_operand" "=&wa")
	(unspec:DF [(match_operand:OO 1 "vsx_register_operand" "wa")]
		   UNSPEC_REDUCE_F64))
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
  unsigned reg1 = REGNO (operands[1]);
  unsigned reg3 = REGNO (operands[3]);

  operands[4] = gen_rtx_REG (V2DFmode, reg1);
  operands[5] = gen_rtx_REG (V2DFmode, reg1 + 1);
  operands[6] = GEN_INT (BYTES_BIG_ENDIAN ? 1 : 0);
  operands[7] = gen_rtx_REG (DFmode, reg3);
})
