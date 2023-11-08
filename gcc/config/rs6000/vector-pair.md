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
   ])

;; Iterator doing unary/binary arithmetic on vector pairs
(define_code_iterator VP_FP_UNARY  [abs neg])
(define_code_iterator VP_FP_BINARY [minus mult plus smin smax])

;; Return the insn name from the VP_* code iterator
(define_code_attr vp_insn [(abs      "abs")
			   (minus    "sub")
			   (mult     "mul")
			   (neg      "neg")
			   (plus     "add")
			   (smin     "smin")
			   (smax     "smax")
			   (xor      "xor")])

;; Iterator for creating the unspecs for vector pair built-ins
(define_int_iterator VP_FP [UNSPEC_VPAIR_V4DF
			    UNSPEC_VPAIR_V8SF])

;; Map VP_* to vector mode of the arguments after they are split
(define_int_attr VP_VEC_MODE [(UNSPEC_VPAIR_V4DF  "V2DF")
			      (UNSPEC_VPAIR_V8SF  "V4SF")])

;; Map VP_* to a lower case name to identify the vector pair.
(define_int_attr vp_pmode [(UNSPEC_VPAIR_V4DF  "v4df")
			   (UNSPEC_VPAIR_V8SF  "v8sf")])

;; Map VP_* to a lower case name to identify the vector after the vector pair
;; has been split.
(define_int_attr vp_vmode [(UNSPEC_VPAIR_V4DF  "v2df")
			   (UNSPEC_VPAIR_V8SF  "v4sf")])


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
