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
  [UNSPEC_VPAIR_ABS
   UNSPEC_VPAIR_DIV
   UNSPEC_VPAIR_MINUS
   UNSPEC_VPAIR_MULT
   UNSPEC_VPAIR_NEG
   UNSPEC_VPAIR_PLUS
   UNSPEC_VPAIR_SMAX
   UNSPEC_VPAIR_SMIN])

;; Vector pair element ID that defines the scaler element within the vector pair.
(define_c_enum "vpair_element"
  [VPAIR_ELEMENT_FLOAT
   VPAIR_ELEMENT_DOUBLE])

(define_int_iterator VPAIR_FP_ELEMENT [VPAIR_ELEMENT_FLOAT
				       VPAIR_ELEMENT_DOUBLE])

;; Map vector pair element ID to the vector mode after the vector pair has been
;; split.
(define_int_attr VPAIR_VMODE [(VPAIR_ELEMENT_FLOAT  "V4SF")
			      (VPAIR_ELEMENT_DOUBLE "V2DF")])

;; Map vector pair element ID to the name used on the define_insn (in lower
;; case).
(define_int_attr vpair_modename [(VPAIR_ELEMENT_FLOAT  "v8sf")
				 (VPAIR_ELEMENT_DOUBLE "v4df")])

;; Unary/binary arithmetic iterator on vector pairs.
(define_int_iterator VPAIR_FP_UNARY  [UNSPEC_VPAIR_ABS
				      UNSPEC_VPAIR_NEG])

(define_int_iterator VPAIR_FP_BINARY [UNSPEC_VPAIR_DIV
				      UNSPEC_VPAIR_MINUS
				      UNSPEC_VPAIR_MULT
				      UNSPEC_VPAIR_PLUS
				      UNSPEC_VPAIR_SMAX
				      UNSPEC_VPAIR_SMIN])

;; Map the vpair operator unspec number to the standard name.
(define_int_attr vpair_stdname [(UNSPEC_VPAIR_ABS    "abs")
				(UNSPEC_VPAIR_DIV    "div")
				(UNSPEC_VPAIR_MINUS  "sub")
				(UNSPEC_VPAIR_MULT   "mul")
				(UNSPEC_VPAIR_NEG    "neg")
				(UNSPEC_VPAIR_PLUS   "add")
				(UNSPEC_VPAIR_SMAX   "smax")
				(UNSPEC_VPAIR_SMIN   "smin")])

;; Map the vpair operator unspec number to the RTL operator.
(define_int_attr VPAIR_OP [(UNSPEC_VPAIR_ABS    "ABS")
			   (UNSPEC_VPAIR_DIV    "DIV")
			   (UNSPEC_VPAIR_MINUS  "MINUS")
			   (UNSPEC_VPAIR_MULT   "MULT")
			   (UNSPEC_VPAIR_NEG    "NEG")
			   (UNSPEC_VPAIR_PLUS   "PLUS")
			   (UNSPEC_VPAIR_SMAX   "SMAX")
			   (UNSPEC_VPAIR_SMIN   "SMIN")])

;; Map the scalar element ID into the appropriate insn type.
(define_int_attr vpair_type [(VPAIR_ELEMENT_FLOAT  "vecfloat")
			     (VPAIR_ELEMENT_DOUBLE "vecdouble")])

;; Map the scalar element ID into the appropriate insn type for divide.
(define_int_attr vpair_divtype [(VPAIR_ELEMENT_FLOAT  "vecfdiv")
				(VPAIR_ELEMENT_DOUBLE "vecdiv")])

;; Vector pair unary operations.  The last argument in the UNSPEC is a
;; CONST_INT which identifies what the scalar element is.
(define_insn_and_split "vpair_<vpair_stdname>_<vpair_modename>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "wa")
	  (const_int VPAIR_FP_ELEMENT)]
	 VPAIR_FP_UNARY))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_unary (operands, <VPAIR_VMODE>mode, <VPAIR_OP>,
		     VPAIR_SPLIT_NORMAL);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Optimize vector pair (neg (abs)).
(define_insn_and_split "vpair_nabs_<vpair_modename>2"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(unspec:OO
	   [(match_operand:OO 1 "vsx_register_operand" "wa")
	    (const_int VPAIR_FP_ELEMENT)]
	   UNSPEC_VPAIR_ABS)
	  (const_int VPAIR_FP_ELEMENT)]
	 UNSPEC_VPAIR_NEG))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_unary (operands, <VPAIR_VMODE>mode, ABS, VPAIR_SPLIT_NEGATE);
  DONE;
}
  [(set_attr "length" "8")
   (set_attr "type" "<vpair_type>")])

;; Vector pair binary operations.  The last argument in the UNSPEC is a
;; CONST_INT which identifies what the scalar element is.
(define_insn_and_split "vpair_<vpair_stdname>_<vpair_modename>3"
  [(set (match_operand:OO 0 "vsx_register_operand" "=wa")
	(unspec:OO
	 [(match_operand:OO 1 "vsx_register_operand" "wa")
	  (match_operand:OO 2 "vsx_register_operand" "wa")
	  (const_int VPAIR_FP_ELEMENT)]
	 VPAIR_FP_BINARY))]
  "TARGET_MMA"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  vpair_split_binary (operands, <VPAIR_VMODE>mode, <VPAIR_OP>);
  DONE;
}
  [(set_attr "length" "8")
   (set (attr "type") (if_then_else (match_test "<VPAIR_OP> == DIV")
				    (const_string "<vpair_divtype>")
				    (const_string "<vpair_type>")))])
