;; Vector Quad / MMA patterns.
;; Copyright (C) 2019 Free Software Foundation, Inc.
;; Contributed by Peter Bergner <bergner@linux.ibm.com> and
;;		  Michael Meissner <meissner@linux.ibm.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Iterator for the modes used for vector pairs.  We don't define XImode unless
;; we have vector pair instructions enabled, because it will cause
;; gcc.c-torture/execute/pr65427.c to fail (due to use of vector_size(32) and
;; endianess issues).  We do define V4TI mode even if vector pairs are not
;; enabled, so that the __vector_quad keyword can be used.

(define_mode_iterator VQUAD	[(V4TI "TARGET_FUTURE")
				 (XI   "TARGET_VECTOR_256BIT")])

;; Vector quad.  Vector quad mode is only defined for FPR registers, because
;; the MMA instructions that use vector quad mode only uses FPR registers.
;; TBD, check the ordering for the loads or stores.
;; Iterator for the modes used for vector pairs
(define_expand "mov<mode>"
  [(set (match_operand:VQUAD 0 "nonimmediate_operand")
	(match_operand:VQUAD 1 "input_operand"))]
  ""
{
  if (!gpc_reg_operand (operands[0], <MODE>mode)
      && !gpc_reg_operand (operands[1], <MODE>mode))
    operands[1] = force_reg (<MODE>mode, operands[1]);
})

(define_insn_and_split "*mov<mode>"
  [(set (match_operand:VQUAD 0 "nonimmediate_operand" "=d,o,d")
	(match_operand:VQUAD 1 "input_operand" "o,d,d"))]
  "TARGET_VECTOR_256BIT
   && (gpc_reg_operand (operands[0], <MODE>mode)
       || gpc_reg_operand (operands[1], <MODE>mode))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecstore,veclogical")
   (set_attr "prefixed_length" "16")
   (set_attr "non_prefixed_length" "8,8,16")])

(define_insn_and_split "*movv4ti_no_vector_pair"
  [(set (match_operand:V4TI 0 "nonimmediate_operand" "=d,o,d")
	(match_operand:V4TI 1 "input_operand" "o,d,d"))]
  "TARGET_FUTURE && !TARGET_VECTOR_256BIT
   && (gpc_reg_operand (operands[0], V4TImode)
       || gpc_reg_operand (operands[1], V4TImode))"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rs6000_split_multireg_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "vecload,vecstore,veclogical")
   (set_attr "prefixed_length" "36,36,16")
   (set_attr "non_prefixed_length" "16")])


