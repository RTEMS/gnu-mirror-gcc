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

;; We need to define an XImode move pattern, even though we don't enable it,
;; because the machine independent parts of the compiler at times uses the
;; large integer modes.
;;
;; If we enable movxi, the compiler will try and use it.  Unfortunately, if it
;; is enabled, it will cause problems on little endian systems with code that
;; uses the vector_size attribute, due to endian issues.
(define_expand "movxi"
  [(set (match_operand:XI 0 "nonimmediate_operand")
	(match_operand:XI 1 "input_operand"))]
  "0"
{
  gcc_unreachable ();
})

;; Vector pair support.  V2TImode is only defined for vector registers.
(define_expand "movv4ti"
  [(set (match_operand:V4TI 0 "nonimmediate_operand")
	(match_operand:V4TI 1 "input_operand"))]
  "TARGET_FUTURE"
{
  if (!gpc_reg_operand (operands[0], V4TImode)
      && !gpc_reg_operand (operands[1], V4TImode))
    operands[1] = force_reg (V4TImode, operands[1]);
})

(define_insn_and_split "*movv4ti_vector_pair"
  [(set (match_operand:V4TI 0 "nonimmediate_operand" "=d,m,d")
	(match_operand:V4TI 1 "input_operand" "m,d,d"))]
  "TARGET_VECTOR_256BIT
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
   (set_attr "prefixed_length" "20")
   (set_attr "non_prefixed_length" "8,8,16")])

;; We define move patterns in case we do not have the vector pair instructions
;; enabled, so that the __vector_pair keyword can be used.
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
