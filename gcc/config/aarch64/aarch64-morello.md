;; AArch64 ldp/stp peephole optimizations.
;; Copyright (C) 2021 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

; TODO: check syntax is OK for TARGET_MORELLO.
; TODO: more alternatives.
(define_insn "pointer_plus_cadi"
  [(set (match_operand:CADI 0 "register_operand" "=rk")
	(pointer_plus:CADI
	  (match_operand:CADI 1 "register_operand" "rk")
	  (match_operand:DI 2 "aarch64_pluslong_operand" "IrJ")))]
  "TARGET_CAPABILITY_ANY"
  "add\\t%0, %1, %2"
)

; TODO: many more alternatives.
; TODO: pure-cap.
(define_insn "*movcadi_aarch64"
  [(set (match_operand:CADI 0 "nonimmediate_operand" "=rk,r,r,m,r,r")
	(match_operand:CADI 1 "aarch64_mov_operand" "rk,Z,m,r,Usa,Ush"))]
  "TARGET_CAPABILITY_FAKE"
  "@
   mov\\t%0, %1
   mov\\t%0, 0
   ldr\\t%0, %1
   str\\t%1, %0
   adr\\t%0, %c1
   adrp\\t%0, %A1"
)

/* MORELLO TODO (just a hack to ensure the relevant optab is doing what it
should).  */
(define_insn "replace_address_value_cadi"
  [(set (match_operand:CADI 0 "register_operand" "=r")
        (replace_address_value:CADI
	      (match_operand:CADI 1 "register_operand" "r")
	      (match_operand:DI 2 "register_operand" "r")))]
  ""
  "mov\\t%0, %2"
)
