;; Machine description for X-Ventana-CondOps
;; Copyright (C) 2022 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_code_iterator eq_or_ne [eq ne])
(define_code_attr n [(eq "n") (ne "")])

(define_insn "*vt.maskc<n>"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (neg:DI (eq_or_ne:DI
			(match_operand:DI 1 "register_operand" "r")
			(const_int 0)))
		(match_operand:DI 2 "register_operand" "r")))]
  "TARGET_XVENTANACONDOPS"
  "vt.maskc<n>\t%0,%2,%1")

;; Make order operators digestible to the vt.maskc<n> logic by
;; wrapping their result in a comparison against (const_int 0).

;; "a >= b" is "!(a < b)"
(define_split
  [(set (match_operand:X 0 "register_operand")
	(and:X (neg:X (match_operator:X 1 "anyge_operator"
			     [(match_operand:X 2 "register_operand")
			      (match_operand:X 3 "register_operand")]))
	       (match_operand:X 4 "register_operand")))
   (clobber (match_operand:X 5 "register_operand"))]
  "TARGET_XVENTANACONDOPS"
  [(set (match_dup 5) (match_dup 6))
   (set (match_dup 0) (and:X (neg:X (eq:X (match_dup 5) (const_int 0)))
			     (match_dup 4)))]
{
  operands[6] = gen_rtx_fmt_ee (GET_CODE (operands[1]) == GE ? LT : LTU,
				<X:MODE>mode, operands[2], operands[3]);
})

;; "a > b"
(define_split
  [(set (match_operand:X 0 "register_operand")
	(and:X (neg:X (match_operator:X 1 "anygt_operator"
			     [(match_operand:X 2 "register_operand")
			      (match_operand:X 3 "register_operand")]))
	       (match_operand:X 4 "register_operand")))
   (clobber (match_operand:X 5 "register_operand"))]
  "TARGET_XVENTANACONDOPS"
  [(set (match_dup 5) (match_dup 1))
   (set (match_dup 0) (and:X (neg:X (ne:X (match_dup 5) (const_int 0)))
			     (match_dup 4)))])

;; "a <= b" is "!(a > b)"
(define_split
  [(set (match_operand:X 0 "register_operand")
	(and:X (neg:X (match_operator:X 1 "anyle_operator"
			     [(match_operand:X 2 "register_operand")
			      (match_operand:X 3 "arith_operand")]))
	       (match_operand:X 4 "register_operand")))
   (clobber (match_operand:X 5 "register_operand"))]
  "TARGET_XVENTANACONDOPS"
  [(set (match_dup 5) (match_dup 6))
   (set (match_dup 0) (and:X (neg:X (eq:X (match_dup 5) (const_int 0)))
			     (match_dup 4)))]
{
  operands[6] = gen_rtx_fmt_ee (GET_CODE (operands[1]) == LE ? GT : GTU,
				<X:MODE>mode, operands[2], operands[3]);
})

;; Users might use explicit arithmetic operations to create a mask and
;; then and it, in a sequence like
;;    cond = (bits >> SHIFT) & 1;
;;    mask = ~(cond - 1);
;;    val &= mask;
;; which will present as a single-bit sign-extract in the combiner.
;;
;; This will give rise to any of the following cases:
;; - with Zbs and XVentanaCondOps: bexti + vt.maskc
;; - with XVentanaCondOps (but w/o Zbs):
;;   - andi + vt.maskc, if the mask is representable in the immediate
;;                      (which requires extra care due to the immediate
;;                       being sign-extended)
;;   - slli + srli + and
;; - otherwise: slli + srli + and

;; With Zbb, we have bexti for all possible bits...
(define_split
  [(set (match_operand:X 0 "register_operand")
	(and:X (sign_extract:X (match_operand:X 1 "register_operand")
			       (const_int 1)
			       (match_operand 2 "immediate_operand"))
	       (match_operand:X 3 "register_operand")))
   (clobber (match_operand:X 4 "register_operand"))]
  "TARGET_XVENTANACONDOPS && TARGET_ZBS"
  [(set (match_dup 4) (zero_extract:X (match_dup 1) (const_int 1) (match_dup 2)))
   (set (match_dup 0) (and:X (neg:X (ne:X (match_dup 4) (const_int 0)))
			     (match_dup 3)))])

;; ...whereas RV64I only allows us access to bits 0..10 in a single andi.
(define_split
  [(set (match_operand:X 0 "register_operand")
	(and:X (sign_extract:X (match_operand:X 1 "register_operand")
			       (const_int 1)
			       (match_operand 2 "immediate_operand"))
	       (match_operand:X 3 "register_operand")))
   (clobber (match_operand:X 4 "register_operand"))]
  "TARGET_XVENTANACONDOPS && !TARGET_ZBS && (UINTVAL (operands[2]) < 11)"
  [(set (match_dup 4) (and:X (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (and:X (neg:X (ne:X (match_dup 4) (const_int 0)))
			     (match_dup 3)))]
{
  operands[2] = GEN_INT(1 << UINTVAL(operands[2]));
})
