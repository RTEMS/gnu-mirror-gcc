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
