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
  [(set (match_operand:CADI 0 "register_operand" "=rk,rk")
	(pointer_plus:CADI
	  (match_operand:CADI 1 "register_operand" "rk,rk")
	  (match_operand:DI 2 "aarch64_pluslong_operand" "Ir,J")))]
  "TARGET_CAPABILITY_ANY"
  "@
  add\\t%0, %1, %2
  sub\\t%0, %1, #%n2"
)

; TODO: many more alternatives.
(define_insn "*movcadi_aarch64"
  [(set (match_operand:CADI 0 "nonimmediate_operand" "=rk,r,r,m,r,r")
	(match_operand:CADI 1 "aarch64_mov_operand" "rk,Z,m,r,Usa,Ush"))]
  "TARGET_CAPABILITY_FAKE"
  "@
   mov\\t%0, %1
   mov\\t%0, xzr
   ldr\\t%0, %1
   str\\t%1, %0
   adr\\t%0, %c1
   adrp\\t%0, %A1"
)

(define_insn "replace_address_value_cadi"
  [(set (match_operand:CADI 0 "register_operand" "=rk")
        (replace_address_value:CADI
	      (match_operand:CADI 1 "register_operand" "rk")
	      (match_operand:DI 2 "register_operand" "r")))]
  "TARGET_CAPABILITY_ANY"
{
  return TARGET_MORELLO ? "scvalue\t%0, %1, %2" : "mov\t%0, %2";
})

(define_insn "*movcadi_aarch64"
  [(set (match_operand:CADI 0 "nonimmediate_operand" "=rk,r,r,m,r  ,r  ")
	(match_operand:CADI 1 "aarch64_mov_operand"  "rk ,Z,m,r,Usa,Ush"))]
  "TARGET_CAPABILITY_PURE"
  "@
  mov\\t%0, %1
  mov\\t%x0, xzr
  ldr\\t%0, %1
  str\\t%1, %0
  adr\\t%0, %c1
  adrp\\t%0, %A1"
)

;; CHERI builtins helpers.

(define_insn "aarch64_cap_base_get"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:CADI 1 "register_operand" "rk")]
            UNSPEC_CHERI_BASE_GET))
  ]
  "TARGET_MORELLO"
  "gcbase\\t%0, %1"
)

(define_insn "aarch64_cap_length_get"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:CADI 1 "register_operand" "rk")]
            UNSPEC_CHERI_LEN_GET))
  ]
  "TARGET_MORELLO"
  "gclen\\t%0, %1"
)

(define_insn "aarch64_cap_repr_align_mask"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "register_operand" "r")]
            UNSPEC_CHERI_REPR_ALIGN_MASK)
    )]
  "TARGET_MORELLO"
  "rrmask\\t%0, %1"
)

(define_insn "aarch64_cap_round_repr_len"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "register_operand" "r")]
           UNSPEC_CHERI_ROUND_REPR_LEN)
    )]
  "TARGET_MORELLO"
  "rrlen\\t%0, %1"
)

(define_insn "aarch64_cap_bounds_set"
  [(set (match_operand:CADI 0 "register_operand" "=rk")
        (unspec:CADI [(match_operand:CADI 1 "register_operand" "rk")
          (match_operand:DI 2 "register_operand" "r")]
            UNSPEC_CHERI_BOUNDS_SET))
  ]
  "TARGET_MORELLO"
  "scbnds\\t%0, %1, %2"
)

(define_insn "aarch64_cap_bounds_set_exact"
  [(set (match_operand:CADI 0 "register_operand" "=rk")
        (unspec:CADI [(match_operand:CADI 1 "register_operand" "rk")
          (match_operand:DI 2 "register_operand" "r")]
            UNSPEC_CHERI_BOUNDS_SET_EXACT))
  ]
  "TARGET_MORELLO"
  "scbndse\\t%0, %1, %2"
)

(define_insn "aarch64_cap_seal"
  [(set (match_operand:CADI 0 "register_operand" "=rk")
        (unspec:CADI [(match_operand:CADI 1 "register_operand" "rk")
          (match_operand:CADI 2 "register_operand" "rk")]
            UNSPEC_CHERI_SEAL))
  ]
  "TARGET_MORELLO"
  "seal\\t%0, %1, %2"
)

(define_insn "aarch64_cap_clear_perm"
  [(set (match_operand:CADI 0 "register_operand" "=rk")
        (unspec:CADI [(match_operand:CADI 1 "register_operand" "rk")
          (match_operand:DI 2 "register_operand" "r")]
            UNSPEC_CHERI_CLEAR_PERM))
  ]
  "TARGET_MORELLO"
  "clrperm\\t%0, %1, %2"
)

(define_insn "aarch64_cap_global_data_get"
  [(set (match_operand:CADI 0 "register_operand" "=rk")
        (unspec:CADI [(const_int 0)]
            UNSPEC_CHERI_GLOBAL_DATA_GET))]
  "TARGET_MORELLO"
  "mrs\\t%0, DDC"
)

(define_expand "aarch64_cap_perms_and"
  [(match_operand:CADI 0 "register_operand")
   (match_operand:CADI 1 "register_operand")
   (match_operand:DI 2 "register_operand")]
  "TARGET_MORELLO"
  {
    emit_insn (gen_one_cmpldi2 (operands[2], operands[2]));
    emit_insn (gen_aarch64_cap_clear_perm (operands[0], operands[1], operands[2]));
    DONE;
  }
)

(define_insn "aarch64_cap_offset_get"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:CADI 1 "register_operand" "rk")]
            UNSPEC_CHERI_OFFSET_GET))
  ]
  "TARGET_MORELLO"
  "gcoff\\t%0, %1"
)

(define_insn "aarch64_cap_offset_set"
  [(set (match_operand:CADI 0 "register_operand" "=rk")
        (unspec:CADI [(match_operand:CADI 1 "register_operand" "rk")
          (match_operand:DI 2 "register_operand" "r")]
            UNSPEC_CHERI_OFFSET_SET))
     ]
  "TARGET_MORELLO"
  "scoff\\t%0, %1, %2"
)
