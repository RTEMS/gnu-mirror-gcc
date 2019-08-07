;; PC relative support.
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

;;
;; UNSPEC usage
;;

(define_c_enum "unspec"
  [UNSPEC_PCREL_LD
   UNSPEC_PCREL_ST
  ])


;; Optimize references to external variables to combine loading up the external
;; address from the GOT and doing the load or store operation.
;;
;; A typical optimization looks like:
;;
;;		pld b,var@pcrel@got(0),1
;;	100:
;;		...
;;		.reloc 100b-8,R_PPC64_PCREL_OPT,0
;;		lwz r,0(b)
;;
;; If 'var' is an external variable defined in another module in the main
;; program, and the code is being linked for the main program, then the
;; linker can optimize this to:
;;
;;		plwz r,var(0),1
;;	100:
;;		...
;;		nop
;;
;; If either the variable or the code being linked is defined in a shared
;; library, then the linker puts the address in the GOT area, and the pld will
;; load up the pointer, and then that pointer is used for the load or store.
;; If there is more than one reference to the GOT pointer, the compiler will
;; not do this optimization, and use the GOT pointer normally.
;;
;; Having the label after the pld instruction and using label-8 in the .reloc
;; addresses the prefixed instruction properly.  If we put the label before the
;; pld instruction, then the relocation might point to the NOP that is
;; generated if the prefixed instruction is not aligned.
;;
;; We need to rewrite the normal GOT load operation before register allocation
;; to include setting the eventual destination register for loads, or referring
;; to the value being stored for store operations so that the proper register
;; lifetime is set in case the optimization is done and the pld/lwz is
;; converted to plwz/nop.

(define_mode_iterator PO [QI HI SI DI SF DF
			  V16QI V8HI V4SI V4SF V2DI V2DF V1TI KF
			  (TF "FLOAT128_IEEE_P (TFmode)")])

;; Vector types for pcrel optimization
(define_mode_iterator POV [V16QI V8HI V4SI V4SF V2DI V2DF V1TI KF
			   (TF "FLOAT128_IEEE_P (TFmode)")])

;; Define the constraints for each mode for pcrel_opt.  The order of the
;; constraints should have the most natural register class first.
(define_mode_attr PO_constraint [(QI    "r,d,v")
				 (HI    "r,d,v")
				 (SI    "r,d,v")
				 (DI    "r,d,v")
				 (SF    "d,v,r")
				 (DF    "d,v,r")
				 (V16QI "wa,wn,wn")
				 (V8HI  "wa,wn,wn")
				 (V4SI  "wa,wn,wn")
				 (V4SF  "wa,wn,wn")
				 (V2DI  "wa,wn,wn")
				 (V2DF  "wa,wn,wn")
				 (V1TI  "wa,wn,wn")
				 (KF    "wa,wn,wn")
				 (TF    "wa,wn,wn")])

;; Combiner pattern that combines the load of the GOT along with the load.  The
;; first split pass before register allocation will split this into the load of
;; the GOT that indicates the resultant value may be created if the PCREL_OPT
;; relocation is done.
;;
;; The (set (match_dup 0)
;;	    (unspec:<MODE> [(const_int 0)] UNSPEC_PCREL_LD))
;;
;; Is to signal to the register allocator that the destination register may be
;; set by the GOT operation (if the linker does the optimization).
;;
;; We need to set the "cost" explicitly so that the instruction length is not
;; used.  We return the same cost as a normal load (4 if we are not optimizing
;; for speed, 8 if we are optimizing for speed)

(define_insn_and_split "*mov<mode>_pcrel_opt_load"
  [(set (match_operand:PO 0 "gpc_reg_operand")
	(match_operand:PO 1 "pcrel_external_mem_operand"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(parallel [(set (match_dup 2)
		   (match_dup 3))
	      (set (match_dup 0)
		   (unspec:<MODE> [(const_int 0)] UNSPEC_PCREL_LD))
	      (use (const_int 0))])
   (parallel [(set (match_dup 0)
		   (match_dup 4))
	      (use (match_dup 0))
	      (use (const_int 0))])]
{
  rtx mem = operands[1];
  rtx got = gen_reg_rtx (DImode);

  operands[2] = got;
  operands[3] = XEXP (mem, 0);
  operands[4] = change_address (mem, <MODE>mode, got);
}
  [(set_attr "type" "load")
   (set_attr "length" "16")
   (set (attr "cost")
	(if_then_else (match_test "optimize_function_for_speed_p (cfun)")
		      (const_string "8")
		      (const_string "4")))
   (set_attr "prefixed" "yes")])

;; Zero extend combiner patterns
(define_insn_and_split "*mov<mode>_pcrel_opt_zero_extend"
  [(set (match_operand:DI 0 "gpc_reg_operand")
	(zero_extend:DI
	 (match_operand:QHSI 1 "pcrel_external_mem_operand")))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(parallel [(set (match_dup 2)
		   (match_dup 3))
	      (set (match_dup 0)
		   (unspec:DI [(const_int 0)] UNSPEC_PCREL_LD))
	      (use (const_int 0))])
   (parallel [(set (match_dup 0)
		   (zero_extend:DI
		    (match_dup 4)))
	      (use (match_dup 0))
	      (use (const_int 0))])]
{
  rtx mem = operands[1];
  rtx got = gen_reg_rtx (DImode);

  operands[2] = got;
  operands[3] = XEXP (mem, 0);
  operands[4] = change_address (mem, <MODE>mode, got);
}
  [(set_attr "type" "load")
   (set_attr "length" "16")
   (set (attr "cost")
	(if_then_else (match_test "optimize_function_for_speed_p (cfun)")
		      (const_string "8")
		      (const_string "4")))
   (set_attr "prefixed" "yes")])

;; Sign extend combiner patterns
(define_insn_and_split "*mov<mode>_pcrel_opt_sign_extend"
  [(set (match_operand:DI 0 "gpc_reg_operand")
	(sign_extend:DI
	 (match_operand:HSI 1 "pcrel_external_mem_operand")))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(parallel [(set (match_dup 2)
		   (match_dup 3))
	      (set (match_dup 0)
		   (unspec:DI [(const_int 0)] UNSPEC_PCREL_LD))
	      (use (const_int 0))])
   (parallel [(set (match_dup 0)
		   (sign_extend:DI
		    (match_dup 4)))
	      (use (match_dup 0))
	      (use (const_int 0))])]
{
  rtx mem = operands[1];
  rtx got = gen_reg_rtx (DImode);

  operands[2] = got;
  operands[3] = XEXP (mem, 0);
  operands[4] = change_address (mem, <MODE>mode, got);
}
  [(set_attr "type" "load")
   (set_attr "length" "16")
   (set (attr "cost")
	(if_then_else (match_test "optimize_function_for_speed_p (cfun)")
		      (const_string "8")
		      (const_string "4")))
   (set_attr "prefixed" "yes")])

;; Float extend combiner pattern
(define_insn_and_split "*movdf_pcrel_opt_float_extend"
  [(set (match_operand:DF 0 "gpc_reg_operand")
	(float_extend:DF
	 (match_operand:SF 1 "pcrel_external_mem_operand")))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64
   && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(parallel [(set (match_dup 2)
		   (match_dup 3))
	      (set (match_dup 0)
		   (unspec:DF [(const_int 0)] UNSPEC_PCREL_LD))
	      (use (const_int 0))])
   (parallel [(set (match_dup 0)
		   (float_extend:DF
		    (match_dup 4)))
	      (use (match_dup 0))
	      (use (const_int 0))])]
{
  rtx mem = operands[1];
  rtx got = gen_reg_rtx (DImode);

  operands[2] = got;
  operands[3] = XEXP (mem, 0);
  operands[4] = change_address (mem, SFmode, got);
}
  [(set_attr "type" "load")
   (set_attr "length" "16")
   (set (attr "cost")
	(if_then_else (match_test "optimize_function_for_speed_p (cfun)")
		      (const_string "8")
		      (const_string "4")))
   (set_attr "prefixed" "yes")])

;; Patterns to load up the GOT address that may be changed into the load of the
;; actual variable.
(define_insn "*mov<mode>_pcrel_opt_load_got"
  [(set (match_operand:DI 0 "base_reg_operand" "=b,b,b")
	(match_operand:DI 1 "pcrel_external_address"))
   (set (match_operand:PO 2 "gpc_reg_operand" "=<PO_constraint>")
	(unspec:PO [(const_int 0)] UNSPEC_PCREL_LD))
   (use (match_operand:DI 3 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
{
  return (INTVAL (operands[3])) ? "ld %0,%a1\n.Lpcrel%3:" : "ld %0,%a1";
}
  [(set_attr "type" "load")
   (set_attr "length" "12")
   (set_attr "pcrel_opt" "load_got")
   (set (attr "cost")
	(if_then_else (match_test "optimize_function_for_speed_p (cfun)")
		      (const_string "8")
		      (const_string "4")))
   (set_attr "prefixed" "yes")])

;; The secondary load insns that uses the GOT pointer that may become a NOP.
(define_insn "*mov<mode>_pcrel_opt_load_mem"
  [(set (match_operand:QHI 0 "gpc_reg_operand" "+r,wa")
	(match_operand:QHI 1 "one_reg_memory_operand" "Q,Q"))
   (use (match_operand:QHI 2 "gpc_reg_operand" "0,0"))
   (use (match_operand:DI 3 "const_int_operand" "n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
   l<wd>z %0,%1
   lxsi<wd>zx %x0,%y1"
  [(set_attr "type" "load,fpload")
   (set_attr "pcrel_opt" "load,no")
   (set_attr "prefixed" "no")])

(define_insn "*movsi_pcrel_opt_load_mem"
  [(set (match_operand:SI 0 "gpc_reg_operand" "+r,d,v")
	(match_operand:SI 1 "one_reg_memory_operand" "Q,Q,Q"))
   (use (match_operand:SI 2 "gpc_reg_operand" "0,0,0"))
   (use (match_operand:DI 3 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
   lwz %0,%1
   lfiwzx %0,%y1
   lxsiwzx %x0,%y1"
  [(set_attr "type" "load,fpload,fpload")
   (set_attr "pcrel_opt" "load,no,no")
   (set_attr "prefixed" "no")])

(define_insn "*movdi_pcrel_opt_load_mem"
  [(set (match_operand:DI 0 "gpc_reg_operand" "+r,d,v")
	(match_operand:DI 1 "one_reg_memory_operand" "Q,Q,Q"))
   (use (match_operand:DI 2 "gpc_reg_operand" "0,0,0"))
   (use (match_operand:DI 3 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
   ld %0,%1
   lfd %0,%1
   lxsd %0,%1"
  [(set_attr "type" "load,fpload,fpload")
   (set_attr "pcrel_opt" "load")
   (set_attr "prefixed" "no")])

(define_insn "*movsf_pcrel_opt_load_mem"
  [(set (match_operand:SF 0 "gpc_reg_operand" "+d,v,r")
	(match_operand:SF 1 "one_reg_memory_operand" "Q,Q,Q"))
   (use (match_operand:SF 2 "gpc_reg_operand" "0,0,0"))
   (use (match_operand:DI 3 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
   lfs %0,%1
   lxssp %0,%1
   lwz %0,%1"
  [(set_attr "type" "fpload,fpload,load")
   (set_attr "pcrel_opt" "load")
   (set_attr "prefixed" "no")])

(define_insn "*movdf_pcrel_opt_load_mem"
  [(set (match_operand:DF 0 "gpc_reg_operand" "+d,v,r")
	(match_operand:DF 1 "one_reg_memory_operand" "Q,Q,Q"))
   (use (match_operand:DF 2 "gpc_reg_operand" "0,0,0"))
   (use (match_operand:DI 3 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
   lfd %0,%1
   lxsd %0,%1
   ld %0,%1"
  [(set_attr "type" "fpload,fpload,load")
   (set_attr "pcrel_opt" "load")
   (set_attr "prefixed" "no")])

(define_insn "*mov<mode>_pcrel_opt_load_mem"
  [(set (match_operand:POV 0 "gpc_reg_operand" "+wa")
	(match_operand:POV 1 "one_reg_memory_operand" "Q"))
   (use (match_operand:POV 2 "gpc_reg_operand" "0"))
   (use (match_operand:DI 3 "const_int_operand" "n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "lxv %x0,%1"
  [(set_attr "type" "vecload")
   (set_attr "pcrel_opt" "load")
   (set_attr "prefixed" "no")])

;; Zero extend insns
(define_insn "*mov<mode>_pcrel_opt_load_zero_extend2"
  [(set (match_operand:DI 0 "gpc_reg_operand" "+r,wa")
	(zero_extend:DI
	 (match_operand:QHI 1 "one_reg_memory_operand" "Q,Q")))
   (use (match_operand:DI 2 "gpc_reg_operand" "0,0"))
   (use (match_operand:DI 3 "const_int_operand" "n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
   l<wd>z %0,%1
   lxsi<wd>zx %x0,%y1"
  [(set_attr "type" "load,fpload")
   (set_attr "pcrel_opt" "load,no")
   (set_attr "prefixed" "no")])

(define_insn "*movsi_pcrel_opt_load_zero_extend2"
  [(set (match_operand:DI 0 "gpc_reg_operand" "+r,d,v")
	(zero_extend:DI
	 (match_operand:SI 1 "one_reg_memory_operand" "Q,Q,Q")))
   (use (match_operand:DI 2 "gpc_reg_operand" "0,0,0"))
   (use (match_operand:DI 3 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
   lwz %0,%1
   lfiwzx %0,%y1
   lxsiwzx %x0,%y1"
  [(set_attr "type" "load,fpload,fpload")
   (set_attr "pcrel_opt" "load,no,no")
   (set_attr "prefixed" "no")])

;; Sign extend insns
(define_insn "*movsi_pcrel_opt_load_sign_extend2"
  [(set (match_operand:DI 0 "gpc_reg_operand" "+r,d,v")
	(sign_extend:DI
	 (match_operand:SI 1 "one_reg_memory_operand" "Q,Q,Q")))
   (use (match_operand:DI 2 "gpc_reg_operand" "0,0,0"))
   (use (match_operand:DI 3 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
   lwa %0,%1
   lfiwax %0,%y1
   lxsiwax %x0,%y1"
  [(set_attr "type" "load,fpload,fpload")
   (set_attr "pcrel_opt" "load,no,no")
   (set_attr "prefixed" "no")])

(define_insn_and_split "*movhi_pcrel_opt_load_sign_extend2"
  [(set (match_operand:DI 0 "gpc_reg_operand" "+r,v")
	(sign_extend:DI
	 (match_operand:HI 1 "one_reg_memory_operand" "Q,Q")))
   (use (match_operand:DI 2 "gpc_reg_operand" "0,0"))
   (use (match_operand:DI 3 "const_int_operand" "n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
   lha %0,%1
   #"
  "&& reload_completed && altivec_register_operand (operands[0], HImode)"
  [(parallel [(set (match_dup 4)
		   (match_dup 1))
	      (use (match_dup 4))
	      (use (const_int 0))])
   (set (match_dup 0)
	(sign_extend:DI
	 (match_dup 4)))]
{
  operands[4] = gen_rtx_REG (HImode, REGNO (operands[0]));
}
  [(set_attr "type" "load,fpload")
   (set_attr "pcrel_opt" "load,no")
   (set_attr "length" "4,8")
   (set_attr "prefixed" "no")])

;; Floating point extend insn
(define_insn "*movsf_pcrel_opt_load_float_extend2"
  [(set (match_operand:DF 0 "gpc_reg_operand" "+d,v")
	(float_extend:DF
	 (match_operand:SF 1 "one_reg_memory_operand" "Q,Q")))
   (use (match_operand:DF 2 "gpc_reg_operand" "0,0"))
   (use (match_operand:DI 3 "const_int_operand" "n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
   lfs %0,%1
   lxssp %0,%1"
  [(set_attr "type" "fpload")
   (set_attr "pcrel_opt" "load")
   (set_attr "prefixed" "no")])

; ;; Store combiner insns that merge together loading up the address of the
; ;; external variable and doing the store.  This is split in the first split
; ;; pass before register allocation.
;;
;; We need to set the "cost" explicitly so that the instruction length is not
;; used.  We return the same cost as a normal store (4).
(define_insn_and_split "*mov<mode>_pcrel_opt_store"
  [(set (match_operand:PO 0 "pcrel_external_mem_operand")
 	(match_operand:PO 1 "gpc_reg_operand"))]
   "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64
    && can_create_pseudo_p ()"
   "#"
   "&& 1"
   [(set (match_dup 2)
	 (unspec:DI [(match_dup 1)
		     (match_dup 3)
		     (const_int 0)] UNSPEC_PCREL_ST))
    (parallel [(set (match_dup 4)
		    (match_dup 1))
	       (use (const_int 0))])]
{
  rtx mem = operands[0];
  rtx addr = XEXP (mem, 0);
  rtx got = gen_reg_rtx (DImode);

  operands[2] = got;
  operands[3] = addr;
  operands[4] = change_address (mem, <MODE>mode, got);
}
  [(set_attr "type" "load")
   (set_attr "length" "20")
   (set_attr "pcrel_opt" "store_got")
   (set_attr "cost" "4")
   (set_attr "prefixed" "yes")])

;; Load of the GOT address for a store operation that may be converted into a
;; direct store.
(define_insn "*mov<mode>_pcrel_opt_store_got"
  [(set (match_operand:DI 0 "base_reg_operand" "=&b,&b,&b")
	(unspec:DI [(match_operand:PO 1 "gpc_reg_operand" "<PO_constraint>")
		    (match_operand:DI 2 "pcrel_external_address")
		    (match_operand:DI 3 "const_int_operand" "n,n,n")]
		   UNSPEC_PCREL_ST))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
{
  return (INTVAL (operands[3])) ? "ld %0,%a2\n.Lpcrel%3:" : "ld %0,%a2";
}
  [(set_attr "type" "load")
   (set_attr "length" "12")
   (set_attr "pcrel_opt" "store_got")
   (set_attr "cost" "4")
   (set_attr "prefixed" "yes")])

;; Secondary store instruction that uses the GOT pointer, and may be optimized
;; into a NOP instruction.
(define_insn "*mov<mode>_pcrel_opt_store_mem"
  [(set (match_operand:QHI 0 "one_reg_memory_operand" "=Q,Q")
	(match_operand:QHI 1 "gpc_reg_operand" "r,wa"))
   (use (match_operand:DI 2 "const_int_operand" "n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
  st<wd> %1,%0
  stxsi<wd>x %x1,%y0"
  [(set_attr "type" "store,fpstore")
   (set_attr "pcrel_opt" "store,no")
   (set_attr "prefixed" "no")])

(define_insn "*movsi_pcrel_opt_store_mem"
  [(set (match_operand:SI 0 "one_reg_memory_operand" "=Q,Q,Q")
	(match_operand:SI 1 "gpc_reg_operand" "r,d,v"))
   (use (match_operand:DI 2 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
  stw %1,%0
  stfiwx %1,%y0
  stxsiwx %1,%y0"
  [(set_attr "type" "store,fpstore,fpstore")
   (set_attr "pcrel_opt" "store,no,no")
   (set_attr "prefixed" "no")])

(define_insn "*movdi_pcrel_opt_store_mem"
  [(set (match_operand:DI 0 "one_reg_memory_operand" "=Q,Q,Q")
	(match_operand:DI 1 "gpc_reg_operand" "r,d,v"))
   (use (match_operand:DI 2 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
  std %1,%0
  stfd %1,%0
  stxsd %1,%0"
  [(set_attr "type" "store,fpstore,fpstore")
   (set_attr "pcrel_opt" "store")
   (set_attr "prefixed" "no")])

(define_insn "*movsf_pcrel_opt_store_mem"
  [(set (match_operand:SF 0 "one_reg_memory_operand" "=Q,Q,Q")
	(match_operand:SF 1 "gpc_reg_operand" "d,v,r"))
   (use (match_operand:DI 2 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
  stfs %1,%0
  stxssp %1,%0
  stw %1,%0"
  [(set_attr "type" "fpstore,fpstore,store")
   (set_attr "pcrel_opt" "store")
   (set_attr "prefixed" "no")])

(define_insn "*movdf_pcrel_opt_store_mem"
  [(set (match_operand:DF 0 "one_reg_memory_operand" "=Q,Q,Q")
	(match_operand:DF 1 "gpc_reg_operand" "d,v,r"))
   (use (match_operand:DI 2 "const_int_operand" "n,n,n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "@
  stfd %1,%0
  stxsd %1,%0
  std %1,%0"
  [(set_attr "type" "fpstore,fpstore,store")
   (set_attr "pcrel_opt" "store")
   (set_attr "prefixed" "no")])

(define_insn "*mov<mode>_pcrel_opt_store_mem"
  [(set (match_operand:POV 0 "one_reg_memory_operand" "=Q")
	(match_operand:POV 1 "gpc_reg_operand" "wa"))
   (use (match_operand:DI 2 "const_int_operand" "n"))]
  "TARGET_PCREL && TARGET_PCREL_OPT && TARGET_POWERPC64"
  "stxv %x1,%0"
  [(set_attr "type" "vecstore")
   (set_attr "pcrel_opt" "store")
   (set_attr "prefixed" "no")])
