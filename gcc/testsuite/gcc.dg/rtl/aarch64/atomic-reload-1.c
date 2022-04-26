/* { dg-do compile { target aarch64-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fdisable-rtl-dse1 -fdisable-rtl-dse2 -mno-fake-capability" } */

int __RTL (startwith ("vregs")) f1 (int *ptr, int val)
{
(function "f1"
  (param "ptr"
    (DECL_RTL (reg/v:DI <1> [ ptr ]))
    (DECL_RTL_INCOMING (reg:DI x0 [ ptr ]))
  )
  (param "val"
    (DECL_RTL (reg/v:SI <2> [ val ]))
    (DECL_RTL_INCOMING (reg:SI x1 [ val ]))
  )
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3
	(parallel
	  [(set (reg:SI x0) (mem/v:SI (reg:DI x0) [-1 S4 A32]))
	   (set (mem/v:SI (reg:DI x0) [-1 S4 A32])
		(unspec_volatile:SI
		  [(plus:SI (mem/v:SI (reg:DI x0) [-1 S4 A32])
			    (reg:SI <2>))
		   (const_int 32773)]
		  UNSPECV_ATOMIC_OP))
	   (clobber (reg:CC cc))
	   (clobber (scratch:SI))
	   (clobber (scratch:SI))]
	)
      )
      (cinsn 4 (use (reg:SI x0)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
  (crtl (return_rtx (reg:SI x0)))
)
}

/* { dg-final { scan-assembler-not {\tldr\t} } } */
/* { dg-final { scan-assembler-not {\tstr\t} } } */
/* { dg-final { scan-assembler-not {sp, } } } */
