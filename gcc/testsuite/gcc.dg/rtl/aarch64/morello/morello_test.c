/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-mfake-capability" { target { ! aarch64_capability_any } } } */

int __RTL (startwith ("final")) f1 ()
{
(function "f1"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 6 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 100 (set (reg:CADI x0) (reg:CADI x1))
       "/home/user/somefile.c":10:3)

      ;; Extra insn, to avoid all of the above from being deleted by DCE
      (insn 10 (use (reg/i:CADI x0)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function
}


int __RTL (startwith ("peephole2")) f2 ()
{
(function "f2"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 6 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 100 (set (subreg:DI (reg:CADI x0) 0) (subreg:DI (reg:CADI x1) 0))
       "/home/user/somefile.c":10:3)

      ;; Extra insn, to avoid all of the above from being deleted by DCE
      (insn 10 (use (reg/i:CADI x0)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function
}

