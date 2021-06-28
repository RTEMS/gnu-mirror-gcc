..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _rtl-passes:

RTL passes
**********

The following briefly describes the RTL generation and optimization
passes that are run after the Tree optimization passes.

* RTL generation

  .. Avoiding overfull is tricky here.

  The source files for RTL generation include
  :samp:`stmt.c`,
  :samp:`calls.c`,
  :samp:`expr.c`,
  :samp:`explow.c`,
  :samp:`expmed.c`,
  :samp:`function.c`,
  :samp:`optabs.c`
  and :samp:`emit-rtl.c`.
  Also, the file
  :samp:`insn-emit.c`, generated from the machine description by the
  program ``genemit``, is used in this pass.  The header file
  :samp:`expr.h` is used for communication within this pass.

  .. index:: genflags

  .. index:: gencodes

  The header files :samp:`insn-flags.h` and :samp:`insn-codes.h`,
  generated from the machine description by the programs ``genflags``
  and ``gencodes``, tell this pass which standard names are available
  for use and which patterns correspond to them.

* Generation of exception landing pads

  This pass generates the glue that handles communication between the
  exception handling library routines and the exception handlers within
  the function.  Entry points in the function that are invoked by the
  exception handling library are called :dfn:`landing pads`.  The code
  for this pass is located in :samp:`except.c`.

* Control flow graph cleanup

  This pass removes unreachable code, simplifies jumps to next, jumps to
  jump, jumps across jumps, etc.  The pass is run multiple times.
  For historical reasons, it is occasionally referred to as the 'jump
  optimization pass'.  The bulk of the code for this pass is in
  :samp:`cfgcleanup.c`, and there are support routines in :samp:`cfgrtl.c`
  and :samp:`jump.c`.

* Forward propagation of single-def values

  This pass attempts to remove redundant computation by substituting
  variables that come from a single definition, and
  seeing if the result can be simplified.  It performs copy propagation
  and addressing mode selection.  The pass is run twice, with values
  being propagated into loops only on the second run.  The code is
  located in :samp:`fwprop.c`.

* Common subexpression elimination

  This pass removes redundant computation within basic blocks, and
  optimizes addressing modes based on cost.  The pass is run twice.
  The code for this pass is located in :samp:`cse.c`.

* Global common subexpression elimination

  This pass performs two
  different types of GCSE  depending on whether you are optimizing for
  size or not (LCM based GCSE tends to increase code size for a gain in
  speed, while Morel-Renvoise based GCSE does not).
  When optimizing for size, GCSE is done using Morel-Renvoise Partial
  Redundancy Elimination, with the exception that it does not try to move
  invariants out of loops---that is left to  the loop optimization pass.
  If MR PRE GCSE is done, code hoisting (aka unification) is also done, as
  well as load motion.
  If you are optimizing for speed, LCM (lazy code motion) based GCSE is
  done.  LCM is based on the work of Knoop, Ruthing, and Steffen.  LCM
  based GCSE also does loop invariant code motion.  We also perform load
  and store motion when optimizing for speed.
  Regardless of which type of GCSE is used, the GCSE pass also performs
  global constant and  copy propagation.
  The source file for this pass is :samp:`gcse.c`, and the LCM routines
  are in :samp:`lcm.c`.

* Loop optimization

  This pass performs several loop related optimizations.
  The source files :samp:`cfgloopanal.c` and :samp:`cfgloopmanip.c` contain
  generic loop analysis and manipulation code.  Initialization and finalization
  of loop structures is handled by :samp:`loop-init.c`.
  A loop invariant motion pass is implemented in :samp:`loop-invariant.c`.
  Basic block level optimizations---unrolling, and peeling loops---
  are implemented in :samp:`loop-unroll.c`.
  Replacing of the exit condition of loops by special machine-dependent
  instructions is handled by :samp:`loop-doloop.c`.

* Jump bypassing

  This pass is an aggressive form of GCSE that transforms the control
  flow graph of a function by propagating constants into conditional
  branch instructions.  The source file for this pass is :samp:`gcse.c`.

* If conversion

  This pass attempts to replace conditional branches and surrounding
  assignments with arithmetic, boolean value producing comparison
  instructions, and conditional move instructions.  In the very last
  invocation after reload/LRA, it will generate predicated instructions
  when supported by the target.  The code is located in :samp:`ifcvt.c`.

* Web construction

  This pass splits independent uses of each pseudo-register.  This can
  improve effect of the other transformation, such as CSE or register
  allocation.  The code for this pass is located in :samp:`web.c`.

* Instruction combination

  This pass attempts to combine groups of two or three instructions that
  are related by data flow into single instructions.  It combines the
  RTL expressions for the instructions by substitution, simplifies the
  result using algebra, and then attempts to match the result against
  the machine description.  The code is located in :samp:`combine.c`.

* Mode switching optimization

  This pass looks for instructions that require the processor to be in a
  specific 'mode' and minimizes the number of mode changes required to
  satisfy all users.  What these modes are, and what they apply to are
  completely target-specific.  The code for this pass is located in
  :samp:`mode-switching.c`.

  .. index:: modulo scheduling

  .. index:: sms, swing, software pipelining

* Modulo scheduling

  This pass looks at innermost loops and reorders their instructions
  by overlapping different iterations.  Modulo scheduling is performed
  immediately before instruction scheduling.  The code for this pass is
  located in :samp:`modulo-sched.c`.

* Instruction scheduling

  This pass looks for instructions whose output will not be available by
  the time that it is used in subsequent instructions.  Memory loads and
  floating point instructions often have this behavior on RISC machines.
  It re-orders instructions within a basic block to try to separate the
  definition and use of items that otherwise would cause pipeline
  stalls.  This pass is performed twice, before and after register
  allocation.  The code for this pass is located in :samp:`haifa-sched.c`,
  :samp:`sched-deps.c`, :samp:`sched-ebb.c`, :samp:`sched-rgn.c` and
  :samp:`sched-vis.c`.

* Register allocation

  These passes make sure that all occurrences of pseudo registers are
  eliminated, either by allocating them to a hard register, replacing
  them by an equivalent expression (e.g. a constant) or by placing
  them on the stack.  This is done in several subpasses:

  * The integrated register allocator (IRA).  It is called
    integrated because coalescing, register live range splitting, and hard
    register preferencing are done on-the-fly during coloring.  It also
    has better integration with the reload/LRA pass.  Pseudo-registers spilled
    by the allocator or the reload/LRA have still a chance to get
    hard-registers if the reload/LRA evicts some pseudo-registers from
    hard-registers.  The allocator helps to choose better pseudos for
    spilling based on their live ranges and to coalesce stack slots
    allocated for the spilled pseudo-registers.  IRA is a regional
    register allocator which is transformed into Chaitin-Briggs allocator
    if there is one region.  By default, IRA chooses regions using
    register pressure but the user can force it to use one region or
    regions corresponding to all loops.

    Source files of the allocator are :samp:`ira.c`, :samp:`ira-build.c`,
    :samp:`ira-costs.c`, :samp:`ira-conflicts.c`, :samp:`ira-color.c`,
    :samp:`ira-emit.c`, :samp:`ira-lives`, plus header files :samp:`ira.h`
    and :samp:`ira-int.h` used for the communication between the allocator
    and the rest of the compiler and between the IRA files.

    .. index:: reloading

  * Reloading.  This pass renumbers pseudo registers with the hardware
    registers numbers they were allocated.  Pseudo registers that did not
    get hard registers are replaced with stack slots.  Then it finds
    instructions that are invalid because a value has failed to end up in
    a register, or has ended up in a register of the wrong kind.  It fixes
    up these instructions by reloading the problematical values
    temporarily into registers.  Additional instructions are generated to
    do the copying.

    The reload pass also optionally eliminates the frame pointer and inserts
    instructions to save and restore call-clobbered registers around calls.

    Source files are :samp:`reload.c` and :samp:`reload1.c`, plus the header
    :samp:`reload.h` used for communication between them.

    .. index:: Local Register Allocator (LRA)

  * This pass is a modern replacement of the reload pass.  Source files
    are :samp:`lra.c`, :samp:`lra-assign.c`, :samp:`lra-coalesce.c`,
    :samp:`lra-constraints.c`, :samp:`lra-eliminations.c`,
    :samp:`lra-lives.c`, :samp:`lra-remat.c`, :samp:`lra-spills.c`, the
    header :samp:`lra-int.h` used for communication between them, and the
    header :samp:`lra.h` used for communication between LRA and the rest of
    compiler.

    Unlike the reload pass, intermediate LRA decisions are reflected in
    RTL as much as possible.  This reduces the number of target-dependent
    macros and hooks, leaving instruction constraints as the primary
    source of control.

    LRA is run on targets for which TARGET_LRA_P returns true.

* Basic block reordering

  This pass implements profile guided code positioning.  If profile
  information is not available, various types of static analysis are
  performed to make the predictions normally coming from the profile
  feedback (IE execution frequency, branch probability, etc).  It is
  implemented in the file :samp:`bb-reorder.c`, and the various
  prediction routines are in :samp:`predict.c`.

* Variable tracking

  This pass computes where the variables are stored at each
  position in code and generates notes describing the variable locations
  to RTL code.  The location lists are then generated according to these
  notes to debug information if the debugging information format supports
  location lists.  The code is located in :samp:`var-tracking.c`.

* Delayed branch scheduling

  This optional pass attempts to find instructions that can go into the
  delay slots of other instructions, usually jumps and calls.  The code
  for this pass is located in :samp:`reorg.c`.

* Branch shortening

  On many RISC machines, branch instructions have a limited range.
  Thus, longer sequences of instructions must be used for long branches.
  In this pass, the compiler figures out what how far each instruction
  will be from each other instruction, and therefore whether the usual
  instructions, or the longer sequences, must be used for each branch.
  The code for this pass is located in :samp:`final.c`.

* Register-to-stack conversion

  Conversion from usage of some hard registers to usage of a register
  stack may be done at this point.  Currently, this is supported only
  for the floating-point registers of the Intel 80387 coprocessor.  The
  code for this pass is located in :samp:`reg-stack.c`.

* Final

  This pass outputs the assembler code for the function.  The source files
  are :samp:`final.c` plus :samp:`insn-output.c`; the latter is generated
  automatically from the machine description by the tool :samp:`genoutput`.
  The header file :samp:`conditions.h` is used for communication between
  these files.

* Debugging information output

  This is run after final because it must output the stack slot offsets
  for pseudo registers that did not get hard registers.  Source files
  are :samp:`dbxout.c` for DBX symbol table format, :samp:`dwarfout.c` for
  DWARF symbol table format, files :samp:`dwarf2out.c` and :samp:`dwarf2asm.c`
  for DWARF2 symbol table format, and :samp:`vmsdbgout.c` for VMS debug
  symbol table format.