..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _sh-function-attributes:

SH Function Attributes
^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported on the SH family of processors:

.. option:: function_vector

  .. index:: function_vector function attribute, SH

  .. index:: calling functions through the function vector on SH2A

  On SH2A targets, this attribute declares a function to be called using the
  TBR relative addressing mode.  The argument to this attribute is the entry
  number of the same function in a vector table containing all the TBR
  relative addressable functions.  For correct operation the TBR must be setup
  accordingly to point to the start of the vector table before any functions with
  this attribute are invoked.  Usually a good place to do the initialization is
  the startup routine.  The TBR relative vector table can have at max 256 function
  entries.  The jumps to these functions are generated using a SH2A specific,
  non delayed branch instruction JSR/N @(disp8,TBR).  You must use GAS and GLD
  from GNU binutils version 2.7 or later for this attribute to work correctly.

  In an application, for a function being called once, this attribute
  saves at least 8 bytes of code; and if other successive calls are being
  made to the same function, it saves 2 bytes of code per each of these
  calls.

.. option:: interrupt_handler

  .. index:: interrupt_handler function attribute, SH

  Use this attribute to
  indicate that the specified function is an interrupt handler.  The compiler
  generates function entry and exit sequences suitable for use in an
  interrupt handler when this attribute is present.

.. option:: nosave_low_regs

  .. index:: nosave_low_regs function attribute, SH

  Use this attribute on SH targets to indicate that an ``interrupt_handler``
  function should not save and restore registers R0..R7.  This can be used on SH3\*
  and SH4\* targets that have a second R0..R7 register bank for non-reentrant
  interrupt handlers.

.. option:: renesas

  .. index:: renesas function attribute, SH

  On SH targets this attribute specifies that the function or struct follows the
  Renesas ABI.

.. option:: resbank

  .. index:: resbank function attribute, SH

  On the SH2A target, this attribute enables the high-speed register
  saving and restoration using a register bank for ``interrupt_handler``
  routines.  Saving to the bank is performed automatically after the CPU
  accepts an interrupt that uses a register bank.

  The nineteen 32-bit registers comprising general register R0 to R14,
  control register GBR, and system registers MACH, MACL, and PR and the
  vector table address offset are saved into a register bank.  Register
  banks are stacked in first-in last-out (FILO) sequence.  Restoration
  from the bank is executed by issuing a RESBANK instruction.

.. option:: sp_switch

  .. index:: sp_switch function attribute, SH

  Use this attribute on the SH to indicate an ``interrupt_handler``
  function should switch to an alternate stack.  It expects a string
  argument that names a global variable holding the address of the
  alternate stack.

  .. code-block:: c++

    void *alt_stack;
    void f () __attribute__ ((interrupt_handler,
                              sp_switch ("alt_stack")));

.. option:: trap_exit

  .. index:: trap_exit function attribute, SH

  Use this attribute on the SH for an ``interrupt_handler`` to return using
  ``trapa`` instead of ``rte``.  This attribute expects an integer
  argument specifying the trap number to be used.

.. option:: trapa_handler

  .. index:: trapa_handler function attribute, SH

  On SH targets this function attribute is similar to ``interrupt_handler``
  but it does not save and restore all registers.