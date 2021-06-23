..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _blackfin-function-attributes:

Blackfin Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Blackfin back end:

.. option:: exception_handler

  .. index:: exception_handler function attribute

  .. index:: exception handler functions, Blackfin

  Use this attribute on the Blackfin to indicate that the specified function
  is an exception handler.  The compiler generates function entry and
  exit sequences suitable for use in an exception handler when this
  attribute is present.

.. option:: interrupt_handler

  .. index:: interrupt_handler function attribute, Blackfin

  Use this attribute to
  indicate that the specified function is an interrupt handler.  The compiler
  generates function entry and exit sequences suitable for use in an
  interrupt handler when this attribute is present.

.. option:: kspisusp

  .. index:: kspisusp function attribute, Blackfin

  .. index:: User stack pointer in interrupts on the Blackfin

  When used together with ``interrupt_handler``, ``exception_handler``
  or ``nmi_handler``, code is generated to load the stack pointer
  from the USP register in the function prologue.

.. option:: l1_text

  .. index:: l1_text function attribute, Blackfin

  This attribute specifies a function to be placed into L1 Instruction
  SRAM. The function is put into a specific section named ``.l1.text``.
  With :option:`-mfdpic`, function calls with a such function as the callee
  or caller uses inlined PLT.

.. option:: l2

  .. index:: l2 function attribute, Blackfin

  This attribute specifies a function to be placed into L2
  SRAM. The function is put into a specific section named
  ``.l2.text``. With :option:`-mfdpic`, callers of such functions use
  an inlined PLT.

.. option:: longcall

  .. index:: indirect calls, Blackfin

  .. index:: longcall function attribute, Blackfin

  .. index:: shortcall function attribute, Blackfin

  The ``longcall`` attribute
  indicates that the function might be far away from the call site and
  require a different (more expensive) calling sequence.  The
  ``shortcall`` attribute indicates that the function is always close
  enough for the shorter calling sequence to be used.  These attributes
  override the :option:`-mlongcall` switch.

.. option:: nesting

  .. index:: nesting function attribute, Blackfin

  .. index:: Allow nesting in an interrupt handler on the Blackfin processor

  Use this attribute together with ``interrupt_handler``,
  ``exception_handler`` or ``nmi_handler`` to indicate that the function
  entry code should enable nested interrupts or exceptions.

.. option:: nmi_handler

  .. index:: nmi_handler function attribute, Blackfin

  .. index:: NMI handler functions on the Blackfin processor

  Use this attribute on the Blackfin to indicate that the specified function
  is an NMI handler.  The compiler generates function entry and
  exit sequences suitable for use in an NMI handler when this
  attribute is present.

.. option:: saveall

  .. index:: saveall function attribute, Blackfin

  .. index:: save all registers on the Blackfin

  Use this attribute to indicate that
  all registers except the stack pointer should be saved in the prologue
  regardless of whether they are used or not.