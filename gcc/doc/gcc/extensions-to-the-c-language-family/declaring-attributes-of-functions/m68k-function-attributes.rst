..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _m68k-function-attributes:

m68k Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the m68k back end:

.. option:: interrupt

  .. index:: interrupt function attribute, m68k

  .. index:: interrupt_handler function attribute, m68k

  Use this attribute to
  indicate that the specified function is an interrupt handler.  The compiler
  generates function entry and exit sequences suitable for use in an
  interrupt handler when this attribute is present.  Either name may be used.

.. option:: interrupt_thread

  .. index:: interrupt_thread function attribute, fido

  Use this attribute on fido, a subarchitecture of the m68k, to indicate
  that the specified function is an interrupt handler that is designed
  to run as a thread.  The compiler omits generate prologue/epilogue
  sequences and replaces the return instruction with a ``sleep``
  instruction.  This attribute is available only on fido.