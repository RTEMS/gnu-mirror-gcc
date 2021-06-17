..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _mep-function-attributes:

MeP Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the MeP back end:

.. option:: disinterrupt

  .. index:: disinterrupt function attribute, MeP

  On MeP targets, this attribute causes the compiler to emit
  instructions to disable interrupts for the duration of the given
  function.

.. option:: interrupt

  .. index:: interrupt function attribute, MeP

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

.. option:: near

  .. index:: near function attribute, MeP

  This attribute causes the compiler to assume the called
  function is close enough to use the normal calling convention,
  overriding the :option:`-mtf` command-line option.

.. option:: far

  .. index:: far function attribute, MeP

  On MeP targets this causes the compiler to use a calling convention
  that assumes the called function is too far away for the built-in
  addressing modes.

.. option:: vliw

  .. index:: vliw function attribute, MeP

  The ``vliw`` attribute tells the compiler to emit
  instructions in VLIW mode instead of core mode.  Note that this
  attribute is not allowed unless a VLIW coprocessor has been configured
  and enabled through command-line options.