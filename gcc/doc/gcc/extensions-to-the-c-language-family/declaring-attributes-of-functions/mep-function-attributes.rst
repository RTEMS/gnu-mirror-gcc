..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _mep-function-attributes:

MeP Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the MeP back end:

.. index:: disinterrupt function attribute, MeP

.. gcc-attr:: disinterrupt

  On MeP targets, this attribute causes the compiler to emit
  instructions to disable interrupts for the duration of the given
  function.

.. index:: interrupt function attribute, MeP

.. gcc-attr:: interrupt

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

.. index:: near function attribute, MeP

.. gcc-attr:: near

  This attribute causes the compiler to assume the called
  function is close enough to use the normal calling convention,
  overriding the :option:`-mtf <MeP -mtf>` command-line option.

.. index:: far function attribute, MeP

.. gcc-attr:: far

  On MeP targets this causes the compiler to use a calling convention
  that assumes the called function is too far away for the built-in
  addressing modes.

.. index:: vliw function attribute, MeP

.. gcc-attr:: vliw

  The :gcc-attr:`vliw` attribute tells the compiler to emit
  instructions in VLIW mode instead of core mode.  Note that this
  attribute is not allowed unless a VLIW coprocessor has been configured
  and enabled through command-line options.