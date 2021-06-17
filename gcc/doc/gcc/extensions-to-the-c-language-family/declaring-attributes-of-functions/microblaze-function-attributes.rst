..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _microblaze-function-attributes:

MicroBlaze Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported on MicroBlaze targets:

.. option:: save_volatiles

  .. index:: save_volatiles function attribute, MicroBlaze

  Use this attribute to indicate that the function is
  an interrupt handler.  All volatile registers (in addition to non-volatile
  registers) are saved in the function prologue.  If the function is a leaf
  function, only volatiles used by the function are saved.  A normal function
  return is generated instead of a return from interrupt.

.. option:: break_handler

  .. index:: break_handler function attribute, MicroBlaze

  .. index:: break handler functions

  Use this attribute to indicate that
  the specified function is a break handler.  The compiler generates function
  entry and exit sequences suitable for use in an break handler when this
  attribute is present. The return from ``break_handler`` is done through
  the ``rtbd`` instead of ``rtsd``.

  .. code-block:: c++

    void f () __attribute__ ((break_handler));

.. option:: interrupt_handler

  .. index:: interrupt_handler function attribute, MicroBlaze

  .. index:: fast_interrupt function attribute, MicroBlaze

  These attributes indicate that the specified function is an interrupt
  handler.  Use the ``fast_interrupt`` attribute to indicate handlers
  used in low-latency interrupt mode, and ``interrupt_handler`` for
  interrupts that do not use low-latency handlers.  In both cases, GCC
  emits appropriate prologue code and generates a return from the handler
  using ``rtid`` instead of ``rtsd``.