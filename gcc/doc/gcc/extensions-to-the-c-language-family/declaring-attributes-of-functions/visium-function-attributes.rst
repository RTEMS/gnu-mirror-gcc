..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _visium-function-attributes:

Visium Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Visium back end:

.. option:: interrupt, interrupt_handler

  .. index:: interrupt function attribute, Visium

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.