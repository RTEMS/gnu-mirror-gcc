..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _visium-function-attributes:

Visium Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Visium back end:

.. index:: interrupt function attribute, Visium

.. gcc-attr:: interrupt, interrupt_handler

.. gcc-attr:: interrupt

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.