..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _arm-floating-point-status-and-control-intrinsics:

ARM Floating Point Status and Control Intrinsics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the ARM family of
processors with floating-point unit.

.. code-block:: c++

  unsigned int __builtin_arm_get_fpscr ()
  void __builtin_arm_set_fpscr (unsigned int)