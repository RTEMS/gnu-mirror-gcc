..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _vms-options:

VMS Options
^^^^^^^^^^^

These :samp:`-m` options are defined for the VMS implementations:

.. option:: -mvms-return-codes

  Return VMS condition codes from ``main``. The default is to return POSIX-style
  condition (e.g. error) codes.

.. option:: -mdebug-main=prefix

  .. index:: mdebug-main=prefix

  Flag the first routine whose name starts with :samp:`{prefix}` as the main
  routine for the debugger.

.. option:: -mmalloc64

  Default to 64-bit memory allocation routines.

.. option:: -mpointer-size=size

  .. index:: mpointer-size=size

  Set the default size of pointers. Possible options for :samp:`{size}` are
  :samp:`32` or :samp:`short` for 32 bit pointers, :samp:`64` or :samp:`long`
  for 64 bit pointers, and :samp:`no` for supporting only 32 bit pointers.
  The later option disables ``pragma pointer_size``.