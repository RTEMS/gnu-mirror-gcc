..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. program:: TILEPro

.. index:: TILEPro options

.. _tilepro-options:

TILEPro Options
^^^^^^^^^^^^^^^

These :samp:`-m` options are supported on the TILEPro:

.. option:: -mcpu={name}

  Selects the type of CPU to be targeted.  Currently the only supported
  type is :samp:`tilepro`.

.. option:: -m32

  Generate code for a 32-bit environment, which sets int, long, and
  pointer to 32 bits.  This is the only supported behavior so the flag
  is essentially ignored.