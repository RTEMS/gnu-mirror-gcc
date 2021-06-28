..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _tile-gx-options:

TILE-Gx Options
^^^^^^^^^^^^^^^

.. index:: TILE-Gx options

These :samp:`-m` options are supported on the TILE-Gx:

.. option:: -mcmodel=small

  Generate code for the small model.  The distance for direct calls is
  limited to 500M in either direction.  PC-relative addresses are 32
  bits.  Absolute addresses support the full address range.

.. option:: -mcmodel=large

  Generate code for the large model.  There is no limitation on call
  distance, pc-relative addresses, or absolute addresses.

.. option:: -mcpu=name

  Selects the type of CPU to be targeted.  Currently the only supported
  type is :samp:`tilegx`.

.. option:: -m32, -m64

  Generate code for a 32-bit or 64-bit environment.  The 32-bit
  environment sets int, long, and pointer to 32 bits.  The 64-bit
  environment sets int to 32 bits and long and pointer to 64 bits.

.. option:: -mbig-endian, -mlittle-endian

  Generate code in big/little endian mode, respectively.