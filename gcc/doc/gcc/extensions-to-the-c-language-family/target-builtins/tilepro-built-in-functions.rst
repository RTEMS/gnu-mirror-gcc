..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _tilepro-built-in-functions:

TILEPro Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides intrinsics to access every instruction of the TILEPro
processor.  The intrinsics are of the form:

.. code-block:: c++

  unsigned __insn_op (...)

where :samp:`{op}` is the name of the instruction.  Refer to the ISA manual
for the complete list of instructions.

GCC also provides intrinsics to directly access the network registers.
The intrinsics are:

.. code-block:: c++

  unsigned __tile_idn0_receive (void)
  unsigned __tile_idn1_receive (void)
  unsigned __tile_sn_receive (void)
  unsigned __tile_udn0_receive (void)
  unsigned __tile_udn1_receive (void)
  unsigned __tile_udn2_receive (void)
  unsigned __tile_udn3_receive (void)
  void __tile_idn_send (unsigned)
  void __tile_sn_send (unsigned)
  void __tile_udn_send (unsigned)

The intrinsic ``void __tile_network_barrier (void)`` is used to
guarantee that no network operations before it are reordered with
those after it.