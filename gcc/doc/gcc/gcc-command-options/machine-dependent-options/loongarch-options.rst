..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. program:: LoongArch

.. _loongarch-options:

.. index:: LoongArch Options

LoongArch Options
^^^^^^^^^^^^^^^^^

These command-line options are defined for LoongArch targets:

.. option:: -march={cpu-type}

  Generate instructions for the machine type :samp:`{cpu-type}`.  In contrast to
  :option:`-mtune`:samp:`={cpu-type}`, which merely tunes the generated code
  for the specified :samp:`{cpu-type}`, :option:`-march`:samp:`={cpu-type}` allows GCC
  to generate code that may not run at all on processors other than the one
  indicated.  Specifying :option:`-march`:samp:`={cpu-type}` implies
  :option:`-mtune`:samp:`={cpu-type}`, except where noted otherwise.

  The choices for :samp:`{cpu-type}` are:

  :samp:`native`
    This selects the CPU to generate code for at compilation time by determining
    the processor type of the compiling machine.  Using :option:`-march`:samp:`=native`
    enables all instruction subsets supported by the local machine (hence
    the result might not run on different machines).  Using :option:`-mtune`:samp:`=native`
    produces code optimized for the local machine under the constraints
    of the selected instruction set.

  :samp:`loongarch64`
    A generic CPU with 64-bit extensions.

  :samp:`la464`
    LoongArch LA464 CPU with LBT, LSX, LASX, LVZ.

.. option:: -mtune={cpu-type}

  Optimize the output for the given processor, specified by microarchitecture
  name.

.. option:: -mabi={base-abi-type}

  Generate code for the specified calling convention.
  :samp:`{base-abi-type}` can be one of:

  :samp:`lp64d`
    Uses 64-bit general purpose registers and 32/64-bit floating-point
    registers for parameter passing.  Data model is LP64, where :samp:`int`
    is 32 bits, while :samp:`long int` and pointers are 64 bits.

  :samp:`lp64f`
    Uses 64-bit general purpose registers and 32-bit floating-point
    registers for parameter passing.  Data model is LP64, where :samp:`int`
    is 32 bits, while :samp:`long int` and pointers are 64 bits.

  :samp:`lp64s`
    Uses 64-bit general purpose registers and no floating-point
    registers for parameter passing.  Data model is LP64, where :samp:`int`
    is 32 bits, while :samp:`long int` and pointers are 64 bits.

.. option:: -mfpu={fpu-type}

  Generate code for the specified FPU type, which can be one of:

  :samp:`64`
    Allow the use of hardware floating-point instructions for 32-bit
    and 64-bit operations.

  :samp:`32`
    Allow the use of hardware floating-point instructions for 32-bit
    operations.

    :samp:`none`
  :samp:`0`
    Prevent the use of hardware floating-point instructions.

.. option:: -msoft-float

  Force :option:`-mfpu`:samp:`=none` and prevents the use of floating-point
  registers for parameter passing.  This option may change the target
  ABI.

.. option:: -msingle-float

  Force :option:`-mfpu`:samp:`=32` and allow the use of 32-bit floating-point
  registers for parameter passing.  This option may change the target
  ABI.

.. option:: -mdouble-float

  Force :option:`-mfpu`:samp:`=64` and allow the use of 32/64-bit floating-point
  registers for parameter passing.  This option may change the target
  ABI.

.. option:: -mbranch-cost={n}

  Set the cost of branches to roughly :samp:`{n}` instructions.

.. option:: -mcheck-zero-division, -mno-check-zero-divison

  Trap (do not trap) on integer division by zero.  The default is
  :option:`-mcheck-zero-division` for :option:`-O0` or :option:`-Og`, and
  :option:`-mno-check-zero-division` for other optimization levels.

.. option:: -mcond-move-int, -mno-cond-move-int

  Conditional moves for integral data in general-purpose registers
  are enabled (disabled).  The default is :option:`-mcond-move-int`.

.. option:: -mcond-move-float, -mno-cond-move-float

  Conditional moves for floating-point registers are enabled (disabled).
  The default is :option:`-mcond-move-float`.

.. option:: -mmemcpy, -mno-memcpy

  Force (do not force) the use of ``memcpy`` for non-trivial block moves.
  The default is :option:`-mno-memcpy`, which allows GCC to inline most
  constant-sized copies.  Setting optimization level to :option:`-Os` also
  forces the use of ``memcpy``, but :option:`-mno-memcpy` may override this
  behavior if explicitly specified, regardless of the order these options on
  the command line.

.. option:: -mstrict-align, -mno-strict-align

  Avoid or allow generating memory accesses that may not be aligned on a natural
  object boundary as described in the architecture specification. The default is
  :option:`-mno-strict-align`.

.. option:: -msmall-data-limit={number}

  Put global and static data smaller than :samp:`{number}` bytes into a special
  section (on some targets).  The default value is 0.

.. option:: -mmax-inline-memcpy-size={n}

  Inline all block moves (such as calls to ``memcpy`` or structure copies)
  less than or equal to :samp:`{n}` bytes.  The default value of :samp:`{n}` is 1024.

.. option:: -mcmodel={code-model}

  Set the code model to one of:

  :samp:`tiny-static`
    * local symbol and global strong symbol: The data section must be within +/-2MiB addressing space.
      The text section must be within +/-128MiB addressing space.

    * global weak symbol: The got table must be within +/-2GiB addressing space.

  :samp:`tiny`
    * local symbol: The data section must be within +/-2MiB addressing space.
      The text section must be within +/-128MiB
      addressing space.

    * global symbol: The got table must be within +/-2GiB addressing space.

  :samp:`normal`
    * local symbol: The data section must be within +/-2GiB addressing space.
      The text section must be within +/-128MiB addressing space.

    * global symbol: The got table must be within +/-2GiB addressing space.

  :samp:`large`
    * local symbol: The data section must be within +/-2GiB addressing space.
      The text section must be within +/-128GiB addressing space.

    * global symbol: The got table must be within +/-2GiB addressing space.

  :samp:`extreme` (Not implemented yet)
    * local symbol: The data and text section must be within +/-8EiB addressing space.

    * global symbol: The data got table must be within +/-8EiB addressing space.

  The default code model is ``normal``.

.. option:: -mexplicit-relocs, -mno-explicit-relocs

  Use or do not use assembler relocation operators when dealing with symbolic
  addresses.  The alternative is to use assembler macros instead, which may
  limit optimization.  The default value for the option is determined during
  GCC build-time by detecting corresponding assembler support:
  ``-mexplicit-relocs`` if said support is present,
  ``-mno-explicit-relocs`` otherwise.  This option is mostly useful for
  debugging, or interoperation with assemblers different from the build-time
  one.
