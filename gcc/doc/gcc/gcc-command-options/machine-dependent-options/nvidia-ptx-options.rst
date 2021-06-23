..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _nvidia-ptx-options:

Nvidia PTX Options
^^^^^^^^^^^^^^^^^^

.. index:: Nvidia PTX options

.. index:: nvptx options

These options are defined for Nvidia PTX:

.. option:: -m64

  Ignored, but preserved for backward compatibility.  Only 64-bit ABI is
  supported.

.. option:: -misa=ISA-string

  Generate code for given the specified PTX ISA (e.g. :samp:`sm_35`).  ISA
  strings must be lower-case.  Valid ISA strings include :samp:`sm_30` and
  :samp:`sm_35`.  The default ISA is sm_35.

.. option:: -mptx=version-string

  Generate code for given the specified PTX version (e.g. :samp:`6.3`).
  Valid version strings include :samp:`3.1` and :samp:`6.3`.  The default PTX
  version is 3.1.

.. option:: -mmainkernel

  Link in code for a __main kernel.  This is for stand-alone instead of
  offloading execution.

.. option:: -moptimize

  Apply partitioned execution optimizations.  This is the default when any
  level of optimization is selected.

.. option:: -msoft-stack

  Generate code that does not use ``.local`` memory
  directly for stack storage. Instead, a per-warp stack pointer is
  maintained explicitly. This enables variable-length stack allocation (with
  variable-length arrays or ``alloca`` ), and when global memory is used for
  underlying storage, makes it possible to access automatic variables from other
  threads, or with atomic instructions. This code generation variant is used
  for OpenMP offloading, but the option is exposed on its own for the purpose
  of testing the compiler; to generate code suitable for linking into programs
  using OpenMP offloading, use option :option:`-mgomp`.

.. option:: -muniform-simt

  Switch to code generation variant that allows to execute all threads in each
  warp, while maintaining memory state and side effects as if only one thread
  in each warp was active outside of OpenMP SIMD regions.  All atomic operations
  and calls to runtime (malloc, free, vprintf) are conditionally executed (iff
  current lane index equals the master lane index), and the register being
  assigned is copied via a shuffle instruction from the master lane.  Outside of
  SIMD regions lane 0 is the master; inside, each thread sees itself as the
  master.  Shared memory array ``int __nvptx_uni[]`` stores all-zeros or
  all-ones bitmasks for each warp, indicating current mode (0 outside of SIMD
  regions).  Each thread can bitwise-and the bitmask at position ``tid.y``
  with current lane index to compute the master lane index.

.. option:: -mgomp

  Generate code for use in OpenMP offloading: enables :option:`-msoft-stack` and
  :option:`-muniform-simt` options, and selects corresponding multilib variant.