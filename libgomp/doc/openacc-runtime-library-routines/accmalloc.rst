..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_malloc:

acc_malloc -- Allocate device memory.
*************************************

Description
  This function allocates :samp:`{len}` bytes of device memory. It returns
  the device address of the allocated memory.

:samp:`{C/C++}:`

  ============  ===================================
  *Prototype*:  ``d_void* acc_malloc(size_t len);``
  ============  ===================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.18.