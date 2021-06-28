..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_create:

acc_create -- Allocate device memory and map it to host memory.
***************************************************************

Description
  This function allocates device memory and maps it to host memory specified
  by the host address :samp:`{a}` with a length of :samp:`{len}` bytes. In C/C++,
  the function returns the device address of the allocated device memory.

  In Fortran, two (2) forms are supported. In the first form, :samp:`{a}` specifies
  a contiguous array section. The second form :samp:`{a}` specifies a variable or
  array element and :samp:`{len}` specifies the length in bytes.

:samp:`{C/C++}:`

  ============  =============================================================
  *Prototype*:  ``void *acc_create(h_void *a, size_t len);``
  *Prototype*:  ``void *acc_create_async(h_void *a, size_t len, int async);``
  ============  =============================================================

:samp:`{Fortran}:`

  ============  ==============================================
  *Interface*:  ``subroutine acc_create(a)``
                ``type, dimension(:[,:]...) :: a``
  *Interface*:  ``subroutine acc_create(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
  *Interface*:  ``subroutine acc_create_async(a, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer(acc_handle_kind) :: async``
  *Interface*:  ``subroutine acc_create_async(a, len, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
                ``integer(acc_handle_kind) :: async``
  ============  ==============================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.21.