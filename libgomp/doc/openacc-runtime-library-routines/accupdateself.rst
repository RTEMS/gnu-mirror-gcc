..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_update_self:

acc_update_self -- Update host memory from mapped device memory.
****************************************************************

Description
  This function updates the host copy from the previously mapped device memory.
  The host memory is specified with the host address :samp:`{a}` and a length of
  :samp:`{len}` bytes.

  In Fortran, two (2) forms are supported. In the first form, :samp:`{a}` specifies
  a contiguous array section. The second form :samp:`{a}` specifies a variable or
  array element and :samp:`{len}` specifies the length in bytes.

:samp:`{C/C++}:`

  ============  ============================================================
  *Prototype*:  ``acc_update_self(h_void *a, size_t len);``
  *Prototype*:  ``acc_update_self_async(h_void *a, size_t len, int async);``
  ============  ============================================================

:samp:`{Fortran}:`

  ============  ===================================================
  *Interface*:  ``subroutine acc_update_self(a)``
                ``type, dimension(:[,:]...) :: a``
  *Interface*:  ``subroutine acc_update_self(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
  *Interface*:  ``subroutine acc_update_self_async(a, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer(acc_handle_kind) :: async``
  *Interface*:  ``subroutine acc_update_self_async(a, len, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
                ``integer(acc_handle_kind) :: async``
  ============  ===================================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.25.